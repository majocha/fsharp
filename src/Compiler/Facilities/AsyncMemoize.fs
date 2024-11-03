namespace Internal.Utilities.Collections

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Threading
open System.Threading.Tasks

open FSharp.Compiler
open FSharp.Compiler.BuildGraph
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.DiagnosticsLogger
open Internal.Utilities.Library
open System.Runtime.CompilerServices

type AsyncLazyState<'t> =
    | Initial of Async<'t>
    | Requested of Lazy<Task<'t>>  * CancellationTokenSource * int
    | Canceled

type AsyncLazy<'t>(computation: Async<'t>, ?cancelUnobserved: bool, ?restartCanceled: bool) =

    let restartCanceled = defaultArg restartCanceled true
    let cancelUnobserved = defaultArg cancelUnobserved true

    let stateUpdateSync = obj()
    let mutable state = Initial computation

    let afterRequest () =
        match state with
        | Requested(work, cts, 1) when not work.Value.IsCompleted && cancelUnobserved ->
            cts.Cancel()
            state <- if restartCanceled then Initial computation else Canceled
        | Requested(work, cts, count) ->
            state <- Requested(work, cts, count - 1)
        | _ -> ()

    let request (work: Lazy<Task<'t>>) firstRequest =
        async {
            try
                let! ct = Async.CancellationToken
                let options = if firstRequest then TaskContinuationOptions.ExecuteSynchronously else TaskContinuationOptions.None
                return!
                    work.Value.ContinueWith((fun (t: Task<_>) -> t.Result), ct, options, TaskScheduler.Current)
                    |> Async.AwaitTask
            finally
                lock stateUpdateSync afterRequest
        }

    let tryRequest () =
        match state with
        | Initial computation ->
            let cts = new CancellationTokenSource()
            let work = lazy Async.StartAsTask(computation, cancellationToken = cts.Token)
            state <- Requested (work, cts, 1)
            Some (request work true)
        | Requested (work, cts, count) ->
            state <- Requested (work, cts, count + 1)
            Some (request work (count = 0))
        | Canceled ->
            None

    member _.TryRequest =
        lock stateUpdateSync tryRequest

    member _.Result =
        match state with
        | Requested (work, _, _) when work.IsValueCreated && work.Value.Status = TaskStatus.RanToCompletion ->
            Some work.Value.Result
        | _ ->
            None

[<AutoOpen>]
module internal Utils =

    /// Return file name with one directory above it
    let shortPath (path: string) =
        let dirPath = !! Path.GetDirectoryName(path)

        let dir =
            dirPath.Split Path.DirectorySeparatorChar
            |> Array.tryLast
            |> Option.map (sprintf "%s/")
            |> Option.defaultValue ""

        $"{dir}{Path.GetFileName path}"

type internal JobEvent =
    | Requested
    | Started
    | Restarted
    | Finished
    | Canceled
    | Evicted
    | Collected
    | Weakened
    | Strengthened
    | Failed
    | Cleared

type internal ICacheKey<'TKey, 'TVersion> =
    abstract member GetKey: unit -> 'TKey
    abstract member GetVersion: unit -> 'TVersion
    abstract member GetLabel: unit -> string

[<Extension>]
type Extensions =

    [<Extension>]
    static member internal WithExtraVersion(cacheKey: ICacheKey<_, _>, extraVersion) =
        { new ICacheKey<_, _> with
            member _.GetLabel() = cacheKey.GetLabel()
            member _.GetKey() = cacheKey.GetKey()
            member _.GetVersion() = cacheKey.GetVersion(), extraVersion
        }

type private KeyData<'TKey, 'TVersion> =
    {
        Label: string
        Key: 'TKey
        Version: 'TVersion
    }

type internal AsyncMemoize<'TKey, 'TVersion, 'TValue when 'TKey: equality and 'TVersion: equality
#if !NO_CHECKNULLS
    and 'TKey:not null
    and 'TVersion:not null
#endif
    >
    (?keepStrongly, ?keepWeakly, ?name: string, ?cancelDuplicateRunningJobs: bool) =

    let name = defaultArg name "N/A"
    do ignore cancelDuplicateRunningJobs

    let event = Event<_>()

    let log (eventType, keyData: KeyData<_, _>) = lock event <| fun () ->
        event.Trigger(eventType, (keyData.Label, keyData.Key, keyData.Version))

    let cache =
        LruCache<'TKey, 'TVersion, AsyncLazy<Result<'TValue * _, exn * _>>>(
            keepStrongly = defaultArg keepStrongly 100,
            keepWeakly = defaultArg keepWeakly 200,
            event =
                (function
                | CacheEvent.Evicted -> (fun k -> event.Trigger(JobEvent.Evicted, k))
                | CacheEvent.Collected -> (fun k -> event.Trigger(JobEvent.Collected, k))
                | CacheEvent.Weakened -> (fun k -> event.Trigger(JobEvent.Weakened, k))
                | CacheEvent.Strengthened -> (fun k -> event.Trigger(JobEvent.Strengthened, k))
                | CacheEvent.Cleared -> (fun k -> event.Trigger(JobEvent.Cleared, k))))

    member _.Get(key: ICacheKey<_, _>, computation) =
        let key =
            {
                Label = key.GetLabel()
                Key = key.GetKey()
                Version = key.GetVersion()
            }
      
        log (Requested, key)

        let startNew () =
            let job =
                AsyncLazy( async {
                    use! _handler = Async.OnCancel <| fun () -> log (Canceled, key)
                    log (Started, key)
                    let logger = CapturingDiagnosticsLogger "cache"
                    SetThreadDiagnosticsLoggerNoUnwind logger
                    match! Async.Catch computation with
                    | Choice.Choice1Of2 result ->
                        log(Finished, key)
                        return Result.Ok(result, logger)
                    | Choice.Choice2Of2 ex ->
                        log (Failed, key)
                        return Result.Error(ex, logger)
                })
            cache.Set(key.Key, key.Version, key.Label, job)
            job.TryRequest |> Option.get

        let request = lock cache <| fun () ->
            let cached, _ = cache.GetAll(key.Key, key.Version)
            cached |> Option.bind _.TryRequest |> Option.defaultWith startNew

        async {
            use _ = new CompilationGlobalsScope()
            match! request with
            | Result.Ok(result, logger) ->
                logger.CommitDelayedDiagnostics DiagnosticsThreadStatics.DiagnosticsLogger
                return result
            | Result.Error(ex, logger) ->
                logger.CommitDelayedDiagnostics DiagnosticsThreadStatics.DiagnosticsLogger
                return raise ex
        }

    member _.TryGet(key: 'TKey, predicate: 'TVersion -> bool) : 'TValue option =
        let versionsAndJobs = cache.GetAll(key)

        versionsAndJobs
        |> Seq.tryPick (fun (version, job) ->
            match predicate version, job.Result with
            | true, Some(Result.Ok(result, _)) -> Some result
            | _ -> None)

    member _.Clear() = cache.Clear()

    member _.Clear predicate = cache.Clear predicate

    member val Event = event.Publish

    member this.OnEvent = this.Event.Add

    member this.Count = lock cache <| fun () -> cache.Count

/// A drop-in replacement for AsyncMemoize that disables caching and just runs the computation every time.
[<DebuggerDisplay("{DebuggerDisplay}")>]
type internal AsyncMemoizeDisabled<'TKey, 'TVersion, 'TValue when 'TKey: equality and 'TVersion: equality>
    (?keepStrongly, ?keepWeakly, ?name: string, ?cancelDuplicateRunningJobs: bool) =

    do ignore (keepStrongly, keepWeakly, name, cancelDuplicateRunningJobs)

    let mutable requests = 0

    member _.Get(_key: ICacheKey<_, _>, computation) =
        Interlocked.Increment &requests |> ignore
        computation

    member _.DebuggerDisplay = $"(disabled) requests: {requests}"
