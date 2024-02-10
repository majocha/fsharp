module Internal.Utilities.Collections.AsyncMemoize

open System
open System.Diagnostics
open System.IO
open System.Threading
open System.Threading.Tasks

open FSharp.Compiler.BuildGraph
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.DiagnosticsLogger
open System.Runtime.CompilerServices
open Internal.Utilities.Collections

// Poor man's Task.WaitAsync(ct), since we don't have the real thing.
let taskWaitAsync (attachedTask: Task<'T>) (cancellationToken: CancellationToken) =
    backgroundTask { 
        let detach = TaskCompletionSource<_>()
        cancellationToken.Register(fun () -> detach.SetCanceled()) |> ignore
        let! resultAsTask = Task.WhenAny<'T>(attachedTask, detach.Task)
        return! resultAsTask
    }

/// Return file name with one directory above it
let shortPath path =
    let dirPath = Path.GetDirectoryName path

    let dir =
        dirPath.Split Path.DirectorySeparatorChar
        |> Array.tryLast
        |> Option.map (sprintf "%s/")
        |> Option.defaultValue ""

    $"{dir}{Path.GetFileName path}"

let (|TaskCancelled|_|) (ex: exn) =
    match ex with
    | :? System.Threading.Tasks.TaskCanceledException as tce -> Some tce
    //| :? System.AggregateException as ae ->
    //    if ae.InnerExceptions |> Seq.forall (fun e -> e :? System.Threading.Tasks.TaskCanceledException) then
    //        ae.InnerExceptions |> Seq.tryHead |> Option.map (fun e -> e :?> System.Threading.Tasks.TaskCanceledException)
    //    else
    //        None
    | _ -> None

type internal JobEvent =
    | Requested
    | Started
    //| Restarted
    | Finished
    | Canceled
    | Evicted
    | Collected
    | Weakened
    | Strengthened
    | Failed
    | Cleared

let withLock (semaphore: SemaphoreSlim) f = async {
    do! semaphore.WaitAsync() |> Async.AwaitTask
    try
        return! f
    finally
        semaphore.Release() |> ignore       
}

type CompilerTaskStatus<'T> =
    | TaskInactive
    | TaskRunning of DateTime
    | TaskCompleted of 'T * DateTime
    | TaskCanceled of DateTime
    | TaskFailed of DateTime * exn

[<DebuggerDisplay("{DebuggerDisplay}")>]
type AsyncLazyCompilerTask<'T>(computation, log) =
    let mutable status = TaskInactive
    let mutable requestCount = 0

    let semaphore = new SemaphoreSlim(1, 1)

    // let gate = obj()
    // let withLock f = lock gate f
    let tcs = TaskCompletionSource<'T>()
    let cts = new CancellationTokenSource()
    let logger = CapturingDiagnosticsLogger("AsyncLazyCompilerTask")

    let startOrContinue () =
        match status with
        | TaskInactive ->
            status <- TaskRunning DateTime.Now

            fun () ->
                log Started
                Async.StartWithContinuations(
                    async {
                        use _ = UseDiagnosticsLogger logger
                        return! computation 
                    },
                    (fun result ->
                        status <- TaskCompleted (result, DateTime.Now)
                        log Finished
                        tcs.SetResult result),
                    (fun ex ->
                        status <- TaskFailed(DateTime.Now, ex)
                        log Failed
                        tcs.SetException ex),
                    (fun _ ->
                        log Canceled
                        tcs.SetCanceled()),
                    cts.Token)
        | _ -> ignore

    member _.GetOrComputeValue() =
        log Requested

        let callerDiagnosticLogger =
            if DiagnosticsAsyncState.DiagnosticsLogger = UninitializedDiagnosticsLogger then
                // TODO: Telemetry?
                DiagnosticsAsyncState.DiagnosticsLogger <- DiscardErrorsLogger
            DiagnosticsAsyncState.DiagnosticsLogger

        async {
            let! startOrContinue = async { return startOrContinue() } |> withLock semaphore
            let! ct = Async.CancellationToken
            Interlocked.Increment &requestCount |> ignore
            try
                startOrContinue()
                return! taskWaitAsync tcs.Task ct |> Async.AwaitTask
            finally
                if not ct.IsCancellationRequested then
                    logger.CommitDelayedDiagnostics callerDiagnosticLogger

                async {
                    match status, Interlocked.Decrement &requestCount with
                    | TaskRunning _, 0 when ct.IsCancellationRequested ->
                        status <- TaskCanceled DateTime.Now
                        cts.Cancel()
                    | TaskRunning _, 0 -> failwithf $"Finished with invalid state {tcs.Task.Status}"
                    | _ -> ()
                } |> withLock semaphore |> Async.StartImmediate
        } |> Async.CompilationScope

    member _.Cancel() = cts.Cancel()

    member _.Status = status

    member _.CanceledOrFailed = match status with TaskCanceled _ | TaskFailed _ -> true | _ -> false

    member _.DebuggerDisplay =
        match status with
        | TaskInactive -> "Inactive"
        | TaskRunning ts ->
            let cancellation =
                if cts.IsCancellationRequested then
                    " ! Cancellation Requested"
                else
                    ""

            $"Running since {ts.ToShortTimeString()}{cancellation}"
        | TaskCompleted (value, _) -> $"Completed {value}" + (if logger.Diagnostics.Length > 0 then $" ({logger.Diagnostics.Length})" else "")
        | TaskCanceled _ -> "Canceled"
        | TaskFailed(_, ex) -> $"Failed {ex}"

let (|TaskCanceledOrFailed|_|) (t: AsyncLazyCompilerTask<_>) = match t.Status with TaskCanceled _ | TaskFailed _ -> Some t | _ -> None

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

[<DebuggerDisplay("{DebuggerDisplay}")>]
type internal AsyncMemoize<'TKey, 'TVersion, 'TValue when 'TKey: equality and 'TVersion: equality>
    (?keepStrongly, ?keepWeakly, ?name: string, ?cancelDuplicateRunningJobs: bool) =

    let name = defaultArg name "N/A"
    let cancelDuplicateRunningJobs = defaultArg cancelDuplicateRunningJobs false

    let event = Event<_>()

    let mutable errors = 0
    let mutable hits = 0
    let mutable started = 0
    let mutable completed = 0
    let mutable canceled = 0
    let mutable restarted = 0
    let mutable failed = 0
    let mutable evicted = 0
    let mutable collected = 0
    let mutable strengthened = 0
    let mutable cleared = 0

    let mutable avgDurationMs = 0.0

    let cache =
        LruCache<'TKey, 'TVersion, AsyncLazyCompilerTask<'TValue>>(
            keepStrongly = defaultArg keepStrongly 100,
            keepWeakly = defaultArg keepWeakly 200,
            requiredToKeep = (fun t ->
                match t.Status with
                | TaskRunning _ -> true
                | TaskCanceled at when at > DateTime.Now.AddMinutes -5.0 -> true
                | TaskFailed (at, _) when at > DateTime.Now.AddMinutes -5.0 -> true
                | _ -> false),
            event =
                (function
                | CacheEvent.Evicted ->
                    (fun k ->
                        Interlocked.Increment &evicted |> ignore
                        event.Trigger(JobEvent.Evicted, k))
                | CacheEvent.Collected ->
                    (fun k ->
                        Interlocked.Increment &collected |> ignore
                        event.Trigger(JobEvent.Collected, k))
                | CacheEvent.Weakened -> (fun k -> event.Trigger(JobEvent.Weakened, k))
                | CacheEvent.Strengthened ->
                    (fun k ->
                        Interlocked.Increment &strengthened |> ignore
                        event.Trigger(JobEvent.Strengthened, k))
                | CacheEvent.Cleared ->
                    (fun k ->
                        Interlocked.Increment &cleared |> ignore
                        event.Trigger(JobEvent.Cleared, k)))
        )

    let log (keyData: KeyData<_, _>) eventType =
        event.Trigger(eventType, (keyData.Label, keyData.Key, keyData.Version))

    let semaphore = new SemaphoreSlim(1, 1)

    let getTask key computation =
        async {
            let cached, otherVersions = cache.GetAll(key.Key, key.Version)

            if cancelDuplicateRunningJobs then
                for _, t in otherVersions do
                    use _ = Activity.start $"{name}: Duplicate running job" [| "key", key.Label |]
                    t.Cancel()

            match cached with
            | None
            | Some (TaskCanceledOrFailed _) ->
                let newTask = AsyncLazyCompilerTask(computation, log key)
                cache.Set(key.Key, key.Version, newTask)
                return newTask
            | Some existingTask ->
                return existingTask
        } |> withLock semaphore

    member this.Get'(key, computation) =

        let wrappedKey =
            { new ICacheKey<_, _> with
                member _.GetKey() = key
                member _.GetVersion() = Unchecked.defaultof<_>
                member _.GetLabel() = key.ToString()
            }

        this.Get(wrappedKey, computation)

    member _.Get(key: ICacheKey<_, _>, computation) =

        let key =
            {
                Label = key.GetLabel()
                Key = key.GetKey()
                Version = key.GetVersion()
            }

        async {
            let! compilerTask = getTask key computation
            return! compilerTask.GetOrComputeValue()
        }

    member _.Clear() = cache.Clear()

    member _.Clear predicate = cache.Clear predicate

    member val Event = event.Publish

    member this.OnEvent = this.Event.Add

    member _.Running =
        cache.GetValues()
        |> Seq.map (fun (_, _, t) -> t.Status)
        |> Seq.filter (function
            | TaskRunning _ -> true
            | _ -> false)
        |> Seq.toArray

    member this.DebuggerDisplay =
        let locked = "" // if this.Locked then " [LOCKED]" else ""

        let valueStats =
            cache.GetValues()
            |> Seq.map (fun (_, _, t) -> t.Status)
            |> Seq.countBy (function
                | TaskInactive -> "Inactive"
                | TaskRunning _ -> "Running"
                | TaskCompleted _ -> "Completed"
                | TaskCanceled _ -> "Canceled"
                | TaskFailed _ -> "Failed")
            |> Map

        let running =
            valueStats.TryFind "Running"
            |> Option.map (sprintf " Running: %d ")
            |> Option.defaultValue ""

        let avgDuration = avgDurationMs |> sprintf "| Avg: %.0f ms"

        let hitRatio =
            if started > 0 then
                $" (%.0f{float hits / (float (started + hits)) * 100.0} %%)"
            else
                ""

        let stats =
            [|
                if errors + failed > 0 then
                    " (_!_) "
                if errors > 0 then $"| ERRORS: {errors} " else ""
                if failed > 0 then $"| FAILED: {failed} " else ""
                $"| hits: {hits}{hitRatio} "
                if started > 0 then $"| started: {started} " else ""
                if completed > 0 then $"| completed: {completed} " else ""
                if canceled > 0 then $"| canceled: {canceled} " else ""
                if restarted > 0 then $"| restarted: {restarted} " else ""
                if evicted > 0 then $"| evicted: {evicted} " else ""
                if collected > 0 then $"| collected: {collected} " else ""
                if cleared > 0 then $"| cleared: {cleared} " else ""
                if strengthened > 0 then
                    $"| strengthened: {strengthened} "
                else
                    ""
            |]
            |> String.concat ""

        $"{locked}{running}{cache.DebuggerDisplay} {stats}{avgDuration}"

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
