// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

module FSharp.Compiler.BuildGraph

open System
open System.Threading
open System.Threading.Tasks
open Internal.Utilities.Library
open FSharp.Compiler.DiagnosticsLogger

[<AbstractClass; Sealed>]
type Async =
    static member CompilationScope(computation: Async<'T>) =
        async {
            use _ =
                new CompilationGlobalsScope(DiagnosticsAsyncState.DiagnosticsLogger, DiagnosticsAsyncState.BuildPhase)

            return! computation
        }

    static member RunImmediateWithoutCancellation(computation) =
        try
            Async
                .StartImmediateAsTask(computation |> Async.CompilationScope, cancellationToken = CancellationToken.None)
                .Result

        with :? AggregateException as ex when ex.InnerExceptions.Count = 1 ->
            raise (ex.InnerExceptions[0])

    static member FromCancellableWithScope(computation: Cancellable<'T>) =
        computation |> Cancellable.toAsync |> Async.CompilationScope

    static member StartAsTask_ForTesting(computation: Async<'T>, ?ct: CancellationToken) =
        Async.StartAsTask(computation |> Async.CompilationScope, cancellationToken = defaultArg ct CancellationToken.None)

    static member SequentialImmediate(computations: Async<'T> seq) =
        async {
            let results = ResizeArray()

            for computation in computations do
                let! result = computation
                results.Add result

            return results.ToArray()
        }
type ComputationState<'T> =
    | Initial of Async<'T>
    | Started of TaskCompletionSource<'T> * (unit -> unit)
    | Completed of 'T

[<Sealed>]
type GraphNode<'T> private (initialState: ComputationState<'T>) =

    // Any locking we do is for very short synchronous state updates.
    let gate = obj()
    let withLock f = lock gate f
    let mutable requestCount = 0
    let mutable state = initialState

    let startCompute computation =
        let tcs = TaskCompletionSource()
        let cts = new CancellationTokenSource()

        let primeForRestart = fun () ->
            match state with
            | Started _ -> cts.Cancel(); state <- Initial computation
            | _ -> ()

        state <- Started(tcs, primeForRestart)

        let start () = 
            Async.StartWithContinuations(
                computation,
                (fun result -> state <- Completed result; tcs.SetResult result),
                (fun ex -> withLock primeForRestart; tcs.SetException(ex)),
                (fun _ -> tcs.SetCanceled()),
                cts.Token)

        tcs.Task, start, primeForRestart

    let cancelIfLastRequestCancels onCancel =
        withLock (fun () -> if requestCount = 1 then onCancel())

    member _.GetOrComputeValue() =
        let awaitResult (tcsTask, start, primeForRestart) =
            start()
            async {
                Interlocked.Increment &requestCount |> ignore
                use! __ = Async.OnCancel (fun () -> cancelIfLastRequestCancels primeForRestart)
                try
                    return! tcsTask |> Async.AwaitTask
                finally
                    if Interlocked.Decrement &requestCount = 0 then primeForRestart()
            }

        withLock( fun () ->
            match state with
            | Completed result -> Task.FromResult result, ignore, ignore
            | Initial computation -> startCompute computation
            | Started (tcs, primeForRestart) -> tcs.Task, ignore, primeForRestart)
        |> awaitResult

    new(computation) = GraphNode(Initial computation)

    member _.TryPeekValue() =
        match state with Completed v -> ValueSome v | _ -> ValueNone

    member _.HasValue =
        match state with Completed _ -> true | _ -> false

    member _.IsComputing = requestCount > 0

    static member FromResult(result: 'T) = Completed result |> GraphNode