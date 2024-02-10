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

// Poor man's Task.WaitAsync(ct), since we don't have the real thing.
let taskWaitAsync (attachedTask: Task<'T>) (cancellationToken: CancellationToken) =
    backgroundTask { 
        let detach = TaskCompletionSource<_>()
        cancellationToken.Register(fun () -> detach.SetCanceled()) |> ignore
        let! resultAsTask = Task.WhenAny<'T>(attachedTask, detach.Task)
        return! resultAsTask
    }

let withLock (semaphore: SemaphoreSlim) f = async {
    do! semaphore.WaitAsync() |> Async.AwaitTask
    try
        return! f
    finally
        semaphore.Release() |> ignore       
}

type ComputationState<'T> =
    | Initial of Async<'T>
    | Started of Task<'T> * (unit -> unit)
    | Completed of 'T

[<Sealed>]
type GraphNode<'T> private (initialState: ComputationState<'T>) =
    let mutable requestCount = 0
    let gate = new SemaphoreSlim(1, 1)
    let mutable state = initialState

    member _.GetOrComputeValue() =
        async {
            let! start, compute =
                async {
                    match state with
                    | Initial computation ->
                        let tcs = TaskCompletionSource<'T>()
                        let cts = new CancellationTokenSource()

                        let reset () =
                            state <- Initial computation
                            cts.Cancel()

                        state <- Started(tcs.Task, reset)

                        let start () = 
                            Async.StartWithContinuations(
                                computation,
                                (fun result -> async { state <- Completed result } |> withLock gate |> Async.RunSynchronously; tcs.SetResult result),
                                (tcs.SetException),
                                (fun _ -> tcs.SetCanceled()),
                                cts.Token)

                        return start, tcs.Task

                    | Completed result -> return ignore, Task.FromResult result
                    | Started (compute, _) -> return ignore, compute
                } |> withLock gate

            start()

            Interlocked.Increment &requestCount |> ignore
            try
                let! ct = Async.CancellationToken
                return! taskWaitAsync compute ct |> Async.AwaitTask 
            finally
                async {
                    match state, Interlocked.Decrement &requestCount with
                    | Started (_, reset), 0 -> reset()
                    | _ -> ()
                } |> withLock gate |> Async.RunSynchronously
        }

    new(computation) = GraphNode(Initial computation)

    member _.TryPeekValue() =
        match state with Completed v -> ValueSome v | _ -> ValueNone

    member _.HasValue =
        match state with Completed _ -> true | _ -> false

    member _.IsComputing = match state with Started _ -> true | _ -> false

    static member FromResult(result: 'T) = Completed result |> GraphNode