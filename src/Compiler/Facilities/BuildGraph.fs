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

type ComputationState<'T> =
    | Initial of Async<'T>
    | Started of Task<'T> * (unit -> unit)
    | Completed of 'T

[<Sealed>]
type GraphNode<'T> private (initialState: ComputationState<'T>) =
    let mutable requestCount = 0
    // Latches to ensure we are running the computation at most once at a time.
    let gate = obj()
    let withLock f = lock gate f
    // State is mutated only after entering the latch.
    let mutable state = initialState

    let startOrContinueCompute() =
        match state with
        | Initial computation ->
            let tcs = TaskCompletionSource()
            let cts = new CancellationTokenSource()

            let reset () =
                match state with
                | Started _ -> 
                    state <- Initial computation
                    cts.Cancel()
                | _ -> ()

            state <- Started(tcs.Task, reset)

            fun () -> Async.StartWithContinuations(
                computation,
                (fun result -> withLock(fun () -> state <- Completed result); tcs.SetResult result),
                (tcs.SetException),
                (fun _ -> tcs.SetCanceled()),
                cts.Token)
        | _ -> ignore

    member _.GetOrComputeValue() =
        let startOrContinue = withLock startOrContinueCompute

        startOrContinue()

        match state with
        | Completed result -> async { return result }
        | Started (compute, reset) ->
            async {
                Interlocked.Increment &requestCount |> ignore
                try
                    let! ct = Async.CancellationToken
                    return! taskWaitAsync compute ct |> Async.AwaitTask 
                finally
                    // The computation didn't complete but no requestor awaits the result.
                    if Interlocked.Decrement &requestCount = 0 then withLock reset
            }
        | _ -> failwith "GraphNode.GetOrComputeValue invalid state: computation not started"

    new(computation) = GraphNode(Initial computation)

    member _.TryPeekValue() =
        match state with Completed v -> ValueSome v | _ -> ValueNone

    member _.HasValue =
        match state with Completed _ -> true | _ -> false

    member _.IsComputing = match state with Started _ -> true | _ -> false

    static member FromResult(result: 'T) = Completed result |> GraphNode