// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

module FSharp.Compiler.BuildGraph

open System
open System.Threading
open System.Threading.Tasks
open Internal.Utilities.Library
open FSharp.Compiler.DiagnosticsLogger
open System

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

[<CustomEquality; NoComparison>]
type ComputationState<'T> =
    | Initial of Async<'T>
    | Running of Task<'T> * (ComputationState<'T> -> unit)
    | Completed of 'T
    override this.Equals(that) = Object.ReferenceEquals(this, that)
    override this.GetHashCode() = this.GetHashCode()

[<Sealed>]
type GraphNode<'T> private (initialState) =
    let mutable requestCount = 0
    let mutable state: ComputationState<'T> = initialState
    let tryUpdateState s expected =
        Interlocked.CompareExchange(&state, s, expected) = expected

    let startOnce = new SemaphoreSlim(1, 1)

    let rec startOrContinue() = async {
        let! ct = Async.CancellationToken
        let! enter = startOnce.WaitAsync(10_000, ct) |> Async.AwaitTask 
        if not enter then return failwith "GraphNode: startOnce timeout."

        match state with
        | Initial computation ->
            let tcs = TaskCompletionSource<'T>()
            let cts = new CancellationTokenSource()

            let reset runningState =
                if 
                    tryUpdateState (Initial computation) runningState
                then 
                    cts.Cancel()

            state <- Running(tcs.Task, reset)
            let currentState = state
            
            startOnce.Release() |> ignore

            Async.StartWithContinuations(
                computation,
                (fun result ->
                    state <- Completed result
                    tcs.SetResult result),
                (fun ex -> 
                    tryUpdateState (Initial computation) currentState |> ignore
                    tcs.SetException ex),
                (fun _ -> tcs.SetCanceled()),
                cts.Token)

            return currentState
        | state ->
            startOnce.Release() |> ignore
            return state
    }

    member this.GetOrComputeValue() =
        async {
            let! state = startOrContinue()
            match state with
            | Completed result -> return result
            | Running(compute, reset) ->
                Interlocked.Increment &requestCount |> ignore

                let! ct = Async.CancellationToken
                try 
                    return! taskWaitAsync compute ct |> Async.AwaitTask              
                finally
                    if Interlocked.Decrement &requestCount = 0 then reset state   
            | _ -> return failwith "invalid state in GraphNode."
        }

    new(computation) = GraphNode(Initial computation)

    member _.TryPeekValue() =
        match state with Completed result -> ValueSome result  | _ -> ValueNone

    member _.HasValue =
        match state with Completed _ -> true | _ -> false

    member _.IsComputing = requestCount > 0

    static member FromResult(result: 'T) = Completed result |> GraphNode