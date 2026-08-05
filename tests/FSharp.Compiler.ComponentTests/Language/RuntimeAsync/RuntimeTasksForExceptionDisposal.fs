module RuntimeTasksForExceptionDisposal

open System
open System.Collections
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Threading.Tasks
open Microsoft.FSharp.Control
open Microsoft.FSharp.Core.CompilerServices

let private sequenceTasks (first: Task<unit>) (second: unit -> Task<'T>) =
    StateMachineHelpers.__runtimeAsync<Task<'T>> (fun () ->
        AsyncHelpers.Await first
        AsyncHelpers.Await(second()))

type RuntimeTaskBuilder() =
    member _.Delay(generator: unit -> Task<'T>) = generator

    member _.Run(generator: unit -> Task<'T>) : Task<'T> =
        StateMachineHelpers.__runtimeAsync<Task<'T>> (fun () ->
            AsyncHelpers.Await(generator()))

    member _.Zero() : Task<unit> = Task.FromResult()

    member _.Return(value: 'T) : Task<'T> = Task.FromResult value

    member _.Bind(task: Task, continuation: unit -> Task<'U>) =
        StateMachineHelpers.__runtimeAsync<Task<'U>> (fun () ->
            AsyncHelpers.Await task
            AsyncHelpers.Await(continuation()))

    member _.Combine(first: Task<unit>, second: unit -> Task<'T>) =
        sequenceTasks first second

    member _.For(sequence: seq<'T>, body: 'T -> Task<unit>) =
        let enumerator = sequence.GetEnumerator()

        let rec loop () =
            if enumerator.MoveNext() then
                sequenceTasks (body enumerator.Current) loop
            else
                enumerator.Dispose()
                Task.FromResult()

        loop()

[<AutoOpen>]
module RuntimeTask =
    let runtimeTask = RuntimeTaskBuilder()

let private runCase () =
    let mutable disposed = false

    let sequence =
        { new IEnumerable<int> with
            member _.GetEnumerator() : IEnumerator<int> =
                let enumerator = (Seq.ofList [ 1; 2; 3 ]).GetEnumerator()

                { new IEnumerator<int> with
                    member _.Current : int = enumerator.Current
                    member _.Current : obj = box enumerator.Current
                    member _.MoveNext() = enumerator.MoveNext()
                    member _.Reset() = enumerator.Reset()

                    member _.Dispose() =
                        disposed <- true
                        enumerator.Dispose() }

            member this.GetEnumerator() : IEnumerator =
                this.GetEnumerator() :> IEnumerator }

    let computation =
        runtimeTask {
            for value in sequence do
                do! Task.Delay(1)

                if value = 2 then
                    failwith "for body failure"
        }

    try
        computation.GetAwaiter().GetResult()
    with _ ->
        ()

    disposed

[<EntryPoint>]
let main _ =
    // Known failure equivalent to TaskBuilder's testForLoopSadPathComplex.
    if runCase () then
        1
    else
        printfn "Problematic: for loop does not dispose enumerator after an exception"
        0
