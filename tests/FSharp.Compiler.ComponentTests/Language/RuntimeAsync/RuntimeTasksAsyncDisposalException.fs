module RuntimeTasksAsyncDisposalException

open System
open System.Runtime.CompilerServices
open System.Threading.Tasks
open Microsoft.FSharp.Control
open Microsoft.FSharp.Core.CompilerServices

let private delayed value =
    Task.Delay(1).ContinueWith(fun (_: Task) -> value)

type RuntimeTaskBuilder() =
    member _.Delay(generator: unit -> Task<'T>) = generator

    member _.Run(generator: unit -> Task<'T>) : Task<'T> =
        StateMachineHelpers.__runtimeAsync<Task<'T>> (fun () ->
            AsyncHelpers.Await(generator()))

    member _.Return(value: 'T) : Task<'T> = Task.FromResult value

    member _.Zero() : Task<unit> = Task.FromResult()

    member _.Bind(task: Task<'T>, continuation: 'T -> Task<'U>) =
        StateMachineHelpers.__runtimeAsync<Task<'U>> (fun () ->
            let result = AsyncHelpers.Await task
            AsyncHelpers.Await(continuation result))

    member _.TryFinally(body: unit -> Task<'T>, compensation: unit -> unit) =
        StateMachineHelpers.__runtimeAsync<Task<'T>> (fun () ->
            try
                AsyncHelpers.Await(body())
            finally
                compensation())

    member _.Using(resource: 'Resource, body: 'Resource -> Task<'T>) : Task<'T> =
        StateMachineHelpers.__runtimeAsync<Task<'T>> (fun () ->
            try
                AsyncHelpers.Await(body resource)
            finally
                match box resource with
                | :? IAsyncDisposable as disposable ->
                    AsyncHelpers.Await(disposable.DisposeAsync())
                | _ -> ())

[<AutoOpen>]
module RuntimeTask =
    let runtimeTask = RuntimeTaskBuilder()

let private runCase () =
    let mutable disposed = 0

    let computation =
        runtimeTask {
            use _resource =
                { new IAsyncDisposable with
                    member _.DisposeAsync() =
                        ValueTask(
                            delayed (disposed <- disposed + 1)
                        ) }

            failwith "body failure"
        }

    try
        computation.GetAwaiter().GetResult() |> ignore
    with _ ->
        ()

    disposed

[<EntryPoint>]
let main _ =
    if runCase () = 1 then 0 else 1
