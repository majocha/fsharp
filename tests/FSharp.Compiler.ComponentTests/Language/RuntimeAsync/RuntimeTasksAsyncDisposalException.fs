module RuntimeTasksAsyncDisposalException

open System
open System.Runtime.CompilerServices
open System.Threading.Tasks
open Microsoft.FSharp.Control
open Microsoft.FSharp.Core.CompilerServices

let private delayed value =
    Task.Delay(1).ContinueWith(fun (_: Task) -> value)

type RuntimeAsyncCode<'T> = unit -> 'T

type RuntimeTaskBuilder() =
    member inline _.Delay([<InlineIfLambda>] generator: unit -> RuntimeAsyncCode<'T>) =
        fun () -> (generator())()

    member inline _.Run([<InlineIfLambda>] code: RuntimeAsyncCode<'T>) : Task<'T> =
        StateMachineHelpers.__runtimeAsync (code())

    member inline _.Return(value: 'T) : RuntimeAsyncCode<'T> =
        fun () -> value

    member inline _.Zero() : RuntimeAsyncCode<unit> =
        fun () -> ()

    member inline _.Bind(task: Task<'T>, [<InlineIfLambda>] continuation: 'T -> RuntimeAsyncCode<'U>) =
        fun () ->
            let result = AsyncHelpers.Await task
            (continuation result)()

    member inline _.TryFinally(
        [<InlineIfLambda>] body: RuntimeAsyncCode<'T>,
        compensation: unit -> unit
    ) =
        fun () ->
            try
                body()
            finally
                compensation()

    member inline _.Using(resource: 'Resource, [<InlineIfLambda>] body: 'Resource -> RuntimeAsyncCode<'T>) : RuntimeAsyncCode<'T> =
        fun () ->
            try
                (body resource)()
            finally
                match box resource with
                | :? IAsyncDisposable as disposable ->
                    AsyncHelpers.Await(disposable.DisposeAsync())
                | _ -> ()

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
