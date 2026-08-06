module Language.RuntimeAsyncTests

open Xunit
open FSharp.Test.Compiler
open System.IO

let private runtimeAsyncSource = """
module RuntimeAsyncTest

open System.Threading.Tasks
open System.Runtime.CompilerServices
open Microsoft.FSharp.Core.CompilerServices

let add =
    StateMachineHelpers.__runtimeAsync<Task<int>> (fun (x: int) (y: int) ->
        AsyncHelpers.Await(Task.Delay(1))
        x + y)

let getValueTask () : ValueTask<int> =
    StateMachineHelpers.__runtimeAsync<ValueTask<int>> (fun () ->
        AsyncHelpers.Await(ValueTask<int>(Task.Delay(1).ContinueWith(fun (_: Task) -> 7))))

let rawBody : Task<int> =
    StateMachineHelpers.__runtimeAsync<Task<int>> (1)

type Calculator() =
    member _.Add(x: int, y: int) : Task<int> =
        StateMachineHelpers.__runtimeAsync<Task<int>> (fun () ->
            AsyncHelpers.Await(Task.Delay(1))
            x + y)

    member _.AddRaw(x: int) : Task<int> =
        StateMachineHelpers.__runtimeAsync<Task<int>> (x + 1)
"""

let private runtimeAsyncRawSource = """
module RuntimeAsyncRawTest

open System.Threading.Tasks
open Microsoft.FSharp.Core.CompilerServices

let rawValue : Task<int> =
    StateMachineHelpers.__runtimeAsync<Task<int>> (40 + 2)

type Calculator() =
    member _.Add(value: int) : Task<int> =
        StateMachineHelpers.__runtimeAsync<Task<int>> (value + 1)

[<EntryPoint>]
let main _ =
    let rawResult = rawValue.GetAwaiter().GetResult()
    let memberResult = Calculator().Add(40).GetAwaiter().GetResult()

    if rawResult = 42 && memberResult = 41 then 0 else 1
"""

let private runtimeTaskSource = """
module RuntimeTaskTest

open System.Threading.Tasks
open System.Runtime.CompilerServices
open Microsoft.FSharp.Core.CompilerServices

let delayed value =
    Task.Delay(1).ContinueWith(fun (_: Task) -> value)

type RuntimeTaskBuilder() =
    member _.Delay(generator: unit -> Task<'T>) = generator

    member _.Run(generator: unit -> Task<'T>) : Task<'T> =
        StateMachineHelpers.__runtimeAsync<Task<'T>> (fun () ->
            AsyncHelpers.Await(generator()))

    member _.Zero() : Task<unit> =
        StateMachineHelpers.__runtimeAsync<Task<unit>> (fun () -> ())

    member _.Return(value: 'T) =
        StateMachineHelpers.__runtimeAsync<Task<'T>> (fun () -> value)

    member _.Bind(task: Task, continuation: unit -> Task<'U>) =
        StateMachineHelpers.__runtimeAsync<Task<'U>> (fun () ->
            AsyncHelpers.Await task
            AsyncHelpers.Await(continuation()))

    member _.Bind(task: Task<'T>, continuation: 'T -> Task<'U>) =
        StateMachineHelpers.__runtimeAsync<Task<'U>> (fun () ->
            let result = AsyncHelpers.Await task
            AsyncHelpers.Await(continuation result))

    member _.Bind(task: Task<struct ('T1 * 'T2)>, continuation: ('T1 * 'T2) -> Task<'U>) =
        StateMachineHelpers.__runtimeAsync<Task<'U>> (fun () ->
            let struct (first, second) = AsyncHelpers.Await task
            AsyncHelpers.Await(continuation (first, second)))

    member _.Combine(first: Task<unit>, continuation: unit -> Task<'T>) =
        StateMachineHelpers.__runtimeAsync<Task<'T>> (fun () ->
            AsyncHelpers.Await first
            AsyncHelpers.Await(continuation()))

    member _.MergeSources(left: Task<'T1>, right: Task<'T2>) =
        StateMachineHelpers.__runtimeAsync<Task<struct ('T1 * 'T2)>> (fun () ->
            let leftResult = AsyncHelpers.Await left
            let rightResult = AsyncHelpers.Await right
            struct (leftResult, rightResult))

[<AutoOpen>]
module RuntimeTask =
    let runtimeTask = RuntimeTaskBuilder()

let compute x y =
    runtimeTask {
        do! Task.Delay(1)
        let! left = delayed x
        and! right = delayed y
        return left + right
    }

type Calculator() =
    member _.Add(x: int, y: int) : Task<int> =
        StateMachineHelpers.__runtimeAsync<Task<int>> (fun () ->
            AsyncHelpers.Await(Task.Delay(1))
            x + y)

[<EntryPoint>]
let main _ =
    let calculator = Calculator()
    let builderResult = compute 20 22 |> fun task -> task.GetAwaiter().GetResult()
    let memberResult = calculator.Add(20, 22).GetAwaiter().GetResult()

    if builderResult = 42 && memberResult = 42 then 0 else 1
"""

[<Fact>]
let ``runtime async requires preview language version`` () =
    FSharp """
open System.Threading.Tasks
open Microsoft.FSharp.Core.CompilerServices

let f : Task<int> =
    StateMachineHelpers.__runtimeAsync (fun () -> 1)
"""
    |> typecheck
    |> shouldFail
    |> withErrorCode 3350

[<Fact>]
let ``runtime async accepts only Task and ValueTask carriers`` () =
    FSharp """
open Microsoft.FSharp.Core.CompilerServices

let f =
    StateMachineHelpers.__runtimeAsync<string> (fun () -> "result")
"""
    |> withLangVersionPreview
    |> typecheck
    |> shouldFail
    |> withErrorCode 4001

[<Fact>]
let ``runtime async intrinsic does not capture user-defined same-named values`` () =
    FSharp """
let __runtimeAsync value = value
let result = __runtimeAsync 1
"""
    |> typecheck
    |> shouldSucceed

#if NETCOREAPP
[<Fact>]
let ``runtime async compiles functions members and ValueTask`` () =
    FSharp runtimeAsyncSource
    |> withLangVersionPreview
    |> compile
    |> shouldSucceed

[<Fact>]
let ``runtime async raw expressions execute`` () =
    FSharp runtimeAsyncRawSource
    |> withLangVersionPreview
    |> compileExeAndRun
    |> shouldSucceed

[<Fact>]
let ``runtime task builder executes through runtime async`` () =
    FSharp runtimeTaskSource
    |> withLangVersionPreview
    |> compileExeAndRun
    |> shouldSucceed

[<Fact>]
let ``runtime task builder fixture executes through runtime async`` () =
    Path.Combine(__SOURCE_DIRECTORY__, "RuntimeAsync", "RuntimeTasks.fs")
    |> FsFromPath
    |> withLangVersionPreview
    |> compileExeAndRun
    |> shouldSucceed

// Equivalent to TaskBuilder's testUsingAsyncDisposableExnAsync. Compilation succeeds,
// but executing the fixture currently terminates the process with 0xC0000409.
[<Fact>]
let ``runtime task async disposal exception compiles (runtime execution is failing)`` () =
    Path.Combine(__SOURCE_DIRECTORY__, "RuntimeAsync", "RuntimeTasksAsyncDisposalException.fs")
    |> FsFromPath
    |> withLangVersionPreview
    |> compile
    |> shouldSucceed

#else
[<Fact>]
let ``runtime async reports unsupported target runtime`` () =
    FSharp """
open System.Threading.Tasks
open Microsoft.FSharp.Core.CompilerServices

let f : Task<int> =
    StateMachineHelpers.__runtimeAsync<Task<int>> (fun () -> 1)
"""
    |> withLangVersionPreview
    |> typecheck
    |> shouldFail
    |> withErrorCode 3351
#endif
