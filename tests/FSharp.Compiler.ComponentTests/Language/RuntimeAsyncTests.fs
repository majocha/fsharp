module Language.RuntimeAsyncTests

open Xunit
open FSharp.Test.Compiler
open System.IO

let private runtimeAsyncSource = """
module RuntimeAsyncTest

open System.Threading.Tasks
open System.Runtime.CompilerServices
open Microsoft.FSharp.Core.CompilerServices

let add (x: int) (y: int) : Task<int> =
    StateMachineHelpers.__runtimeAsync (
        AsyncHelpers.Await(Task.Delay(1))
        x + y)

let rawBody () : Task<int> =
    StateMachineHelpers.__runtimeAsync 1

type Calculator() =
    member _.Add(x: int, y: int) : Task<int> =
        StateMachineHelpers.__runtimeAsync (
            AsyncHelpers.Await(Task.Delay(1))
            x + y)

    member _.AddRaw(x: int) : Task<int> =
        StateMachineHelpers.__runtimeAsync (x + 1)
"""

let private runtimeAsyncRawSource = """
module RuntimeAsyncRawTest

open System.Threading.Tasks
open Microsoft.FSharp.Core.CompilerServices
open System.Runtime.CompilerServices

type RuntimeAsyncCode<'T> = unit -> 'T

type RuntimeTaskBuilder() =
    member inline _.Delay([<InlineIfLambda>] generator: unit -> RuntimeAsyncCode<'T>) =
        fun () -> (generator())()

    member inline _.Run([<InlineIfLambda>] code: RuntimeAsyncCode<'T>) : Task<'T> =
        StateMachineHelpers.__runtimeAsync (code())

    member inline _.Zero() : RuntimeAsyncCode<unit> =
        fun () -> ()

    member inline _.Return(value: 'T) : RuntimeAsyncCode<'T> =
        fun () -> value

    member inline _.Bind(task: Task, [<InlineIfLambda>] continuation: unit -> RuntimeAsyncCode<'U>) =
        fun () ->
            AsyncHelpers.Await task
            (continuation())()

    member inline _.Combine(first: RuntimeAsyncCode<unit>, second: RuntimeAsyncCode<'T>) =
        fun () ->
            first()
            second()

[<AutoOpen>]
module RuntimeTask =
    let runtimeTask = RuntimeTaskBuilder()

type ICalculator =
    abstract Combined: unit -> Task<int>

type Calculator() =
    member _.Combined() : Task<int> =
        runtimeTask {
            do! Task.Delay(1)
            do! Task.Delay(1)
            return 42
        }

    interface ICalculator with
        member this.Combined() = this.Combined()

"""

let private runtimeTaskSource = """
module RuntimeTaskTest

open System.Threading.Tasks
open System.Runtime.CompilerServices
open Microsoft.FSharp.Core.CompilerServices

let delayed value =
    Task.Delay(1).ContinueWith(fun (_: Task) -> value)

type RuntimeAsyncCode<'T> = unit -> 'T

type RuntimeTaskBuilder() =
    member inline _.Delay([<InlineIfLambda>] generator: unit -> RuntimeAsyncCode<'T>) =
        fun () -> (generator())()

    member inline _.Run([<InlineIfLambda>] code: RuntimeAsyncCode<'T>) : Task<'T> =
        StateMachineHelpers.__runtimeAsync (code())

    member inline _.Zero() : RuntimeAsyncCode<unit> =
        fun () -> ()

    member inline _.Return(value: 'T) : RuntimeAsyncCode<'T> =
        fun () -> value

    member inline _.Bind(task: Task, [<InlineIfLambda>] continuation: unit -> RuntimeAsyncCode<'U>) =
        fun () ->
            AsyncHelpers.Await task
            (continuation())()

    member inline _.Bind(task: Task<'T>, [<InlineIfLambda>] continuation: 'T -> RuntimeAsyncCode<'U>) =
        fun () ->
            let result = AsyncHelpers.Await task
            (continuation result)()

    member inline _.Bind(code: RuntimeAsyncCode<'T>, [<InlineIfLambda>] continuation: 'T -> RuntimeAsyncCode<'U>) =
        fun () ->
            let result = code()
            (continuation result)()

    member inline _.Bind(
        task: Task<struct ('T1 * 'T2)>,
        [<InlineIfLambda>] continuation: ('T1 * 'T2) -> RuntimeAsyncCode<'U>
    ) =
        fun () ->
            let struct (first, second) = AsyncHelpers.Await task
            (continuation (first, second))()

    member inline _.Combine(first: RuntimeAsyncCode<unit>, continuation: RuntimeAsyncCode<'T>) =
        fun () ->
            first()
            continuation()

    member inline _.MergeSources(left: Task<'T1>, right: Task<'T2>) : RuntimeAsyncCode<struct ('T1 * 'T2)> =
        fun () ->
            let leftResult = AsyncHelpers.Await left
            let rightResult = AsyncHelpers.Await right
            struct (leftResult, rightResult)

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
        StateMachineHelpers.__runtimeAsync (
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
    StateMachineHelpers.__runtimeAsync 1
"""
    |> typecheck
    |> shouldFail
    |> withErrorCode 3350

[<Fact>]
let ``runtime async rejects non Task result carriers`` () =
    FSharp """
open Microsoft.FSharp.Core.CompilerServices

let f : string =
    StateMachineHelpers.__runtimeAsync "result"
"""
    |> withLangVersionPreview
    |> typecheck
    |> shouldFail
    |> withErrorCode 1

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
let ``runtime async compiles functions and members`` () =
    FSharp runtimeAsyncSource
    |> withLangVersionPreview
    |> compile
    |> shouldSucceed

[<Fact>]
let ``runtime async combines awaited chunks without delegates`` () =
    FSharp runtimeAsyncRawSource
    |> withLangVersionPreview
    |> compile
    |> verifyILContains [
        "Task::Delay(int32)"
        "AsyncHelpers::Await(class [runtime]System.Threading.Tasks.Task)"
    ]
    |> shouldSucceed

[<Fact>]
let ``runtime task builder compiles through runtime async`` () =
    FSharp runtimeTaskSource
    |> withLangVersionPreview
    |> compile
    |> shouldSucceed

[<Fact>]
let ``runtime task builder fixture compiles through runtime async`` () =
    Path.Combine(__SOURCE_DIRECTORY__, "RuntimeAsync", "RuntimeTasks.fs")
    |> FsFromPath
    |> withLangVersionPreview
    |> compile
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
    StateMachineHelpers.__runtimeAsync 1
"""
    |> withLangVersionPreview
    |> typecheck
    |> shouldFail
    |> withErrorCode 3351
#endif
