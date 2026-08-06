module RuntimeTasks

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Threading
open System.Threading.Tasks
open Microsoft.FSharp.Control
open Microsoft.FSharp.Core.CompilerServices

let private delayed value =
    Task.Delay(1).ContinueWith(fun (_: Task) -> value)

type RuntimeAsyncCode<'T> = unit -> 'T

let inline bindAwaiter
    ([<InlineIfLambda>] getAwaiter: unit -> 'Awaiter)
    ([<InlineIfLambda>] getResult: 'Awaiter -> 'T)
    ([<InlineIfLambda>] continuation: 'T -> RuntimeAsyncCode<'U>)
    =
    fun () ->
        let awaiter = getAwaiter()
        AsyncHelpers.AwaitAwaiter awaiter
        let result = getResult awaiter
        (continuation result)()

type RuntimeTaskBuilder() =
    member inline _.Delay([<InlineIfLambda>] generator: unit -> RuntimeAsyncCode<'T>) =
        fun () -> (generator())()

    member inline _.Run([<InlineIfLambda>] code: RuntimeAsyncCode<'T>) : Task<'T> =
        StateMachineHelpers.__runtimeAsync (code())

    member inline _.Zero() : RuntimeAsyncCode<unit> =
        fun () -> ()

    member inline _.Return(value: 'T) : RuntimeAsyncCode<'T> =
        fun () -> value

    member inline _.ReturnFrom(task: Task<'T>) : RuntimeAsyncCode<'T> =
        fun () -> AsyncHelpers.Await task

    member inline _.ReturnFrom(task: ValueTask<'T>) : RuntimeAsyncCode<'T> =
        fun () -> AsyncHelpers.Await task

    member inline _.ReturnFrom(computation: Async<'T>) : RuntimeAsyncCode<'T> =
        fun () -> AsyncHelpers.Await(Async.StartAsTask computation)

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

    member inline _.Bind(task: ValueTask, [<InlineIfLambda>] continuation: unit -> RuntimeAsyncCode<'U>) =
        fun () ->
                AsyncHelpers.Await task
                (continuation())()

    member inline _.Bind(task: ValueTask<'T>, [<InlineIfLambda>] continuation: 'T -> RuntimeAsyncCode<'U>) =
        fun () ->
                let result = AsyncHelpers.Await task
                (continuation result)()

    member inline _.Bind(computation: Async<'T>, [<InlineIfLambda>] continuation: 'T -> RuntimeAsyncCode<'U>) =
        fun () ->
                let result = AsyncHelpers.Await(Async.StartAsTask computation)
                (continuation result)()

    member inline _.Combine(first: RuntimeAsyncCode<unit>, second: RuntimeAsyncCode<'T>) =
        fun () ->
                first()
                second()

    member inline _.TryWith(
        [<InlineIfLambda>] body: RuntimeAsyncCode<'T>,
        [<InlineIfLambda>] handler: exn -> RuntimeAsyncCode<'T>
    ) =
        fun () ->
                try
                    body()
                with error ->
                    (handler error)()

    member inline _.TryFinally([<InlineIfLambda>] body: RuntimeAsyncCode<'T>, compensation: unit -> unit) =
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
                    | :? IDisposable as disposable ->
                        disposable.Dispose()
                    | _ -> ()

    member inline _.While(guard: unit -> bool, [<InlineIfLambda>] body: RuntimeAsyncCode<unit>) : RuntimeAsyncCode<unit> =
        fun () ->
            while guard() do
                body()

    member inline _.For(sequence: seq<'T>, [<InlineIfLambda>] body: 'T -> RuntimeAsyncCode<unit>) : RuntimeAsyncCode<unit> =
        fun () ->
            use enumerator = sequence.GetEnumerator()

            while enumerator.MoveNext() do
                (body enumerator.Current)()

    member inline _.MergeSources(left: Task<'T1>, right: Task<'T2>) : RuntimeAsyncCode<struct ('T1 * 'T2)> =
        fun () ->
            let leftResult = AsyncHelpers.Await left
            let rightResult = AsyncHelpers.Await right
            leftResult, rightResult

    member inline _.MergeSources(left: ValueTask<'T1>, right: ValueTask<'T2>) : RuntimeAsyncCode<struct ('T1 * 'T2)> =
        fun () ->
            let leftResult = AsyncHelpers.Await left
            let rightResult = AsyncHelpers.Await right
            leftResult, rightResult

    member inline _.MergeSources(left: Task<'T1>, right: ValueTask<'T2>) : RuntimeAsyncCode<struct ('T1 * 'T2)> =
        fun () ->
            let leftResult = AsyncHelpers.Await left
            let rightResult = AsyncHelpers.Await right
            leftResult, rightResult

    member inline _.MergeSources(left: ValueTask<'T1>, right: Task<'T2>) : RuntimeAsyncCode<struct ('T1 * 'T2)> =
        fun () ->
            let leftResult = AsyncHelpers.Await left
            let rightResult = AsyncHelpers.Await right
            leftResult, rightResult

    member inline _.MergeSources(left: Task<'T1>, right: Async<'T2>) : RuntimeAsyncCode<struct ('T1 * 'T2)> =
        fun () ->
            let leftResult = AsyncHelpers.Await left
            let rightResult = AsyncHelpers.Await(Async.StartAsTask right)
            leftResult, rightResult

    member inline _.MergeSources(left: Async<'T1>, right: Task<'T2>) : RuntimeAsyncCode<struct ('T1 * 'T2)> =
        fun () ->
            let leftResult = AsyncHelpers.Await(Async.StartAsTask left)
            let rightResult = AsyncHelpers.Await right
            leftResult, rightResult

    member inline _.MergeSources(left: Async<'T1>, right: Async<'T2>) : RuntimeAsyncCode<struct ('T1 * 'T2)> =
        fun () ->
            let leftResult = AsyncHelpers.Await(Async.StartAsTask left)
            let rightResult = AsyncHelpers.Await(Async.StartAsTask right)
            leftResult, rightResult

    member inline _.MergeSources(left: RuntimeAsyncCode<'T1>, right: Task<'T2>) : RuntimeAsyncCode<struct ('T1 * 'T2)> =
        fun () ->
            let leftResult = left()
            let rightResult = AsyncHelpers.Await right
            leftResult, rightResult

    member inline _.MergeSources(left: RuntimeAsyncCode<'T1>, right: ValueTask<'T2>) : RuntimeAsyncCode<struct ('T1 * 'T2)> =
        fun () ->
            let leftResult = left()
            let rightResult = AsyncHelpers.Await right
            leftResult, rightResult

    member inline _.MergeSources(left: RuntimeAsyncCode<'T1>, right: Async<'T2>) : RuntimeAsyncCode<struct ('T1 * 'T2)> =
        fun () ->
            let leftResult = left()
            let rightResult = AsyncHelpers.Await(Async.StartAsTask right)
            leftResult, rightResult

    member inline _.MergeSources(left: RuntimeAsyncCode<'T1>, right: RuntimeAsyncCode<'T2>) : RuntimeAsyncCode<struct ('T1 * 'T2)> =
        fun () ->
            let leftResult = left()
            let rightResult = right()
            leftResult, rightResult

    member inline _.MergeSources(left: Task<'T1>, right: RuntimeAsyncCode<'T2>) : RuntimeAsyncCode<struct ('T1 * 'T2)> =
        fun () ->
            let leftResult = AsyncHelpers.Await left
            let rightResult = right()
            leftResult, rightResult

    member inline _.MergeSources(left: ValueTask<'T1>, right: RuntimeAsyncCode<'T2>) : RuntimeAsyncCode<struct ('T1 * 'T2)> =
        fun () ->
            let leftResult = AsyncHelpers.Await left
            let rightResult = right()
            leftResult, rightResult

    member inline _.MergeSources(left: Async<'T1>, right: RuntimeAsyncCode<'T2>) : RuntimeAsyncCode<struct ('T1 * 'T2)> =
        fun () ->
            let leftResult = AsyncHelpers.Await(Async.StartAsTask left)
            let rightResult = right()
            leftResult, rightResult

    member inline _.MergeSources(left: Async<'T1>, right: ValueTask<'T2>) : RuntimeAsyncCode<struct ('T1 * 'T2)> =
        fun () ->
            let leftResult = AsyncHelpers.Await(Async.StartAsTask left)
            let rightResult = AsyncHelpers.Await right
            leftResult, rightResult

    member inline _.MergeSources(left: ValueTask<'T1>, right: Async<'T2>) : RuntimeAsyncCode<struct ('T1 * 'T2)> =
        fun () ->
            let leftResult = AsyncHelpers.Await left
            let rightResult = AsyncHelpers.Await(Async.StartAsTask right)
            leftResult, rightResult

module RuntimeTaskAwaitableExtensions =
    type RuntimeTaskBuilder with
        [<NoEagerConstraintApplication>]
        member inline _.Bind< ^TaskLike, ^Awaiter, 'T, 'U
            when ^TaskLike: (member GetAwaiter: unit -> ^Awaiter)
            and ^Awaiter :> ICriticalNotifyCompletion
            and ^Awaiter: (member get_IsCompleted: unit -> bool)
            and ^Awaiter: (member GetResult: unit -> 'T)>
            (task: ^TaskLike, [<InlineIfLambda>] continuation: 'T -> RuntimeAsyncCode<'U>)
            : RuntimeAsyncCode<'U> =
            bindAwaiter
                (fun () -> (^TaskLike: (member GetAwaiter: unit -> ^Awaiter) task))
                (fun awaiter -> (^Awaiter: (member GetResult: unit -> 'T) awaiter))
                continuation

open RuntimeTaskAwaitableExtensions

[<AutoOpen>]
module RuntimeTask =
    let runtimeTask = RuntimeTaskBuilder()

type Disposable(log: ResizeArray<string>) =
    interface IDisposable with
        member _.Dispose() = log.Add "disposed"

type AsyncDisposable(log: ResizeArray<string>) =
    interface IAsyncDisposable with
        member _.DisposeAsync() =
            ValueTask(Task.Delay(1).ContinueWith(fun (_: Task) -> log.Add "async-disposed"))

type Calculator() =
    member _.Add(x, y) =
        runtimeTask {
            do! Task.Delay(1)
            return x + y
        }

exception TestException of string

let private require condition message =
    printfn "Checking: %s" message
    if not condition then
        failwith message

let private failtest message = raise (TestException message)

let private resultOf (task: Task<'T>) = task.GetAwaiter().GetResult()

let private checkBasicBinding () =
    let firstTask: Task<int> = Task.FromResult 20
    let secondTask: Task<int> = delayed 22

    let result =
        runtimeTask {
            let! first = firstTask
            do! Task.Delay(1)
            let! second = secondTask
            return first + second
        }

    require (resultOf result = 42) "basic binding"

    let immediate =
        runtimeTask {
            let! first = Task.FromResult 1
            let! second = Task.FromResult 2
            return first + second
        }

    require immediate.IsCompleted "immediate task did not complete synchronously"
    require (resultOf immediate = 3) "immediate task result"

let private checkImmediateAndDelayedExecution () =
    let mutable progress = 0

    let task =
        runtimeTask {
            progress <- 1
            do! Task.Delay(1)
            progress <- 2
            return 42
        }

    require (progress = 1) "runtime task ran past suspension"
    require (resultOf task = 42) "delayed result"
    require (progress = 2) "runtime task did not resume"

    let completion = TaskCompletionSource<unit>()

    let nonBlocking =
        runtimeTask {
            do! completion.Task
            return 42
        }

    require (not nonBlocking.IsCompleted) "runtime task blocked on await"
    completion.SetResult()
    require (resultOf nonBlocking = 42) "task completion source result"

let private checkNestedAndMembers () =
    let nested (value: int) : Task<int> =
        runtimeTask {
            do! Task.Delay(1)
            return value
        }

    let combine (x: int) (y: int) : Task<int> =
        runtimeTask {
            let! left = nested x
            let! right = nested y
            return left + right
        }

    require (resultOf (combine 20 22) = 42) "nested binding"
    require (Calculator().Add(20, 22).GetAwaiter().GetResult() = 42) "member binding"

let private checkTaskBuilderCoreCases () =
    let caughtWithoutAwait =
        runtimeTask {
            try
                return 1
            with _ ->
                return 2
        }

    require (resultOf caughtWithoutAwait = 1) "synchronous try"

    let caughtWithAwait =
        runtimeTask {
            try
                let! value = delayed 1
                return value
            with _ ->
                return 2
        }

    require (resultOf caughtWithAwait = 1) "try with awaited result"

    let incompleteMatch =
        runtimeTask {
            try
                do! Task.Delay(0)
            with
            | :? ArgumentException -> ()
        }

    resultOf incompleteMatch

    let threeTasks =
        runtimeTask {
            let! first = delayed 1
            and! second = delayed 2
            and! third = delayed 3
            return first + second + third
        }

    require (resultOf threeTasks = 6) "three task merge"

    let threeValueTasks =
        runtimeTask {
            let! first = ValueTask<int>(delayed 1)
            and! second = ValueTask<int>(delayed 2)
            and! third = ValueTask<int>(delayed 3)
            return first + second + third
        }

    require (resultOf threeValueTasks = 6) "three value task merge"

    let threeAsyncs =
        runtimeTask {
            let! first = async { return 1 }
            and! second = async { return 2 }
            and! third = async { return 3 }
            return first + second + third
        }

    require (resultOf threeAsyncs = 6) "three async merge"

let private checkApplicativeBinding () =
    let result =
        runtimeTask {
            let! left = delayed 20
            and! right = ValueTask<int>(delayed 22)
            return left + right
        }

    require (resultOf result = 42) "applicative binding"

    let threeTasks =
        runtimeTask {
            let! first = delayed 1
            and! second = delayed 2
            and! third = delayed 3
            return first + second + third
        }

    require (resultOf threeTasks = 6) "three task binding"

    let asyncs =
        runtimeTask {
            let! left = async { return 1 }
            and! right = async { return 2 }
            return left + right
        }

    require (resultOf asyncs = 3) "two async binding"

    let asyncAndValueTask =
        runtimeTask {
            let! left = async { return 1 }
            and! right = ValueTask<int>(delayed 2)
            return left + right
        }

    require (resultOf asyncAndValueTask = 3) "async and value task binding"

    let valueTaskAndAsync =
        runtimeTask {
            let! left = ValueTask<int>(delayed 1)
            and! right = async { return 2 }
            return left + right
        }

    require (resultOf valueTaskAndAsync = 3) "value task and async binding"

    let twoTasksAndValueTask =
        runtimeTask {
            let! first = delayed 1
            and! second = delayed 2
            and! third = ValueTask<int>(delayed 3)
            return first + second + third
        }

    require (resultOf twoTasksAndValueTask = 6) "two tasks and value task binding"

    let twoAsyncsAndValueTask =
        runtimeTask {
            let! first = async { return 1 }
            and! second = async { return 2 }
            and! third = ValueTask<int>(delayed 3)
            return first + second + third
        }

    require (resultOf twoAsyncsAndValueTask = 6) "two asyncs and value task binding"

let private checkAwaitables () =
    let yielded =
        runtimeTask {
            do! Task.Yield()
            return 42
        }

    let valueTask =
        runtimeTask {
            let! value = ValueTask<int>(delayed 42)
            return value
        }

    let asyncValue =
        runtimeTask {
            let! value = async { return 42 }
            return value
        }

    require (resultOf yielded = 42) "yield awaiter"
    require (resultOf valueTask = 42) "value task binding"
    require (resultOf asyncValue = 42) "async binding"

let private checkTryFinallyAndUsing () =
    let mutable completed = false

    let happy =
        runtimeTask {
            try
                do! Task.Delay(1)
            finally
                completed <- true
        }

    resultOf happy
    require completed "try finally happy path"

    let mutable failedFinally = false

    let failed =
        runtimeTask {
            try
                do! Task.Delay(1)
                failtest "body failure"
            finally
                failedFinally <- true
        }

    try
        resultOf failed |> ignore
        failwith "failed try finally completed"
    with
    | TestException "body failure" -> ()
    require failedFinally "try finally failure path"

    let mutable disposed = false

    let disposableResult =
        runtimeTask {
            use _resource =
                { new IDisposable with
                    member _.Dispose() = disposed <- true }

            do! Task.Delay(1)
        }

    resultOf disposableResult
    require disposed "synchronous disposal"

    let mutable asyncDisposed = 0

    let asyncDisposableResult =
        runtimeTask {
            use _resource =
                { new IAsyncDisposable with
                    member _.DisposeAsync() =
                        ValueTask(
                            Task.Delay(1).ContinueWith(fun (_: Task) ->
                                asyncDisposed <- asyncDisposed + 1)
                        ) }

            do! Task.Delay(1)
        }

    resultOf asyncDisposableResult
    require (asyncDisposed = 1) "asynchronous disposal"

    let mutable boundDisposed = false

    let boundDisposableResult =
        runtimeTask {
            use! _resource =
                runtimeTask {
                    do! Task.Delay(1)

                    return
                        { new IDisposable with
                            member _.Dispose() = boundDisposed <- true }
                }

            do! Task.Delay(1)
        }

    resultOf boundDisposableResult
    require boundDisposed "bound resource disposal"

let private checkLoops () =
    let mutable whileCount = 0

    let whileResult =
        runtimeTask {
            while whileCount < 3 do
                whileCount <- whileCount + 1
                do! Task.Delay(1)
        }

    whileResult.GetAwaiter().GetResult()
    require (whileCount = 3) "while loop"

    let forResult =
        runtimeTask {
            let mutable total = 0

            for value in [ 1; 2; 3 ] do
                do! Task.Delay(1)
                total <- total + value

            return total
        }

    require (resultOf forResult = 6) "for loop"

    let mutable enumeratorDisposed = false

    let sequence =
        { new IEnumerable<string> with
            member _.GetEnumerator() : IEnumerator<string> =
                let enumerator = (Seq.ofList [ "a"; "b"; "c" ]).GetEnumerator()

                { new IEnumerator<string> with
                    member _.Current : string = enumerator.Current
                    member _.Current : obj = box enumerator.Current
                    member _.MoveNext() = enumerator.MoveNext()
                    member _.Reset() = enumerator.Reset()

                    member _.Dispose() =
                        enumeratorDisposed <- true
                        enumerator.Dispose() }

            member this.GetEnumerator() : System.Collections.IEnumerator =
                this.GetEnumerator() :> System.Collections.IEnumerator }

    let complexFor =
        runtimeTask {
            let mutable count = 0

            for value in sequence do
                do! Task.Delay(1)
                require (value = [ "a"; "b"; "c" ][count]) "for loop value"
                count <- count + 1
        }

    resultOf complexFor
    require enumeratorDisposed "for loop disposal"

    let mutable failedEnumeratorDisposed = false

    let failingSequence =
        { new IEnumerable<int> with
            member _.GetEnumerator() : IEnumerator<int> =
                let enumerator = (Seq.ofList [ 1; 2; 3 ]).GetEnumerator()

                { new IEnumerator<int> with
                    member _.Current : int = enumerator.Current
                    member _.Current : obj = box enumerator.Current
                    member _.MoveNext() = enumerator.MoveNext()
                    member _.Reset() = enumerator.Reset()

                    member _.Dispose() =
                        failedEnumeratorDisposed <- true
                        enumerator.Dispose() }

            member this.GetEnumerator() : System.Collections.IEnumerator =
                this.GetEnumerator() :> System.Collections.IEnumerator }

    let caughtFor =
        runtimeTask {
            try
                for value in failingSequence do
                    do! Task.Yield()

                    if value = 2 then
                        failtest "for body failure"

                return 0
            with
            | TestException "for body failure" -> return 42
        }

    require (resultOf caughtFor = 42) "for loop exception"

    require failedEnumeratorDisposed "for loop disposal after exception"

let private checkExceptionsAndStackSafety () =
    let mutable ranBeforeException = false
    let mutable ranAfterException = false

    let failed =
        runtimeTask {
            ranBeforeException <- true
            failtest "unhandled"
            ranAfterException <- true
        }

    require ranBeforeException "synchronous exception did not run"
    require (not ranAfterException) "synchronous exception continued"
    require (not (isNull failed.Exception)) "exception was not attached"

    let caught =
        runtimeTask {
            try
                let! _ = failed
                return false
            with
            | TestException "unhandled" -> return true
        }

    require (resultOf caught) "attached exception was not caught"

    let mutable whileCount = 0

    let whileTask =
        runtimeTask {
            while whileCount < 10 do
                whileCount <- whileCount + 1
                do! Task.Yield()

            return whileCount
        }

    require (resultOf whileTask = 10) "yielding while loop"

    let fixedStack =
        runtimeTask {
            let mutable count = 0

            while count < 100 do
                count <- count + 1
                do! Task.Yield()

            return count
        }

    require (resultOf fixedStack = 100) "fixed stack while loop"

    let fixedFor =
        runtimeTask {
            for _ in Seq.init 100 id do
                do! Task.Yield()
        }

    resultOf fixedFor

    let rec tailLoop count =
        runtimeTask {
            if count < 20 then
                do! Task.Yield()
                let! _ = Task.FromResult()
                return! tailLoop (count + 1)
            else
                return count
        }

    require (resultOf (tailLoop 0) = 20) "tail recursion"

let private checkRemainingTaskCases () =
    let synchronousWhile =
        runtimeTask {
            let mutable count = 0

            while count < 10 do
                count <- count + 1

            return count
        }

    require synchronousWhile.IsCompleted "synchronous while was not completed"
    require (resultOf synchronousWhile = 10) "synchronous while result"

    let mutable nestedCount = 0

    let nestedReturnFrom =
        runtimeTask {
            while nestedCount < 20 do
                do!
                    runtimeTask {
                        do! Task.Yield()
                        let! _ = Task.FromResult()
                        nestedCount <- nestedCount + 1
                        return ()
                    }

            return nestedCount
        }

    require (resultOf nestedReturnFrom = 20) "nested return from"

    let immediateReturnFrom =
        runtimeTask {
            let mutable count = 0

            while count < 100 do
                count <- count + 1
                return! Task.FromResult()
        }

    resultOf immediateReturnFrom

    let mutable finallyExceptionRan = false

    let finallyException =
        runtimeTask {
            try
                do! Task.Delay(1)
            finally
                finallyExceptionRan <- true
                failtest "finally failure"
        }

    try
        resultOf finallyException |> ignore
        failwith "finally exception was lost"
    with
    | TestException "finally failure" -> ()

    require finallyExceptionRan "finally exception did not run"

    let caughtFinally =
        runtimeTask {
            try
                do! Task.Delay(1)
                failtest "caught body failure"
            finally
                finallyExceptionRan <- true
        }

    try
        resultOf caughtFinally |> ignore
        failwith "caught finally exception was lost"
    with
    | TestException "caught body failure" -> ()

    require finallyExceptionRan "caught finally did not run"

    let mutable innerDisposed = false
    let mutable outerDisposed = false

    let useFromTask =
        runtimeTask {
            use! _resource =
                runtimeTask {
                    do! Task.Delay(1)

                    use _inner =
                        { new IDisposable with
                            member _.Dispose() = innerDisposed <- true }

                    return
                        { new IDisposable with
                            member _.Dispose() = outerDisposed <- true }
                }

            require innerDisposed "inner resource was not disposed"
            do! Task.Delay(1)
        }

    resultOf useFromTask
    require outerDisposed "resource returned from task was not disposed"

    let mutable syncContextPosted = false
    let oldContext = SynchronizationContext.Current
    let context =
        { new SynchronizationContext() with
            member _.Post(callback, state) =
                syncContextPosted <- true
                callback.Invoke(state) }

    try
        SynchronizationContext.SetSynchronizationContext context
        let contextTask =
            runtimeTask {
                do! Task.Yield()
            }

        resultOf contextTask
        require syncContextPosted "task did not post to synchronization context"
    finally
        SynchronizationContext.SetSynchronizationContext oldContext

    let genericTaskMethod (task: Task<'T>) =
        runtimeTask {
            let! result = task
            return result
        }

    require (resultOf (genericTaskMethod (Task.FromResult 42)) = 42) "generic task method"

    let yieldMember () : YieldAwaitable = Task.Yield()

    let yieldedValue =
        runtimeTask {
            let! _ = yieldMember ()
            return 42
        }

    require (resultOf yieldedValue = 42) "annotated yield awaitable"

    let valueTaskUnit (task: ValueTask) =
        runtimeTask {
            let! result = task
            return result
        }

    resultOf (valueTaskUnit (ValueTask(Task.Delay(1))))

    let taskUnit (task: Task) =
        runtimeTask {
            let! result = task
            return result
        }

    resultOf (taskUnit (Task.Delay(1)))

    let genericReturn (value: 'T) : Task<'T> =
        runtimeTask {
            do! Task.Yield()
            return value
        }

    require (resultOf (genericReturn "value") = "value") "generic return"

    let genericTransform (value: 'T) (transform: 'T -> 'U) : Task<'U> =
        runtimeTask {
            do! Task.Yield()
            return transform value
        }

    require (resultOf (genericTransform 21 (fun value -> value * 2)) = 42) "generic transformed return"

    let inferredTaskMethod (task: Task<'T>) =
        runtimeTask {
            let! result = task
            return result
        }

    require (resultOf (inferredTaskMethod (Task.FromResult 42)) = 42) "inferred task method"

let private checkExceptionsAndDisposal () =
    let caught =
        runtimeTask {
            try
                do! Task.Delay(1)
                return raise (InvalidOperationException())
            with :? InvalidOperationException ->
                return 42
        }

    require (resultOf caught = 42) "exception handling"

    let failed =
        runtimeTask {
            try
                let! _ = Task.FromException<int>(InvalidOperationException())
                return 0
            with :? InvalidOperationException ->
                return 42
        }

    require (resultOf failed = 42) "awaited exception handling"

    let yieldedCatch =
        runtimeTask {
            try
                do! Task.Yield()
                failtest "yielded failure"
                return 0
            with
            | TestException "yielded failure" -> return 42
        }

    require (resultOf yieldedCatch = 42) "yielded exception handling"

    let applicativeCatch =
        runtimeTask {
            try
                let! _ = delayed 1
                and! _ = Task.FromException<unit>(TestException "applicative failure")
                return 0
            with
            | TestException "applicative failure" -> return 42
        }

    require (resultOf applicativeCatch = 42) "applicative exception handling"

    let nestedCatch =
        runtimeTask {
            try
                try
                    do! Task.Yield()
                    failtest "nested failure"
                    return 0
                with
                | TestException "nested failure" as error -> return raise error
            with
            | TestException "nested failure" -> return 42
        }

    require (resultOf nestedCatch = 42) "nested exception handling"

    let log = ResizeArray()

    let disposed =
        runtimeTask {
            use _resource = new Disposable(log)
            do! Task.Delay(1)
            return 42
        }

    require (resultOf disposed = 42) "using result"
    require (log |> Seq.toList = [ "disposed" ]) "using disposal"

    let asyncLog = ResizeArray()

    let asyncDisposed =
        runtimeTask {
            use _resource = new AsyncDisposable(asyncLog)
            do! Task.Delay(1)
            return 42
        }

    require (resultOf asyncDisposed = 42) "async using result"
    require (asyncLog |> Seq.toList = [ "async-disposed" ]) "async using disposal"

let private checkReturnFromAndAsync () =
    let delayedResult: Task<int> = delayed 42

    let taskResult =
        runtimeTask {
            return! delayedResult
        }

    let asyncResult =
        runtimeTask {
            return! async { return 42 }
        }

    require (resultOf taskResult = 42) "return from task"
    require (resultOf asyncResult = 42) "return from async"

let private checkReturnFromAndMixedAsync () =
    let inner () =
        runtimeTask {
            do! Task.Yield()
            failtest "inner"
            return 1
        }

    let caught =
        runtimeTask {
            try
                do! Task.Yield()
                return! inner ()
            with
            | TestException "inner" -> return 2
        }

    require (resultOf caught = 2) "try over return from"

    let mutable finallyRan = false

    let withFinally =
        runtimeTask {
            try
                do! Task.Yield()
                return! inner ()
            finally
                finallyRan <- true
        }

    try
        resultOf withFinally |> ignore
        failwith "return from exception was lost"
    with
    | TestException "inner" -> ()

    require finallyRan "finally over return from"

    let mixed =
        runtimeTask {
            do! Task.Delay(1)
            do! Async.Sleep(1)

            let! value =
                async {
                    do! Async.Sleep(1)
                    return 5
                }

            return! async { return value + 3 }
        }

    require (resultOf mixed = 8) "mixed async and task"

    let inferred: Task<int> =
        runtimeTask {
            if true then
                return 1
            else
                return! Task.FromResult 2
        }

    require (resultOf inferred = 1) "return from inference"

let private checkTypeInferenceCases () =
    let textTask: Task<string> =
        runtimeTask {
            return "hello"
        }

    let lengthTask: Task<int> =
        runtimeTask {
            let! (text: string) = textTask
            return text.Length
        }

    require (resultOf lengthTask = 5) "task type inference"

    let taskMethod (task: Task<int>) =
        runtimeTask {
            let! result = task
            return result
        }

    require (resultOf (taskMethod (Task.FromResult 42)) = 42) "task argument inference"

    let asyncMethod (value: int) : Async<int> =
        async { return value * 2 }

    let asyncMember =
        runtimeTask {
            let! result = asyncMethod 21
            return result
        }

    require (resultOf asyncMember = 42) "async argument inference"

    let valueTaskMethod (value: int) : ValueTask<int> =
        ValueTask<int>(Task.FromResult value)

    let valueTaskResult =
        runtimeTask {
            let! result = valueTaskMethod 42
            return result
        }

    require (resultOf valueTaskResult = 42) "value task argument inference"

[<EntryPoint>]
let main _ =
    checkBasicBinding()
    checkImmediateAndDelayedExecution()
    checkNestedAndMembers()
    checkTaskBuilderCoreCases()
    checkApplicativeBinding()
    checkAwaitables()
    checkLoops()
    checkTryFinallyAndUsing()
    checkExceptionsAndStackSafety()
    checkRemainingTaskCases()
    checkExceptionsAndDisposal()
    checkReturnFromAndAsync()
    checkReturnFromAndMixedAsync()
    checkTypeInferenceCases()
    0
