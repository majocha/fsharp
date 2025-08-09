open System.Threading
open System.Threading.Tasks

printfn $"Current synchronization context: %A{SynchronizationContext.Current}"

//let t _ = task {
//    do! Task.Yield()
//    return! task {
//        do! Task.Yield()
//        return 42  }
//}

let rec t n = backgroundTask {
    if n % 1000 = 0 then
        printfn $"{n} in t, thread: { Thread.CurrentThread.ManagedThreadId }"
        do! Tasks.Task.Yield() // This prevents stack overflow
    //if n  = 10 then failwith "boom"
    if n > 0 then
        return! t (n - 1)
}

t 100_000 |> _.Result |> printfn "result: %A"

printfn "done!"


