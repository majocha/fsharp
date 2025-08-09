open System.Threading

printfn $"Current synchronization context: %A{SynchronizationContext.Current}"

let rec t n = backgroundTask {
    if n % 1 = 0 then
        printfn $"{n} in t, thread: { Thread.CurrentThread.ManagedThreadId }"
        do! Tasks.Task.Yield() // This prevents stack overflow
    if n  = 10 then failwith "boom"
    if n > 45 then
        return! t (n - 1)
}

t 50 |> _.Result |> printfn "result: %A"

printfn "done!"


