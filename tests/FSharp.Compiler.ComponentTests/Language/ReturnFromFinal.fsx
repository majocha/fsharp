printfn $"Current synchronization context: %A{System.Threading.SynchronizationContext.Current}"

let rec t n = backgroundTask {
    if n % 1000 = 0 then
        printfn $"{n} in t, thread: { System.Threading.Thread.CurrentThread.ManagedThreadId }"
        do! System.Threading.Tasks.Task.Yield() // This prevents stack overflow
    if n  = 10 then failwith "boom"
    if n > 0 then
        return! t (n - 1)
    else return 42
}

t 50_000 |> _.Result |> printfn "result: %A"

printfn "done!"

