open System.Threading.Tasks

let rec t n = task {
    if n % 1000 = 0 then
        printfn "in t %d" n
        do! System.Threading.Tasks.Task.Delay(0)
    if n > 0 then
        return! t (n - 1)
    else return 42
}

// Fail after one second
// task {
//     do! Task.Delay(1000)
//     exit 1
// }

let x = t 10_000

printfn $"done, {x.Result}"
