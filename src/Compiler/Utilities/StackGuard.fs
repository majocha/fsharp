namespace Internal.Utilities.Library

open System.Runtime.CompilerServices
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Core.CompilerServices.StateMachineHelpers
open Extras
open System.Diagnostics

type internal IResumableBox =
    abstract member MoveNext: unit -> unit
    abstract IsCompleted: bool
    abstract ReplayExceptionIfStored: unit -> unit

/// Helpers to do zero-allocation call to interface methods on structs
[<AutoOpen>]
module internal Helpers =
    let inline MoveNext(x: byref<'T> when 'T :> IAsyncStateMachine) = x.MoveNext()
    let inline GetData(x: byref<'T> when 'T :> IResumableStateMachine<'Data>) = x.Data
    let inline IsCompleted(x: byref<'T> when 'T :> IResumableStateMachine<'Data>) = x.ResumptionPoint = -1

type internal IResumableBox<'T> =
    inherit IResumableBox
    abstract Result: Result<'T, exn>

/// Guard against depth of expression nesting, by moving to new stack when a maximum depth is reached
type internal StackGuard(maxDepth: int, name: string) =

    do ignore name 

    let mutable isRunning = false

    let mutable depth = 0
    let mutable syncDepth = 0

    let pending = System.Collections.Generic.Stack<_>()

    let run () =
        isRunning <- true
        try 
            while pending.Count > 0 do pending.Pop() ()
        finally isRunning <- false

    member _.GuardSync(f: unit -> unit): unit =

        syncDepth <- syncDepth + 1

        try
            if syncDepth % maxDepth = 0 then

                pending.Push (f)
                if not isRunning then run () else Unchecked.defaultof<_>

            else
                f()
        finally
            syncDepth <- syncDepth - 1

    member _.Guard(f: unit -> 'T): 'T =

        depth <- depth + 1

        try
            if depth % maxDepth = 0 then
                System.Threading.Tasks.Task.Run(f).Result
            else
                f()
        finally
            depth <- depth - 1

    static member val DefaultDepth =
        #if DEBUG
        GetEnvInteger "FSHARP_DefaultStackGuardDepth" 50
        #else
        GetEnvInteger "FSHARP_DefaultStackGuardDepth" 100
        #endif

    static member GetDepthOption(name: string) =
        GetEnvInteger ("FSHARP_" + name + "StackGuardDepth") StackGuard.DefaultDepth

    [<DebuggerHidden; DebuggerStepThrough>]
    member x.GuardCancellable(original: Cancellable<'T>) =
        Cancellable(fun ct -> x.Guard(fun () -> Cancellable.run ct original))