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

    let mutable taken = false

    let mutable depth = 0

    let unwrapResult = function  Ok v -> v | Error exn -> raise exn

    let castResult = function  Ok (v: objnull) -> Ok (downcast v) | Error (exn: exn) -> Error exn
    
    [<DefaultValue(false)>]
    val mutable lastResult: Result<objnull, exn>

    member val Stack = System.Collections.Generic.Stack<IResumableBox>()

    member this.Delayed (f: unit -> 'T) =
        __stateMachine<_, _>
            (MoveNextMethodImpl<Result<'T, exn>>(fun sm ->
                __resumeAt sm.ResumptionPoint
                match __resumableEntry() with
                | Some contId ->
                    sm.ResumptionPoint <- contId
                | _ ->
                    let currentStack = this.Stack.Count
                    this.lastResult <- try Ok <| (f() :> objnull) with exn -> Error exn
                    let top = this.Stack.Count = currentStack
                    if top then
                        sm.Data <- castResult <| this.lastResult
                        sm.ResumptionPoint <- -1
                    else
                    match __resumableEntry() with
                    | Some contId ->
                        // suspend to let the inner stuff complete
                        sm.ResumptionPoint <- contId
                    | _ ->
                        sm.Data <- this.lastResult |> Result.map (fun x -> downcast x)
                        sm.ResumptionPoint <- -1
            ))

            (SetStateMachineMethodImpl<_>(fun _ _ -> ()))

            (AfterCode<_, _>(fun sm ->
                let mutable sm = sm
                { new IResumableBox<'T> with 
                    member _.Result = GetData(&sm)
                    member _.IsCompleted = IsCompleted(&sm)
                    member _.MoveNext () = MoveNext(&sm)
                    member _.ReplayExceptionIfStored () = GetData(&sm) |> unwrapResult |> ignore
                }
            ))

    member this.Guard(f: unit -> 'T): 'T =

        depth <- depth + 1

        try
            if depth % maxDepth = 0 then

                let box = this.Delayed f
                this.Stack.Push box
                if taken then               
                    Unchecked.defaultof<_>
                else
                    taken <- true
                    while not box.IsCompleted do
                        let top = this.Stack.Peek()
                        if top.IsCompleted then this.Stack.Pop() |> ignore else top.MoveNext()
                    taken <- false
                    unwrapResult box.Result
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