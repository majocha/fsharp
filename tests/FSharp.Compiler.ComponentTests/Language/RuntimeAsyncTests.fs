module Language.RuntimeAsyncTests

open Xunit
open FSharp.Test.Compiler

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
    |> withErrorCode 3351

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
