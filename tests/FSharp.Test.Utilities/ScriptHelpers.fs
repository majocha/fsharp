// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace FSharp.Test.ScriptHelpers

open System
open System.IO
open System.Text
open System.Threading
open FSharp.Compiler
open FSharp.Compiler.Interactive.Shell
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.EditorServices
open FSharp.Test

[<RequireQualifiedAccess>]
type LangVersion =
    | V47
    | V50
    | V60
    | V70
    | V80
    | V90
    | Preview
    | Latest
    | SupportsMl
    member this.ToOption =
        match this with
        | V47 -> "4.7"
        | V50 | SupportsMl -> "5.0"
        | V60 -> "6.0"
        | V70 -> "7.0"
        | V80 -> "8.0"
        | V90 -> "9.0"
        | Preview -> "preview"
        | Latest -> "latest"

type FSharpScript(?additionalArgs: string[], ?langVersion: LangVersion) =

    let additionalArgs = defaultArg additionalArgs [||]
    let langVersion = defaultArg langVersion LangVersion.Preview
    let config = FsiEvaluationSession.GetDefaultConfiguration()
    let langVersionSpecifiedInOptions =
        additionalArgs |> Array.exists (fun arg -> arg.StartsWith("--langversion:"))

    let computedProfile =
        // If we are being executed on the desktop framework (we can tell because the assembly containing int is mscorlib) then profile must be mscorlib otherwise use netcore
        if typeof<int>.Assembly.GetName().Name = "mscorlib" then "mscorlib"
        else "netcore"

    let baseArgs = [|
        //typeof<FSharpScript>.Assembly.Location;
        "--targetprofile:" + computedProfile
        if not langVersionSpecifiedInOptions then "--langversion:" + langVersion.ToOption
        |]

    let argv = Array.append baseArgs additionalArgs

    let fsi = FsiEvaluationSession.Create (config, argv, stdin, stdout, stderr)

    member _.ValueBound = fsi.ValueBound

    member _.Fsi = fsi

    member this.Eval(code: string, ?cancellationToken: CancellationToken) =
        let cancellationToken = defaultArg cancellationToken CancellationToken.None
        let ch, errors =
            // lock, because For memory conservation in CI FSharpScripts may be reused between tests
            lock fsi <| fun () ->
                fsi.EvalInteractionNonThrowing(code, cancellationToken)

        match ch with
        | Choice1Of2 v -> Ok(v), errors
        | Choice2Of2 ex -> Error(ex), errors

    /// Get the available completion items from the code at the specified location.
    ///
    /// <param name="text">The input text on which completions will be calculated</param>
    /// <param name="line">The 1-based line index</param>
    /// <param name="column">The 0-based column index</param>
    member _.GetCompletionItems(text: string, line: int, column: int) =
        async {
            let parseResults, checkResults, _projectResults = fsi.ParseAndCheckInteraction(text)
            let lineText = text.Split('\n').[line - 1]
            let partialName = QuickParse.GetPartialLongNameEx(lineText, column - 1)
            let declarationListInfos = checkResults.GetDeclarationListInfo(Some parseResults, line, lineText, partialName)
            return declarationListInfos.Items
        }

    interface IDisposable with
        member this.Dispose() =
            ((this.Fsi) :> IDisposable).Dispose()

[<AutoOpen>]
module TestHelpers =

    let getValue ((value: Result<FsiValue option, exn>), (errors: FSharpDiagnostic[])) =
        if errors.Length > 0 then
            failwith <| sprintf "Evaluation returned %d errors:\r\n\t%s" errors.Length (String.Join("\r\n\t", errors))
        match value with
        | Ok(value) -> value
        | Error ex -> raise ex

    let ignoreValue = getValue >> ignore
