// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.Interactive

open System.Threading.Tasks

open EnvDTE
open EnvDTE80
open Microsoft.VisualStudio.Shell

module internal InteractiveDebugger =

    /// JSON-RPC sessions run on .NET. Left to choose, the debugger can pick an engine under which
    /// Debugger.IsAttached stays false, and the break a debug submission starts with is skipped.
    [<Literal>]
    let private CoreClrEngine = "{2E36F1D4-B23C-435D-AB41-18E608940038}"

    let private tryFindProcess (processId: int) (processes: Processes | null) =
        match processes with
        | null -> None
        | processes -> processes |> Seq.cast<Process> |> Seq.tryFind (fun p -> p.ProcessID = processId)

    let private tryGetDebugger () =
        ThreadHelper.ThrowIfNotOnUIThread()

        match ServiceProvider.GlobalProvider.GetService typeof<DTE> with
        | :? DTE as dte -> ValueSome dte.Debugger
        | _ -> ValueNone

    /// Debugging the session is fine; debugging anything else leaves no way to step into a submission.
    let isDebuggingElsewhere (sessionProcessId: int voption) =
        match tryGetDebugger () |> ValueOption.map _.DebuggedProcesses with
        | ValueNone -> false
        | ValueSome(processes: Processes | null) ->
            match processes, sessionProcessId with
            | null, _ -> false
            | processes, _ when processes.Count = 0 -> false
            | processes, ValueSome processId -> tryFindProcess processId processes |> Option.isNone
            | _, ValueNone -> true

    /// Does nothing when the debugger is already attached to the process.
    let attachAsync (processId: int) : Task<Result<unit, string>> =
        task {
            do! ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync()

            try
                match tryGetDebugger () with
                | ValueNone -> return Result.Error "The debugger is not available."
                | ValueSome debugger ->
                    if tryFindProcess processId debugger.DebuggedProcesses |> Option.isSome then
                        return Result.Ok()
                    else
                        match tryFindProcess processId debugger.LocalProcesses with
                        | Some(:? Process2 as fsi) ->
                            fsi.Attach2 CoreClrEngine
                            return Result.Ok()
                        | _ -> return Result.Error $"Process {processId} was not found to attach the debugger to."
            with e ->
                return Result.Error e.Message
        }
