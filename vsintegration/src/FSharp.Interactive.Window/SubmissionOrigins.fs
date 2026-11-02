// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.Interactive

open System

[<Sealed>]
type internal SubmissionOrigins() =

    let waiting = ResizeArray<struct (string * string * int * bool)>()

    static let normalized (text: string) = text.Replace("\r\n", "\n").Trim()

    member _.Add(text: string, sourcePath: string, startLine: int, debug: bool) =
        lock waiting (fun () -> waiting.Add(struct (normalized text, sourcePath, startLine, debug)))

    /// Drops the origins of submissions sent before this one that never ran.
    member _.Take(text: string) : struct (string * int * bool) voption =
        let key = normalized text

        lock waiting (fun () ->
            match waiting.FindIndex(fun (struct (queued, _, _, _)) -> String.Equals(queued, key, StringComparison.Ordinal)) with
            | -1 -> ValueNone
            | index ->
                let struct (_, sourcePath, startLine, debug) = waiting[index]
                waiting.RemoveRange(0, index + 1)
                ValueSome(struct (sourcePath, startLine, debug)))
