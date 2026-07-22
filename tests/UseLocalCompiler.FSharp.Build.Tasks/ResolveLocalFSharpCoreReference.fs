namespace UseLocalCompiler.FSharp.Build.Tasks

open System
open System.IO
open Microsoft.Build.Framework
open Microsoft.Build.Utilities

type public ResolveLocalFSharpCoreReference() =
    inherit Task()

    let mutable localFSharpCompilerPath = ""
    let mutable localFSharpCompilerConfiguration = ""
    let mutable targetFramework = ""
    let mutable targetFrameworkIdentifier = ""
    let mutable targetFrameworkVersion = ""
    let mutable resolvedFSharpCorePath = ""

    static member private tryParseVersionFromTargetFramework(targetFramework: string) =
        if String.IsNullOrWhiteSpace targetFramework then
            None
        elif targetFramework.StartsWith("netstandard", StringComparison.OrdinalIgnoreCase) then
            Some(".NETStandard", targetFramework.Substring("netstandard".Length))
        elif targetFramework.StartsWith("netcoreapp", StringComparison.OrdinalIgnoreCase) then
            Some(".NETCoreApp", targetFramework.Substring("netcoreapp".Length))
        elif targetFramework.StartsWith("net", StringComparison.OrdinalIgnoreCase) then
            let suffix = targetFramework.Substring("net".Length)

            if suffix.Length >= 2 && Char.IsDigit suffix[0] && Char.IsDigit suffix[1] && not (suffix.Contains(".")) then
                Some(".NETFramework", sprintf "%c.%c" suffix[0] suffix[1])
            elif suffix.Length > 0 && Char.IsDigit suffix[0] then
                Some(".NETCoreApp", suffix)
            else
                None
        else
            None

    static member private tryNormalizeVersion(versionText: string) =
        if String.IsNullOrWhiteSpace versionText then
            None
        else
            let trimmed =
                if versionText.StartsWith("v", StringComparison.OrdinalIgnoreCase) then
                    versionText.Substring(1)
                else
                    versionText

            match Version.TryParse(trimmed) with
            | true, version -> Some version
            | _ -> None

    static member private shouldUseNetStandard20(identifier: string, version: Version option) =
        match identifier, version with
        | ".NETFramework", _ -> true
        | ".NETStandard", Some version -> version < Version(2, 1)
        | ".NETCoreApp", Some version -> version < Version(3, 0)
        | _ -> false

    [<Required>]
    member _.LocalFSharpCompilerPath
        with get () = localFSharpCompilerPath
        and set value = localFSharpCompilerPath <- value

    [<Required>]
    member _.LocalFSharpCompilerConfiguration
        with get () = localFSharpCompilerConfiguration
        and set value = localFSharpCompilerConfiguration <- value

    member _.TargetFramework
        with get () = targetFramework
        and set value = targetFramework <- value

    member _.TargetFrameworkIdentifier
        with get () = targetFrameworkIdentifier
        and set value = targetFrameworkIdentifier <- value

    member _.TargetFrameworkVersion
        with get () = targetFrameworkVersion
        and set value = targetFrameworkVersion <- value

    [<Output>]
    member _.ResolvedFSharpCorePath
        with get () = resolvedFSharpCorePath
        and set value = resolvedFSharpCorePath <- value

    override _.Execute() =
        let identifier, version =
            if String.IsNullOrWhiteSpace targetFrameworkIdentifier then
                match ResolveLocalFSharpCoreReference.tryParseVersionFromTargetFramework targetFramework with
                | Some(identifier, versionText) -> identifier, ResolveLocalFSharpCoreReference.tryNormalizeVersion versionText
                | None -> ".NETCoreApp", None
            else
                targetFrameworkIdentifier, ResolveLocalFSharpCoreReference.tryNormalizeVersion targetFrameworkVersion

        let localFSharpCoreTargetFramework =
            if ResolveLocalFSharpCoreReference.shouldUseNetStandard20(identifier, version) then
                "netstandard2.0"
            else
                "netstandard2.1"

        resolvedFSharpCorePath <-
            Path.Combine(
                localFSharpCompilerPath,
                "artifacts",
                "bin",
                "FSharp.Core",
                localFSharpCompilerConfiguration,
                localFSharpCoreTargetFramework,
                "FSharp.Core.dll"
            )

        true
