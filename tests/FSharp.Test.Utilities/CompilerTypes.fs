// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace FSharp.Test.Compiler

open FSharp.Compiler.Interactive.Shell
open FSharp.Compiler.Diagnostics
open FSharp.Test
open FSharp.Test.Utilities

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp

open System
open System.IO
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Collections.Immutable

open TestFramework

type CompileOutput =
    | Exe
    | Library
    | Module

type SourceCodeFile =
    {
        FileName: string
        SourceText: string option
    }

/// A source code file
[<RequireQualifiedAccess>]
type SourceCodeFileKind =
    | Fs of SourceCodeFile
    | Fsx of SourceCodeFile
    | Fsi of SourceCodeFile
    | Cs of SourceCodeFile

    static member Create(path:string, ?source: string) =
        match Path.GetExtension(path).ToLowerInvariant() with
        | ".fsi" -> Fsi({FileName=path; SourceText=source})
        | ".fsx" -> Fsx({FileName=path; SourceText=source})
        | ".cs" -> Cs({FileName=path; SourceText=source})
        | ".fs" | _ -> Fs({FileName=path; SourceText=source})

    member this.ChangeExtension =
        match this with
        | Fs s -> Fs({s with FileName=Path.ChangeExtension(s.FileName, ".fs")})
        | Fsx s -> Fsx({s with FileName=Path.ChangeExtension(s.FileName, ".fsx")})
        | Fsi s -> Fsi({s with FileName=Path.ChangeExtension(s.FileName, ".fsi")})
        | Cs s -> Cs({s with FileName=Path.ChangeExtension(s.FileName, ".cs")})

    member this.IsScript =
        match this with
        | Fsx _ -> true
        | _ -> false

    member this.WithFileName (name:string)=
        match this with
        | Fs s -> Fs({s with FileName=name})
        | Fsx s -> Fsx({s with FileName=name})
        | Fsi s -> Fsi({s with FileName=name})
        | Cs s -> Cs({s with FileName=name})

    member this.GetSourceFileName =
        match this with
        | Fs s -> s.FileName
        | Fsx s -> s.FileName
        | Fsi s -> s.FileName
        | Cs s -> s.FileName

    member this.GetSourceText =
        match this with
        | Fs s -> s.SourceText
        | Fsx s -> s.SourceText
        | Fsi s -> s.SourceText
        | Cs s -> s.SourceText


[<AutoOpen>]
type SourceUtilities () =
    static member getCurrentMethodName([<CallerMemberName; Optional; DefaultParameterValue("")>] memberName: string) = memberName

type BaselineFile =
    {
        FilePath: string
        BslSource: string
        Content: string option
    }

type Baseline =
    {
        SourceFilename: string option
        FSBaseline: BaselineFile
        ILBaseline: BaselineFile
    }

type TestType =
    | Text of string
    | Path of string

type CSharpLanguageVersion =
    | CSharp8 = 0
    | CSharp9 = 1
    | CSharp11 = 11
    | CSharp12 = 12
    | Preview = 99
    
module CSharpLanguageVersion =
    /// Converts the given C# language version to a Roslyn language version value.
    let toLanguageVersion lv =
        match lv with
        | CSharpLanguageVersion.CSharp8 -> LanguageVersion.CSharp8
        | CSharpLanguageVersion.CSharp9 -> LanguageVersion.CSharp9
        | CSharpLanguageVersion.CSharp11 -> LanguageVersion.CSharp11
        | CSharpLanguageVersion.CSharp12 -> LanguageVersion.CSharp12
        | CSharpLanguageVersion.Preview -> LanguageVersion.Preview
        | _ -> LanguageVersion.Default

type FSharpCompilationSource =
    { Source:           SourceCodeFileKind
      AdditionalSources:SourceCodeFileKind list
      Baseline:         Baseline option
      Options:          string list
      OutputType:       CompileOutput
      OutputDirectory:  DirectoryInfo option
      Name:             string option
      IgnoreWarnings:   bool
      References:       CompilationUnit list
      TargetFramework:  TargetFramework
      StaticLink:       bool }

    member this.CreateOutputDirectory() =
        match this.OutputDirectory with
        | Some d -> d.Create()
        | None -> ()

    member this.FullName =
        match this.OutputDirectory, this.Name with
        | Some directory, Some name -> Some(Path.Combine(directory.FullName, name))
        | None, _ -> this.Name
        | _ -> None

    member this.OutputFileName =
        match this.FullName, this.OutputType with
        | Some fullName, CompileOutput.Library -> Some (Path.ChangeExtension(fullName, ".dll"))
        | Some fullName, CompileOutput.Exe -> Some (Path.ChangeExtension(fullName, ".exe"))
        | _ -> None

    override this.ToString() = match this.Name with | Some n -> n | _ -> (sprintf "%A" this)

and CompilationUnit =
    | FS of FSharpCompilationSource
    | CS of CSharpCompilationSource
    | IL of ILCompilationSource
    override this.ToString() = match this with | FS fs -> fs.ToString() | _ -> (sprintf "%A" this   )
    member this.OutputDirectory =
        let toString diOpt =
            match diOpt: DirectoryInfo option with
            | Some di -> di.FullName
            | None -> ""
        match this with
        | FS fs -> fs.OutputDirectory |> toString
        | CS cs -> cs.OutputDirectory |> toString
        | _ -> raise (Exception "Not supported for this compilation type")
    member this.WithStaticLink(staticLink: bool) = match this with | FS fs -> FS { fs with StaticLink = staticLink } | cu -> cu

and CSharpCompilationSource =
    {
        Source:          SourceCodeFileKind
        LangVersion:     CSharpLanguageVersion
        TargetFramework: TargetFramework
        OutputType:      CompileOutput
        OutputDirectory: DirectoryInfo option
        Name:            string option
        References:      CompilationUnit list
    }

and ILCompilationSource =
    {
        Source:     TestType
        References: CompilationUnit list
    }

type ErrorType = Error of int | Warning of int | Information of int | Hidden of int

type SymbolType =
    | MemberOrFunctionOrValue of string
    | Entity of string
    | GenericParameter of string
    | Parameter of string
    | StaticParameter of string
    | ActivePatternCase of string
    | UnionCase of string
    | Field of string

    member this.FullName () =
        match this with
        | MemberOrFunctionOrValue fullname
        | Entity fullname
        | GenericParameter fullname
        | Parameter fullname
        | StaticParameter fullname
        | ActivePatternCase fullname
        | UnionCase fullname
        | Field fullname -> fullname

type Line = Line of int
type Col = Col of int

type Range =
  { StartLine:   int
    StartColumn: int
    EndLine:     int
    EndColumn:   int }

type Disposable (dispose : unit -> unit) =
    interface IDisposable with
        member this.Dispose() =
            dispose()

type ErrorInfo =
  { Error:   ErrorType
    Range:   Range
    NativeRange : FSharp.Compiler.Text.range
    Message: string
    SubCategory: string }

type ExecutionOutput =
  { ExitCode: int
    StdOut:   string
    StdErr:   string }

type EvalOutput =
  { Result: Result<FsiValue option, exn>
    StdOut: string
    StdErr: string }

type RunOutput =
    | EvalOutput of EvalOutput
    | ExecutionOutput of ExecutionOutput

type SourceCodeFileName = string

type CompilationOutput =
  { OutputPath:    string option
    Dependencies:  string list
    Adjust:        int
    Diagnostics:   ErrorInfo list
    PerFileErrors: (SourceCodeFileName * ErrorInfo) list
    Output:        RunOutput option
    Compilation:   CompilationUnit }

[<RequireQualifiedAccess>]
type CompilationResult =
    | Success of CompilationOutput
    | Failure of CompilationOutput
    with
        member this.Output = match this with Success o | Failure o -> o
        member this.RunOutput = this.Output.Output

type ExecutionPlatform =
    | Anycpu = 0
    | AnyCpu32bitPreferred = 1
    | X86 = 2
    | Itanium = 3
    | X64 = 4
    | Arm = 5
    | Arm64 = 6

[<Sealed>]
type ILVerifier (dllFilePath: string) =

    member _.VerifyIL (expectedIL: string list) =
        ILChecker.checkIL dllFilePath expectedIL

[<Sealed>]
type PdbDebugInfo(debugInfo: string) =

    member _.InfoText = debugInfo

type RoslynLanguageVersion = LanguageVersion

[<Flags>]
type CSharpCompilationFlags =
    | None = 0x0
    | InternalsVisibleTo = 0x1

[<RequireQualifiedAccess>]
type TestCompilation =
    | CSharp of CSharpCompilation
    | IL of ilSource: string * result: Lazy<string * byte []>

    member this.AssertNoErrorsOrWarnings () =
        match this with
            | TestCompilation.CSharp c ->
                let diagnostics = c.GetDiagnostics ()

                if not diagnostics.IsEmpty then
                    failwith ("CSharp source diagnostics:\n" + (diagnostics |> Seq.map (fun x -> x.GetMessage () + "\n") |> Seq.reduce (+)))

            | TestCompilation.IL (_, result) ->
                let errors, _ = result.Value
                if errors.Length > 0 then
                    failwith ("IL source errors: " + errors)

    member this.EmitAsFile (outputPath: string) =
        match this with
            | TestCompilation.CSharp c ->
                let c = c.WithAssemblyName(Path.GetFileNameWithoutExtension outputPath)
                let emitResult = c.Emit outputPath
                if not emitResult.Success then
                    failwithf "Unable to emit C# compilation.\n%A" emitResult.Diagnostics

            | TestCompilation.IL (_, result) ->
                let (_, data) = result.Value
                File.WriteAllBytes (outputPath, data)

[<AbstractClass; Sealed>]
type CompilationUtil private () =

    static let createCSharpCompilation (source: SourceCodeFileKind, lv, tf, additionalReferences, name) =
        let lv = CSharpLanguageVersion.toLanguageVersion lv
        let tf = defaultArg tf TargetFramework.NetStandard20
        let source =
            match source.GetSourceText with
            | Some text ->
                // In memory source file copy it to the build directory
                text
            | None ->
                // On Disk file
                File.ReadAllText(source.GetSourceFileName)
        let name = defaultArg name (Guid.NewGuid().ToString ())
        let additionalReferences = defaultArg additionalReferences ImmutableArray<PortableExecutableReference>.Empty
        let references = TargetFrameworkUtil.getReferences tf
        let c =
            CSharpCompilation.Create(
                name,
                [ CSharpSyntaxTree.ParseText (source, CSharpParseOptions lv) ],
                references.AddRange(additionalReferences).As<MetadataReference>(),
                CSharpCompilationOptions (OutputKind.DynamicallyLinkedLibrary))
        TestCompilation.CSharp c

    static member CreateCSharpCompilation (source:SourceCodeFileKind, lv, ?tf, ?additionalReferences, ?name) =
        createCSharpCompilation (source, lv, tf, additionalReferences, name)

    static member CreateCSharpCompilation (source:string, lv, ?tf, ?additionalReferences, ?name) =
        createCSharpCompilation (SourceCodeFileKind.Create("test.cs", source), lv, tf, additionalReferences, name)

    static member CreateILCompilation (source: string) =
        let compute =
            lazy
                let ilFilePath = getTemporaryFileName() + ".il"
                let dllFilePath = Path.ChangeExtension (ilFilePath, ".dll")
                try
                    File.WriteAllText (ilFilePath, source)
                    let errors = ILChecker.reassembleIL ilFilePath dllFilePath
                    try
                        (errors, File.ReadAllBytes dllFilePath)
                    with
                        | _ -> (errors, [||])
                finally
                    try Directory.Delete(Path.GetDirectoryName ilFilePath, true) with _ -> ()
        TestCompilation.IL (source, compute)

and CompilationReference =
    internal
    | CompilationReference of Compilation * staticLink: bool
    | TestCompilationReference of TestCompilation

    static member CreateFSharp(cmpl: Compilation, ?staticLink) =
        let staticLink = defaultArg staticLink false
        CompilationReference(cmpl, staticLink)

    static member Create(cmpl: TestCompilation) =
        TestCompilationReference cmpl

and Compilation =
    internal
    | Compilation of
        sources: SourceCodeFileKind list *
        outputType: CompileOutput *
        options: string[] *
        targetFramework: TargetFramework *
        CompilationReference list *
        name: string option *
        outputDirectory: DirectoryInfo option with

        static member Create(source:SourceCodeFileKind, output:CompileOutput, ?options:string array, ?targetFramework:TargetFramework, ?cmplRefs:CompilationReference list, ?name:string, ?outputDirectory: DirectoryInfo) =
            let options = defaultArg options [||]
            let targetFramework = defaultArg targetFramework TargetFramework.Current
            let cmplRefs = defaultArg cmplRefs []
            let name =
                match defaultArg name null with
                | null -> None
                | n -> Some n
            Compilation([source], output, options, targetFramework, cmplRefs, name, outputDirectory)

        static member Create(source:string, output:CompileOutput, ?options:string array, ?targetFramework:TargetFramework, ?cmplRefs:CompilationReference list, ?name:string, ?outputDirectory: DirectoryInfo) =
            let options = defaultArg options [||]
            let targetFramework = defaultArg targetFramework TargetFramework.Current
            let cmplRefs = defaultArg cmplRefs []
            let name =
                match defaultArg name null with
                | null -> None
                | n -> Some n
            Compilation([SourceCodeFileKind.Create("test.fs", source)], output, options, targetFramework, cmplRefs, name, outputDirectory)

        static member Create(fileName:string, source:string, output, ?options, ?targetFramework:TargetFramework, ?cmplRefs, ?name, ?outputDirectory: DirectoryInfo) =
            let source = SourceCodeFileKind.Create(fileName, source)
            let options = defaultArg options [||]
            let targetFramework = defaultArg targetFramework TargetFramework.Current
            let cmplRefs = defaultArg cmplRefs []
            let name = defaultArg name null
            let outputDirectory = defaultArg outputDirectory null
            Compilation.Create(source, output, options, targetFramework, cmplRefs, name, outputDirectory)

        static member CreateFromSources(sources, output, ?options, ?targetFramework, ?cmplRefs, ?name, ?outputDirectory: DirectoryInfo) =
            let options = defaultArg options [||]
            let targetFramework = defaultArg targetFramework TargetFramework.Current
            let cmplRefs = defaultArg cmplRefs []
            let name =
                match defaultArg name null with
                | null -> None
                | n -> Some n
            Compilation(sources, output, options, targetFramework, cmplRefs, name, outputDirectory)