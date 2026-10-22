// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

module internal FSharp.Compiler.Debugging.ExpressionEvaluation

open System
open System.IO
open Internal.Utilities.Library
open FSharp.Compiler
open FSharp.Compiler.AbstractIL.IL
open FSharp.Compiler.AbstractIL.ILBinaryReader
open FSharp.Compiler.AbstractIL.ILBinaryWriter
open FSharp.Compiler.CheckBasics
open FSharp.Compiler.CheckDeclarations
open FSharp.Compiler.CheckExpressions
open FSharp.Compiler.CheckExpressionsOps
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.CompilerConfig
open FSharp.Compiler.CompilerDiagnostics
open FSharp.Compiler.CompilerImports
open FSharp.Compiler.DependencyManager
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.DiagnosticsLogger
open FSharp.Compiler.IlxGen
open FSharp.Compiler.Lexhelp
open FSharp.Compiler.NameResolution
open FSharp.Compiler.OptimizeInputs
open FSharp.Compiler.ParseAndCheckInputs
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.Text.Range
open FSharp.Compiler.TypedTree
open FSharp.Compiler.TypedTreeBasics
open FSharp.Compiler.TypedTreeOps

type FrameContext =
    {
        AssemblyName: string
        MethodToken: int
        Opens: string list
    }

type CompiledExpression =
    {
        Assembly: byte[]
        TypeName: string
        MethodName: string
        ResultType: string
    }

type EvaluationDiagnostic =
    {
        Number: int
        Message: string
        IsError: bool
    }

/// Sequences the compilation phases, stopping at the first that reports errors.
type private ResultBuilder() =
    member _.Bind(r: Result<'T, 'E>, f: 'T -> Result<'U, 'E>) = Result.bind f r
    member _.Return x : Result<'T, 'E> = Ok x
    member _.Zero() : Result<unit, 'E> = Ok()
    member _.Combine(r: Result<unit, 'E>, rest: Result<'T, 'E>) = Result.bind (fun () -> rest) r
    member _.Delay f = f ()

let private result = ResultBuilder()

/// Names of the synthesized query module and method (as Roslyn's `<>x.<>m0`).
[<Literal>]
let private QueryTypeName = "<>x"

[<Literal>]
let private QueryMethodName = "<>m0"

let private queryFileName = "<>x.fs"

let private toDiagnostic (d: PhasedDiagnostic) =
    {
        Number = d.Number
        Message = d.FormatCore(false, false)
        IsError = (d.Severity = FSharpDiagnosticSeverity.Error)
    }

/// Runs `f` with a capturing diagnostics logger; fails with the captured errors, if any.
let private withDiagnostics (phase: BuildPhase) (f: unit -> 'T) : Result<'T, EvaluationDiagnostic list> =
    let logger = CapturingDiagnosticsLogger("ExpressionEvaluation")
    use _ = UseBuildPhase phase
    use _ = UseDiagnosticsLogger logger

    let value =
        try
            Some(f ())
        with
        | ReportedError _ -> None
        | e ->
            errorRecovery e range0
            None

    match value with
    | Some value when logger.ErrorCount = 0 -> Result.Ok value
    | _ -> Result.Error(logger.Diagnostics |> List.map toDiagnostic |> List.filter _.IsError)

/// Finds a method definition by token, with the chain of type definitions enclosing it.
let private tryFindMethod (ilModule: ILModuleDef) (token: int) =
    let rid = token &&& 0x00FFFFFF

    let rec search (enclosing: ILTypeDef list) (typeDefs: ILTypeDef list) =
        typeDefs
        |> List.tryPick (fun td ->
            let path = enclosing @ [ td ]

            match td.Methods.AsList() |> List.tryFind (fun md -> md.MetadataIndex = rid) with
            | Some md -> Some(path, md)
            | None -> search path (td.NestedTypes.AsList()))

    search [] (ilModule.TypeDefs.AsList())

/// The F# entities corresponding to a chain of IL type definitions, outermost first, as far as
/// they exist in the F# view of the assembly (closures and other generated types do not).
let private fsharpEntities (ccu: CcuThunk) (enclosing: ILTypeDef list) =
    match enclosing with
    | [] -> []
    | outer :: nested ->
        let ns, name = splitILTypeName outer.Name

        let rec namespaceEntity (modul: ModuleOrNamespace) path =
            match path with
            | [] -> Some modul
            | part :: rest ->
                match modul.ModuleOrNamespaceType.ModulesAndNamespacesByDemangledName.TryFind part with
                | Some child -> namespaceEntity child rest
                | None -> None

        match namespaceEntity ccu.Contents ns with
        | None -> []
        | Some nsEntity ->
            let rec types (parent: Entity) (names: string list) acc =
                match names with
                | [] -> List.rev acc
                | compiledName :: rest ->
                    match parent.ModuleOrNamespaceType.AllEntitiesByCompiledAndLogicalMangledNames.TryFind compiledName with
                    | Some e -> types e rest (e :: acc)
                    | None -> List.rev acc

            types nsEntity (name :: (nested |> List.map _.Name)) []

/// The path of a module or type as written in F# source, e.g. `global.TestDebuggee.Program`.
let private sourcePath (e: Entity) =
    let parents =
        e.CompilationPath.MangledPath
        |> List.map PrettyNaming.NormalizeIdentifierBackticks

    String.Join(
        ".",
        "global" :: parents
        @ [ PrettyNaming.NormalizeIdentifierBackticks e.DisplayName ]
    )

/// The source of the query module: opens, then `let <>m0 () = (expression)`.
let private querySource (opens: string list) (text: string) =
    let indented =
        text.Replace("\r\n", "\n").Split('\n')
        |> Array.map (fun line -> "        " + line)
        |> String.concat "\n"

    let openLines = opens |> List.map (fun o -> $"open {o}") |> String.concat "\n"
    $"module ``{QueryTypeName}``\n{openLines}\nlet ``{QueryMethodName}`` () =\n    (\n{indented}\n    )\n"

/// Gives `<>m0` the frame's `this` and parameters as its own parameters: the type checker saw them
/// as free locals, so after this rewrite IlxGen stores them as method arguments.
let private bindFrameValues g (frameVals: Val list) (contents: ModuleOrNamespaceContents) =
    let rewriteBinding (TBind(v, expr, sp) as bind) =
        if v.LogicalName <> QueryMethodName || frameVals.IsEmpty then
            bind
        else
            match stripDebugPoints expr with
            | Expr.Lambda(_, _, _, [ _unitArg ], body, m, bodyTy) ->
                let lambda =
                    mkMultiLambdas g m [] (frameVals |> List.map List.singleton) (body, bodyTy)

                let args = frameVals |> List.map (fun _ -> [ ValReprInfo.unnamedTopArg1 ])
                v.SetValReprInfo(Some(ValReprInfo([], args, ValReprInfo.unnamedRetVal)))
                v.SetType(tyOfExpr g lambda)
                TBind(v, lambda, sp)
            | _ -> bind

    let rec rewrite contents =
        match contents with
        | TMDefRec(isRec, opens, tycons, bindings, m) ->
            let bindings =
                bindings
                |> List.map (function
                    | ModuleOrNamespaceBinding.Binding bind -> ModuleOrNamespaceBinding.Binding(rewriteBinding bind)
                    | ModuleOrNamespaceBinding.Module(mspec, def) -> ModuleOrNamespaceBinding.Module(mspec, rewrite def))

            TMDefRec(isRec, opens, tycons, bindings, m)
        | TMDefLet(bind, m) -> TMDefLet(rewriteBinding bind, m)
        | TMDefs defs -> TMDefs(defs |> List.map rewrite)
        | TMDefDo _
        | TMDefOpens _ -> contents

    rewrite contents

/// `IgnoresAccessChecksToAttribute` (defined in the query assembly, as the runtime expects) applied
/// for the debuggee's assemblies. The debugger's IL interpreter ignores accessibility; this lets
/// CoreCLR do the same when the query assembly runs as ordinary code (func-eval of lambdas, tests).
let private ignoresAccessChecksTo (g: TcGlobals.TcGlobals) (assemblyNames: string list) =
    let name = "System.Runtime.CompilerServices.IgnoresAccessChecksToAttribute"
    let typeRef = mkILTyRef (ILScopeRef.Local, name)
    let ty = mkILNonGenericBoxedTy typeRef

    let ctor =
        mkILSimpleStorageCtor (
            Some g.ilg.typ_Attribute.TypeSpec,
            ty,
            [ mkILParamNamed ("assemblyName", g.ilg.typ_String) ],
            [],
            ILMemberAccess.Public,
            None,
            None
        )

    let typeDef =
        (mkILSimpleClass
            g.ilg
            (name,
             ILTypeDefAccess.Public,
             mkILMethods [ ctor ],
             emptyILFields,
             emptyILTypeDefs,
             emptyILProperties,
             emptyILEvents,
             emptyILCustomAttrs,
             ILTypeInit.BeforeField))
            .With(extends = notlazy (Some g.ilg.typ_Attribute))

    let attributes =
        assemblyNames
        |> List.map (fun n -> mkILCustomAttribute (typeRef, [ g.ilg.typ_String ], [ ILAttribElem.String(Some n) ], []))

    typeDef, attributes

[<Sealed>]
type EvaluationSession(tcConfig: TcConfig, tcGlobals: TcGlobals.TcGlobals, tcImports: TcImports) =

    let ctok = AssumeCompilationThreadWithoutEvidence()
    let lexResourceManager = LexResourceManager()
    let amap = tcImports.GetImportMap()

    static member Create(references: string list) =
        let fsharpCore =
            references
            |> List.tryFind (fun r -> String.Equals(Path.GetFileName r, "FSharp.Core.dll", StringComparison.OrdinalIgnoreCase))

        let binariesDir =
            match fsharpCore with
            | Some path ->
                Path.GetDirectoryName path
                |> Option.ofObj
                |> Option.defaultValue AppContext.BaseDirectory
            | None -> AppContext.BaseDirectory

        let tcConfigB =
            TcConfigBuilder.CreateNew(
                SimulatedMSBuildReferenceResolver.getResolver (),
                defaultFSharpBinariesDir = binariesDir,
                reduceMemoryUsage = ReduceMemoryFlag.Yes,
                implicitIncludeDir = Directory.GetCurrentDirectory(),
                isInteractive = false,
                isInvalidationSupported = false,
                defaultCopyFSharpCore = CopyFSharpCoreFlag.No,
                tryGetMetadataSnapshot = (fun _ -> None),
                sdkDirOverride = None,
                rangeForErrors = range0
            )

        // Compile against exactly what the debuggee has loaded: implementation assemblies, no SDK packs.
        tcConfigB.implicitlyReferenceDotNetAssemblies <- false
        tcConfigB.implicitlyResolveAssemblies <- false
        tcConfigB.SetUseSdkRefs false
        tcConfigB.primaryAssembly <- PrimaryAssembly.System_Runtime
        tcConfigB.target <- CompilerTarget.Dll
        tcConfigB.debuginfo <- false
        tcConfigB.emitTailcalls <- false

        tcConfigB.optSettings <-
            { tcConfigB.optSettings with
                jitOptUser = Some false
                localOptUser = Some false
            }

        for r in references do
            tcConfigB.AddReferencedAssemblyByPath(range0, r)

        let tcConfig = TcConfig.Create(tcConfigB, validate = false)
        let tcConfigP = TcConfigProvider.Constant tcConfig

        let tcGlobals, tcImports =
            TcImports.BuildTcImports(tcConfigP, new DependencyProvider())
            |> Async.RunSynchronouslyImmediate

        EvaluationSession(tcConfig, tcGlobals, tcImports)

    member _.CompileExpression(frame: FrameContext, text: string) =
        let g = tcGlobals
        let m = range0

        result {
            // The frame's method and the F# entities enclosing it.
            let! dllInfo, ccu, enclosing, methodDef =
                withDiagnostics BuildPhase.TypeCheck (fun () ->
                    let dllInfo = tcImports.FindDllInfo(ctok, m, frame.AssemblyName)

                    let ccu =
                        tcImports.GetImportedAssemblies()
                        |> List.find (fun a -> a.ILScopeRef = dllInfo.ILScopeRef)
                        |> _.FSharpViewOfMetadata

                    let ilModule =
                        match dllInfo.RawMetadata.TryGetILModuleDef() with
                        | Some ilModule -> ilModule
                        | None -> failwith $"No metadata for '{frame.AssemblyName}'"

                    match tryFindMethod ilModule frame.MethodToken with
                    | Some(enclosing, md) -> dllInfo, ccu, enclosing, md
                    | None -> failwith $"Method 0x{frame.MethodToken:X8} not found in '{frame.AssemblyName}'")

            let entities = fsharpEntities ccu enclosing

            // Opens: enclosing namespace and modules (their values are in scope unqualified), then the PDB's.
            let opens =
                [
                    for e in entities do
                        if
                            e.IsModuleOrNamespace
                            && not (EntityHasWellKnownAttribute g WellKnownEntityAttributes.RequireQualifiedAccessAttribute e)
                        then
                            sourcePath e
                    yield! frame.Opens
                ]
                |> List.distinct

            // `this` and the parameters, as locals the expression can name.
            let declaringType = enclosing |> List.last

            let declaringTypeRef =
                mkRefForNestedILTypeDef dllInfo.ILScopeRef (enclosing |> List.take (enclosing.Length - 1), declaringType)

            if not methodDef.GenericParams.IsEmpty || not declaringType.GenericParams.IsEmpty then
                failwith "Generic frames are not supported yet"

            let thisVal =
                if methodDef.IsStatic then
                    []
                else
                    let ty = Import.ImportILType amap m [] (mkILNonGenericBoxedTy declaringTypeRef)
                    [ fst (mkLocal m "this" ty) ]

            let parameterVals =
                methodDef.Parameters
                |> List.mapi (fun i p ->
                    let ty = Import.ImportILType amap m [] p.Type
                    fst (mkLocal m (defaultArg p.Name $"arg{i}") ty))

            let frameVals = thisVal @ parameterVals

            let! parsed =
                withDiagnostics BuildPhase.Parse (fun () ->
                    let source = querySource opens text

                    ParseOneInputSourceText(
                        tcConfig,
                        lexResourceManager,
                        queryFileName,
                        (true, true),
                        DiagnosticsThreadStatics.DiagnosticsLogger,
                        SourceText.ofString source
                    ))

            let! tcState, topAttrs, implFile, denv =
                withDiagnostics BuildPhase.TypeCheck (fun () ->
                    let tcEnv0, openDecls0 =
                        GetInitialTcEnv(QueryTypeName, m, tcConfig, tcImports, tcGlobals)

                    let tcState =
                        GetInitialTcState(m, QueryTypeName, tcConfig, tcGlobals, tcImports, tcEnv0, openDecls0)

                    // The frame's module and assembly grant private and internal access to the expression.
                    let accessPaths =
                        [
                            yield CompPath(dllInfo.ILScopeRef, SyntaxAccess.Unknown, [])
                            for e in entities -> e.CompilationPath.NestedCompPath e.LogicalName (ModuleOrType)
                        ]

                    let tcEnv =
                        { tcEnv0 with
                            eInternalsVisibleCompPaths = accessPaths @ tcEnv0.eInternalsVisibleCompPaths
                        }

                    let tcEnv =
                        (tcEnv, frameVals)
                        ||> List.fold (fun env v -> AddLocalVal g TcResultsSink.NoSink m v env)

                    let implInput =
                        match parsed with
                        | ParsedInput.ImplFile implInput -> implInput
                        | ParsedInput.SigFile _ -> failwith "unexpected signature file"

                    let topAttrs, implFile, tcEnvAtEnd, _, _ =
                        CheckOneImplFile(
                            g,
                            amap,
                            tcState.Ccu,
                            openDecls0,
                            (fun () -> DiagnosticsThreadStatics.DiagnosticsLogger.ErrorCount > 0),
                            tcConfig.conditionalDefines |> Some,
                            TcResultsSink.NoSink,
                            false,
                            tcEnv,
                            None,
                            implInput,
                            tcConfig.diagnosticsOptions
                        )
                        |> Cancellable.runWithoutCancellation

                    tcState, topAttrs, implFile, tcEnvAtEnd.DisplayEnv)

            let queryVal =
                let rec find (mty: ModuleOrNamespaceType) =
                    match mty.AllValsAndMembers |> Seq.tryFind (fun v -> v.LogicalName = QueryMethodName) with
                    | Some v -> Some v
                    | None ->
                        mty.ModuleAndNamespaceDefinitions
                        |> List.tryPick (fun e -> find e.ModuleOrNamespaceType)

                match find implFile.Signature with
                | Some v -> v
                | None -> failwith $"'{QueryMethodName}' not found in the checked query module"

            if not queryVal.Typars.IsEmpty then
                failwith "Generic expressions are not supported yet"

            let resultType =
                match stripFunTy g queryVal.Type with
                | _, resultTy -> NicePrint.prettyStringOfTy denv resultTy

            let implFile =
                let (CheckedImplFile(qualifiedNameOfFile,
                                     signature,
                                     contents,
                                     hasExplicitEntryPoint,
                                     isScript,
                                     anonRecdTypes,
                                     namedDebugPoints)) =
                    implFile

                CheckedImplFile(
                    qualifiedNameOfFile,
                    signature,
                    bindFrameValues g frameVals contents,
                    hasExplicitEntryPoint,
                    isScript,
                    anonRecdTypes,
                    namedDebugPoints
                )

            let! assembly =
                withDiagnostics BuildPhase.IlxGen (fun () ->
                    let tcVal = LightweightTcValForUsingInBuildMethodCall g traitCtxtNone
                    let optEnv0 = GetInitialOptimizationEnv(tcImports, tcGlobals)

                    let optimized, _, _ =
                        ApplyAllOptimizations(tcConfig, g, tcVal, queryFileName, amap, false, optEnv0, tcState.Ccu, [ implFile ])

                    let ilxGenerator =
                        CreateIlxAssemblyGenerator(tcConfig, tcImports, g, tcVal, tcState.Ccu)

                    let codegen =
                        GenerateIlxCode(IlWriteBackend, false, tcConfig, topAttrs, optimized, QueryTypeName, ilxGenerator)

                    let ilModule =
                        let ignoresAccessChecks = ignoresAccessChecksTo g [ frame.AssemblyName ]

                        mkILSimpleModule
                            QueryTypeName
                            (QueryTypeName + ".dll")
                            true
                            (4, 0)
                            false
                            (mkILTypeDefs (fst ignoresAccessChecks :: codegen.ilTypeDefs))
                            None
                            None
                            0x0
                            (mkILExportedTypes [])
                            ""
                        |> fun ilModule ->
                            let manifest = ilModule.ManifestOfAssembly

                            { ilModule with
                                Manifest =
                                    Some
                                        { manifest with
                                            CustomAttrsStored = storeILCustomAttrs (mkILCustomAttrs (snd ignoresAccessChecks))
                                        }
                            }

                    let options =
                        {
                            ilg = g.ilg
                            outfile = QueryTypeName + ".dll"
                            pdbfile = None
                            emitTailcalls = false
                            deterministic = true
                            portablePDB = false
                            embeddedPDB = false
                            embedAllSource = false
                            embedSourceList = []
                            allGivenSources = []
                            sourceLink = ""
                            checksumAlgorithm = tcConfig.checksumAlgorithm
                            signer = None
                            dumpDebugInfo = false
                            referenceAssemblyOnly = false
                            referenceAssemblyAttribOpt = None
                            referenceAssemblySignatureHash = None
                            pathMap = tcConfig.pathMap
                            moduleCustomDebugInfoRows = []
                            methodCustomDebugInfoRows = Map.empty
                        }

                    fst (WriteILBinaryInMemory(options, ilModule, id)))

            return
                {
                    Assembly = assembly
                    TypeName = QueryTypeName
                    MethodName = QueryMethodName
                    ResultType = resultType
                }
        }
