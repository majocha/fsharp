/// Tests for FSharp.Compiler.Debugging.ExpressionEvaluation: expressions are compiled in the
/// context of methods of this assembly (the "debuggee"), then the produced query method is
/// loaded and invoked with argument values standing in for the frame's.
module FSharp.Compiler.Service.Tests.ExpressionEvaluationTests

open System
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open Xunit
open FSharp.Compiler.Debugging.ExpressionEvaluation

module EvalFixtures =

    let moduleValue = [ 1; 2; 3 ]

    let private secret = 42

    let add (x: int) (y: int) = x + y

    let greet (name: string) = "hello " + name

    let withLocals (x: int) =
        let doubled = x * 2
        let label = "n" + string doubled
        doubled + label.Length

    type Counter(start: int) =
        member this.Start = start
        member this.Increment(by: int) = start + by

let private all = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.Static

/// What a debuggee process has loaded: the runtime's implementation assemblies, FSharp.Core and this assembly.
let private loadedAssemblies () =
    let isManaged (path: string) =
        try
            AssemblyName.GetAssemblyName path |> ignore
            true
        with _ ->
            false

    let runtimeDir = Path.GetDirectoryName(typeof<obj>.Assembly.Location)

    [
        yield! Directory.GetFiles(runtimeDir, "*.dll") |> Array.filter isManaged
        typeof<list<int>>.Assembly.Location
        Assembly.GetExecutingAssembly().Location
    ]

/// A module as the debugger sees it in memory: its metadata blob and its F# resources (with their
/// offsets in the resources directory), nothing else.
let private metadataOnly (path: string) =
    use pe = new System.Reflection.PortableExecutable.PEReader(File.OpenRead path)
    let md = pe.GetMetadataReader()
    let metadata = pe.GetMetadata().GetContent() |> Seq.toArray
    let resourcesRva = pe.PEHeaders.CorHeader.ResourcesDirectory.RelativeVirtualAddress

    let resources =
        [
            for h in md.ManifestResources do
                let r = md.GetManifestResource h

                if r.Implementation.IsNil && (md.GetString r.Name).StartsWith "FSharp" then
                    let mutable blob = pe.GetSectionData(resourcesRva + int r.Offset).GetReader()
                    let length = blob.ReadInt32()
                    int r.Offset, blob.ReadBytes length
        ]

    DebuggeeImage.Metadata(metadata, resources)

/// Where the debuggee's modules claim to live: directories that do not exist on this machine, as
/// for a remote debuggee, so nothing can be read from disk. The shared framework and the app are
/// in different directories, as in a real process.
let private debuggeePath (path: string) =
    let runtimeDir = Path.GetDirectoryName(typeof<obj>.Assembly.Location)
    let dir = if Path.GetDirectoryName path = runtimeDir then @"Z:\debuggee\shared" else @"Z:\debuggee\app"
    Path.Combine(dir, Path.GetFileName path)

/// The debuggee's modules, as metadata only, read from memory as the debugger would. With
/// `framePE` the frame's module (this assembly) is a full image instead.
let private debuggeeModules framePE =
    let self = Assembly.GetExecutingAssembly().Location

    loadedAssemblies ()
    |> List.map (fun path ->
        {
            FileName = debuggeePath path
            Image =
                if framePE && path = self then
                    DebuggeeImage.PE(File.ReadAllBytes path)
                else
                    metadataOnly path
        })

let private session = lazy (EvaluationSession.Create(debuggeeModules false))

let private sessionWithFramePE = lazy (EvaluationSession.Create(debuggeeModules true))

let private fixtures = typeof<EvalFixtures.Counter>.DeclaringType

let private methodOf (t: Type) name = t.GetMethods all |> Array.find (fun m -> m.Name = name)

/// All named locals of a method, from this assembly's portable PDB, as the debugger would read them.
let private pdbLocals (frameMethod: MethodBase) =
    let pdbPath = Path.ChangeExtension(Assembly.GetExecutingAssembly().Location, ".pdb")
    use provider = MetadataReaderProvider.FromPortablePdbStream(File.OpenRead pdbPath)
    let pdb = provider.GetMetadataReader()
    let handle = MetadataTokens.MethodDefinitionHandle(frameMethod.MetadataToken &&& 0xFFFFFF)

    [
        for scope in pdb.GetLocalScopes handle do
            for v in (pdb.GetLocalScope scope).GetLocalVariables() do
                let local = pdb.GetLocalVariable v
                { Name = pdb.GetString local.Name; Slot = local.Index }
    ]

let private frameOf (frameMethod: MethodBase) =
    {
        AssemblyName = Assembly.GetExecutingAssembly().GetName().Name
        MethodToken = frameMethod.MetadataToken
        LocalSignatureToken =
            match frameMethod.GetMethodBody() with
            | null -> 0
            | body -> body.LocalSignatureMetadataToken
        Opens = []
        Locals = pdbLocals frameMethod
    }

let private compileWith localsAsParameters (frameMethod: MethodBase) (text: string) =
    session.Value.CompileExpression(frameOf frameMethod, text, localsAsParameters = localsAsParameters)

let private compile frameMethod text = compileWith true frameMethod text

/// Compiles `text` in the frame of `frameMethod`, runs it with `args` as the frame's this/parameters.
let private evaluate (frameMethod: MethodBase) (args: obj list) (text: string) =
    match compile frameMethod text with
    | Ok compiled ->
        let query = Assembly.Load(compiled.Assembly).GetType(compiled.TypeName).GetMethod(compiled.MethodName, all)
        query.Invoke(null, Array.ofList args), compiled.ResultType
    | Error diagnostics -> failwithf "%s: %A" text diagnostics

[<Fact>]
let ``the session reads every module from memory`` () =
    Assert.All(debuggeeModules false, fun m -> Assert.False(File.Exists m.FileName))
    Assert.Equal((box 3, "int"), evaluate (methodOf fixtures "add") [ 1; 2 ] "List.length moduleValue")

[<Fact>]
let ``constant expression`` () =
    Assert.Equal((box 3, "int"), evaluate (methodOf fixtures "add") [ 1; 2 ] "1 + 2")

[<Fact>]
let ``parameters of the frame`` () =
    Assert.Equal((box 7, "int"), evaluate (methodOf fixtures "add") [ 3; 4 ] "x + y")
    Assert.Equal((box "hello you!", "string"), evaluate (methodOf fixtures "greet") [ "you" ] "greet name + \"!\"")

[<Fact>]
let ``module values and FSharp.Core functions`` () =
    Assert.Equal((box 3, "int"), evaluate (methodOf fixtures "add") [ 0; 0 ] "moduleValue |> List.length")
    Assert.Equal((box [ 2; 4; 6 ], "int list"), evaluate (methodOf fixtures "add") [ 2; 0 ] "moduleValue |> List.map (fun i -> i * x)")

[<Fact>]
let ``private module values are accessible from the frame's module`` () =
    Assert.Equal((box 42, "int"), evaluate (methodOf fixtures "add") [ 0; 0 ] "secret")

[<Fact>]
let ``this and parameters of an instance member`` () =
    let increment = methodOf typeof<EvalFixtures.Counter> "Increment"
    let counter = EvalFixtures.Counter 10
    Assert.Equal((box 15, "int"), evaluate increment [ counter; 2 ] "this.Start + by + 3")

[<Fact>]
let ``errors are reported with F# diagnostics`` () =
    match compile (methodOf fixtures "add") "undefinedThing + x" with
    | Ok _ -> failwith "expected an error"
    | Error diagnostics -> Assert.Contains(diagnostics, fun d -> d.Number = 39)

[<Fact>]
let ``locals of the frame`` () =
    let withLocals = methodOf fixtures "withLocals"
    let locals = (frameOf withLocals).Locals |> List.map _.Name
    Assert.Equal<string list>([ "doubled"; "label" ], locals)
    // With localsAsParameters the locals follow the parameters: x, doubled, label.
    Assert.Equal((box 11, "int"), evaluate withLocals [ 3; 6; "n6" ] "doubled + label.Length + x")

[<Fact>]
let ``the query method starts its locals with the frame's local signature`` () =
    let withLocals = methodOf fixtures "withLocals"

    match compileWith false withLocals "doubled + x" with
    | Error diagnostics -> failwithf "%A" diagnostics
    | Ok compiled ->
        let query = Assembly.Load(compiled.Assembly).GetType(compiled.TypeName).GetMethod(compiled.MethodName, all)
        let frameLocals = withLocals.GetMethodBody().LocalVariables |> Seq.map _.LocalType.FullName |> List.ofSeq
        let queryLocals = query.GetMethodBody().LocalVariables |> Seq.map _.LocalType.FullName |> List.ofSeq

        Assert.Equal<string list>(frameLocals, List.truncate frameLocals.Length queryLocals)
        Assert.Equal(1, query.GetParameters().Length)
        // Invoked directly the frame's locals are zero-initialized: doubled = 0.
        Assert.Equal(box 5, query.Invoke(null, [| box 5 |]))

[<Fact>]
let ``with a full image of the frame's module the local signature comes from the method body`` () =
    let withLocals = methodOf fixtures "withLocals"
    let frame = { frameOf withLocals with LocalSignatureToken = 0 }

    match sessionWithFramePE.Value.CompileExpression(frame, "doubled + label.Length + x", localsAsParameters = true) with
    | Error diagnostics -> failwithf "%A" diagnostics
    | Ok compiled ->
        let query = Assembly.Load(compiled.Assembly).GetType(compiled.TypeName).GetMethod(compiled.MethodName, all)
        Assert.Equal(box 11, query.Invoke(null, [| box 3; box 6; box "n6" |]))

