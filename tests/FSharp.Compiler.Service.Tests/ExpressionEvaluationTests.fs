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
let private references () =
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

let private session = lazy (EvaluationSession.Create(references ()))

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

