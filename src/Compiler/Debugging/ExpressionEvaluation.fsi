// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

/// Compiles F# expressions in the context of a stopped stack frame, for the Visual Studio
/// debugger's F# expression evaluator. The result is a small assembly with one static method
/// whose parameters mirror the frame, which the debugger executes against the frame's values.
module internal FSharp.Compiler.Debugging.ExpressionEvaluation

/// The contents of a module loaded in the debuggee.
[<RequireQualifiedAccess>]
type DebuggeeImage =
    /// A complete PE image (file layout). Needed for the frame's own module, whose method bodies
    /// supply the frame's local signature.
    | PE of image: byte[]

    /// The module's metadata blob (as the debugger exposes it) and some of its embedded managed
    /// resources, each with its offset in the resources directory. F# signature and optimization
    /// data are such resources; other resources and method bodies are not needed for references.
    | Metadata of metadata: byte[] * resources: (int * byte[]) list

/// A module loaded in the debuggee.
type DebuggeeModule =
    {
        /// The module's path in the debuggee, used to identify it; it need not exist on this machine.
        FileName: string

        Image: DebuggeeImage
    }

/// A local variable of the frame that is in scope at the stop location (from the PDB).
type FrameLocal =
    {
        Name: string

        /// Index of the local in the frame method's local signature.
        Slot: int
    }

/// What a debugger alias stands for (Concord's DkmClrAliasKind).
[<RequireQualifiedAccess>]
type AliasKind =
    /// `$exception`: the exception being thrown.
    | Exception

    /// `$stowedexception`: a WinRT stowed exception.
    | StowedException

    /// `$ReturnValue`, `$ReturnValue1`, ...: the value returned by a method just stepped over.
    | ReturnValue

    /// `$1`, `$2`, ...: an object id the user made.
    | ObjectId

    /// A variable the user declared in the Immediate window.
    | Variable

/// A pseudo-variable the debugger provides, read through its intrinsic methods.
type Alias =
    {
        /// The name the expression uses, e.g. `$exception` or `$1`.
        Name: string

        Kind: AliasKind

        /// The value's runtime type, as a CLR type name (assembly-qualified or not, with generic
        /// arguments in `[[...]]`). A type the session can't find reads as `obj`.
        TypeName: string
    }

/// The stopped frame an expression is evaluated in.
type FrameContext =
    {
        /// Simple name of the assembly that contains the frame's method.
        AssemblyName: string

        /// Metadata token of the frame's method.
        MethodToken: int

        /// Token of the frame method's local signature (StandAloneSig), as the debugger reports it,
        /// or 0 to take it from the method body (which needs a PE image of the frame's module).
        LocalSignatureToken: int

        /// Namespaces and modules opened at the frame's location (from the PDB import scope).
        Opens: string list

        /// Locals in scope at the frame's location, innermost last (later names shadow earlier ones).
        Locals: FrameLocal list

        /// The debugger's pseudo-variables.
        Aliases: Alias list

        /// Display name of the assembly defining `Microsoft.VisualStudio.Debugger.Clr.IntrinsicMethods`,
        /// which aliases are read through.
        IntrinsicsAssembly: string
    }

/// A compiled expression: an assembly with `TypeName.MethodName(this?, parameters...)`.
type CompiledExpression =
    {
        Assembly: byte[]
        TypeName: string
        MethodName: string

        /// The F# type of the result, for display.
        ResultType: string

        /// Format specifiers that followed the expression (`x, h` gives `h`).
        FormatSpecifiers: string list

        /// The expression may have side effects (assignments, calls, function applications), so
        /// the debugger won't run it where side effects are not allowed.
        PotentialSideEffect: bool

        /// The result is not a variable that can be assigned to.
        ReadOnlyResult: bool

        /// The result is a `bool` (as a breakpoint condition needs).
        BoolResult: bool
    }

/// A diagnostic from compiling an expression.
type EvaluationDiagnostic =
    { Number: int
      Message: string
      IsError: bool }

/// Type-checking state for one debuggee: imports built once, reused for every expression.
[<Sealed>]
type EvaluationSession =

    /// Creates a session over the modules the debuggee has loaded, read from memory: no file
    /// of the debuggee needs to exist on this machine.
    static member Create: modules: DebuggeeModule list -> EvaluationSession

    /// Compiles an expression in the context of a frame. The query method takes the frame's
    /// `this` and parameters as parameters and starts its locals with the frame's local signature,
    /// so the debugger can run it against the frame. With `localsAsParameters` the frame's locals
    /// are trailing parameters instead, so the method can be invoked directly (for testing).
    /// In a generic frame the query class takes the frame's class type parameters and the query
    /// method its method type parameters, to be instantiated as the frame's.
    member CompileExpression:
        frame: FrameContext * text: string * ?localsAsParameters: bool ->
            Result<CompiledExpression, EvaluationDiagnostic list>
