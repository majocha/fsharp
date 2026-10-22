// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

/// Compiles F# expressions in the context of a stopped stack frame, for the Visual Studio
/// debugger's F# expression evaluator. The result is a small assembly with one static method
/// whose parameters mirror the frame, which the debugger executes against the frame's values.
module internal FSharp.Compiler.Debugging.ExpressionEvaluation

/// A local variable of the frame that is in scope at the stop location (from the PDB).
type FrameLocal =
    {
        Name: string

        /// Index of the local in the frame method's local signature.
        Slot: int
    }

/// The stopped frame an expression is evaluated in.
type FrameContext =
    {
        /// Simple name of the assembly that contains the frame's method.
        AssemblyName: string

        /// Metadata token of the frame's method.
        MethodToken: int

        /// Namespaces and modules opened at the frame's location (from the PDB import scope).
        Opens: string list

        /// Locals in scope at the frame's location, innermost last (later names shadow earlier ones).
        Locals: FrameLocal list
    }

/// A compiled expression: an assembly with `TypeName.MethodName(this?, parameters...)`.
type CompiledExpression =
    {
        Assembly: byte[]
        TypeName: string
        MethodName: string

        /// The F# type of the result, for display.
        ResultType: string
    }

/// A diagnostic from compiling an expression.
type EvaluationDiagnostic =
    { Number: int
      Message: string
      IsError: bool }

/// Type-checking state for one debuggee: imports built once, reused for every expression.
[<Sealed>]
type EvaluationSession =

    /// Creates a session over the given assemblies (paths of everything the debuggee has loaded).
    static member Create: references: string list -> EvaluationSession

    /// Compiles an expression in the context of a frame. The query method takes the frame's
    /// `this` and parameters as parameters and starts its locals with the frame's local signature,
    /// so the debugger can run it against the frame. With `localsAsParameters` the frame's locals
    /// are trailing parameters instead, so the method can be invoked directly (for testing).
    member CompileExpression:
        frame: FrameContext * text: string * ?localsAsParameters: bool ->
            Result<CompiledExpression, EvaluationDiagnostic list>
