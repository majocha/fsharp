// Copyright (c) Microsoft Corporation. All Rights Reserved. See License.txt in the project root for license information.

module internal FSharp.Compiler.RuntimeAsyncAnalysis

open FSharp.Compiler.TcGlobals
open FSharp.Compiler.TypedTree

type RuntimeAsyncAnalyzer =
    new: g: TcGlobals -> RuntimeAsyncAnalyzer

    new: g: TcGlobals * getLambdaBody: (ValRef -> Expr option) -> RuntimeAsyncAnalyzer

    member ContainsFragment: expr: Expr -> bool
    member ContainsSuspension: expr: Expr -> bool

val ShouldForceRuntimeAsyncInline: analyzer: RuntimeAsyncAnalyzer -> vref: ValRef -> inlineBody: Expr option -> bool

val ShouldForceRuntimeAsyncApplication:
    analyzer: RuntimeAsyncAnalyzer -> vref: ValRef -> inlineBody: Expr option -> args: Expr list -> bool

val GetRuntimeAsyncNonPreservableUses: g: TcGlobals -> expr: Expr -> Val list

val RestoreRuntimeAsyncPinning: expr: Expr -> unit
