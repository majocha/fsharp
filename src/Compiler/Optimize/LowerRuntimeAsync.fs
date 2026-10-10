// Copyright (c) Microsoft Corporation. All Rights Reserved. See License.txt in the project root for license information.

module internal FSharp.Compiler.LowerRuntimeAsync

open System.Collections.Concurrent
open System.Collections.Generic

open Internal.Utilities.Collections
open Internal.Utilities.Library
open Internal.Utilities.Library.Extras

open FSharp.Compiler
open FSharp.Compiler.AbstractIL.IL
open FSharp.Compiler.AccessibilityLogic
open FSharp.Compiler.DiagnosticsLogger
open FSharp.Compiler.Features
open FSharp.Compiler.InfoReader
open FSharp.Compiler.MethodCalls
open FSharp.Compiler.RuntimeAsync
open FSharp.Compiler.RuntimeAsyncAnalysis
open FSharp.Compiler.RuntimeAsyncExceptionRewrite
open FSharp.Compiler.Syntax
open FSharp.Compiler.TcGlobals
open FSharp.Compiler.Text
open FSharp.Compiler.TypedTree
open FSharp.Compiler.TypedTreeBasics
open FSharp.Compiler.TypedTreeOps
open FSharp.Compiler.TypeRelations

let private isSequenceCodeCall (g: TcGlobals) expr =
    match expr with
    | Expr.Val(vref, _, _) ->
        valRefEq g vref g.seq_vref
        || valRefEq g vref g.seq_delay_vref
        || valRefEq g vref g.seq_append_vref
        || valRefEq g vref g.seq_generated_vref
        || valRefEq g vref g.seq_finally_vref
        || valRefEq g vref g.seq_using_vref
        || valRefEq g vref g.seq_collect_vref
        || valRefEq g vref g.seq_map_vref
    | _ -> false

let private isQuotationUsing (v: Val) expr =
    match expr with
    | Expr.Quote(body, _, _, _, _) ->
        ExistsExpr
            (function
            | Expr.Val(vref, _, _) -> valEq v vref.Deref
            | _ -> false)
            body
    | _ -> false

let private canRewriteFunctionUses g isFragment inSequence callback continuation =
    let visiting = HashSet<Stamp>()
    let stackGuard = StackGuard("CheckRuntimeAsyncFragmentUses")

    let rec check inSequence (callback: Val) continuation =
        stackGuard.Guard(fun () ->
            if not (visiting.Add callback.Stamp) then
                true
            else
                let usesCallback expr =
                    ExistsExpr
                        (function
                        | Expr.Val(vref, _, _) -> valEq callback vref.Deref
                        | _ -> false)
                        expr

                let checkBinding inSequence continuation (TBind(v, rhs, _)) =
                    not (
                        (isFunTy g (snd (tryDestForallTy g v.Type)) || isFSharpDelegateTy g v.Type)
                        && isFragment v.Stamp
                        && not (valEq v callback)
                        && usesCallback rhs
                    )
                    || check inSequence v continuation

                let rec scan inSequence continuation =
                    FoldExpr
                        { ExprFolder0 with
                            exprIntercept =
                                fun recurse descend valid expr ->
                                    if not valid then
                                        false
                                    else
                                        match expr with
                                        | Expr.App(_, _, _, [ recipe ], _) when (TryGetRuntimeAsyncSequence g expr).IsSome ->
                                            scan true recipe
                                        | DelegateInvokeExpr g (_, _, _, Expr.Val(vref, _, _), arg, _) when valEq callback vref.Deref ->
                                            recurse true arg
                                        | Expr.App(Expr.Val(vref, _, _), _, _, args, _) when valEq callback vref.Deref && not args.IsEmpty ->
                                            List.fold recurse true args
                                        | Expr.Let(TBind(alias, Expr.Val(vref, _, _), _), body, _, _) when valEq callback vref.Deref ->
                                            check inSequence alias body && recurse true body
                                        | Expr.Let(binding, body, _, _) -> checkBinding inSequence body binding && descend true expr
                                        | Expr.LetRec(bindings, _, _, _) ->
                                            List.forall (checkBinding inSequence expr) bindings && descend true expr
                                        | Expr.App(f, _, _, args, _) when inSequence && isSequenceCodeCall g f ->
                                            List.fold recurse true args
                                        | Expr.App(f, _, _, args, _) ->
                                            recurse true f
                                            && List.forall
                                                (fun arg ->
                                                    match stripDebugPoints arg with
                                                    | Expr.Lambda(_, _, _, _, body, _, _)
                                                    | NewDelegateExpr g (_, _, body, _, _) when usesCallback body ->
                                                        (TryGetRuntimeAsyncReturn g body).IsSome && recurse true body
                                                    | _ -> recurse true arg)
                                                args
                                        | Expr.Val(vref, _, _) when valEq callback vref.Deref -> false
                                        | Expr.Quote(_, dataCell, _, _, _) ->
                                            not (isQuotationUsing callback expr)
                                            && match dataCell.Value with
                                               | Some((_, _, args, _), (_, _, legacyArgs, _)) ->
                                                   List.fold recurse (List.fold recurse true args) legacyArgs
                                               | None -> true
                                        | _ -> descend true expr
                        }
                        true
                        continuation

                let valid = scan inSequence continuation
                visiting.Remove callback.Stamp |> ignore
                valid)

    check inSequence callback continuation

let private awaitFragment (g: TcGlobals) amap resultTy invocation m =
    let awaiterTy = mkWoNullAppTy g.runtimeAsyncFragmentAwaiter_tcref [ resultTy ]
    let infoReader = InfoReader(g, amap)

    let constructor =
        match GetIntrinsicConstructorInfosOfType infoReader m awaiterTy with
        | [ constructor ] -> constructor
        | _ -> error (InternalError("runtime-async fragment awaiter constructor not found", m))

    let awaiter, awaiterExpr = mkCompGenLocal m "runtimeAsyncFragmentAwaiter" awaiterTy
    let createAwaiter = MakeMethInfoCall amap m constructor [] [ invocation ] None

    let awaitRef =
        mkILMethRef (
            g.FindSysILTypeRef "System.Runtime.CompilerServices.AsyncHelpers",
            ILCallingConv.Static,
            "AwaitAwaiter",
            1,
            [ mkILTyvarTy 0us ],
            ILType.Void
        )

    let suspend =
        Expr.Op(TOp.ILCall(false, false, false, false, NormalValUse, false, false, awaitRef, [], [ awaiterTy ], []), [], [ awaiterExpr ], m)

    let callAwaiterMember name =
        let methodInfo =
            match TryFindIntrinsicMethInfo infoReader m AccessorDomain.AccessibleFromEverywhere name awaiterTy with
            | [ methodInfo ] -> methodInfo
            | _ -> error (InternalError($"runtime-async fragment awaiter {name} not found", m))

        let wrap, address, _, _ =
            mkExprAddrOfExpr g true false NeverMutates awaiterExpr None m

        MakeMethInfoCall amap m methodInfo [] [ address ] None |> wrap

    let suspend =
        mkCond DebugPointAtBinding.NoneAtInvisible m g.unit_ty (callAwaiterMember "get_IsCompleted") (mkUnit g m) suspend

    let result = callAwaiterMember "GetResult"
    mkCompGenLet m awaiter createAwaiter (mkCompGenSequential m suspend result)

let private isRuntimeAsyncEntry g expr =
    (TryGetRuntimeAsyncReturn g expr).IsSome
    || (TryGetRuntimeAsyncSequence g expr).IsSome

let private containsRuntimeAsyncEntry g implFile =
    let folder =
        { ExprFolder0 with
            exprIntercept =
                fun _ noInterceptF acc expr ->
                    if acc || isRuntimeAsyncEntry g expr then
                        true
                    else
                        noInterceptF acc expr
        }

    FoldImplFile folder false implFile

let private outlineApplications (g: TcGlobals) amap optimizeExpr implFile =
    let stackGuard = StackGuard("OutlineRuntimeAsyncApplications")

    let isStateMachine expr =
        isReturnsResumableCodeTy g (tyOfExpr g expr)
        || match expr with
           | Expr.App(Expr.Val(vref, _, _), _, _, _, _) -> valRefEq g vref g.cgh__stateMachine_vref
           | _ -> false

    let containsFragment isFragment expr =
        FoldExpr
            { ExprFolder0 with
                exprIntercept =
                    fun _ descend found expr ->
                        if found then
                            true
                        elif isRuntimeAsyncEntry g expr || isStateMachine expr then
                            false
                        else
                            match expr with
                            | Expr.Val(vref, _, _) when isFragment vref.Stamp -> true
                            | _ -> IsRuntimeAsyncSuspensionExpr g expr || descend false expr
            }
            false
            expr

    let definitions = Dictionary<Stamp, Expr>()

    FoldImplFile
        { ExprFolder0 with
            exprIntercept =
                fun _ descend () expr ->
                    match expr with
                    | Expr.Let(TBind(v, construction, _), _, _, _) -> definitions[v.Stamp] <- construction
                    | Expr.LetRec(bindings, _, _, _) ->
                        for TBind(v, construction, _) in bindings do
                            definitions[v.Stamp] <- construction
                    | _ -> ()

                    descend () expr
        }
        ()
        implFile

    let fragments = HashSet<Stamp>()
    let mutable changed = true

    while changed do
        changed <- false

        for KeyValue(stamp, construction) in definitions do
            let refersToFragment = containsFragment fragments.Contains construction

            if refersToFragment && fragments.Add stamp then
                changed <- true

    let called = HashSet<Stamp>()

    let rec visitCalls expr =
        stackGuard.Guard(fun () -> visitCallsCore expr)

    and visitCallsCore expr =
        FoldExpr
            { ExprFolder0 with
                exprIntercept =
                    fun _ descend () expr ->
                        match expr with
                        | _ when isStateMachine expr -> ()
                        | Expr.Op((TOp.While _ | TOp.IntegerForLoop _ | TOp.TryFinally _ | TOp.TryWith _), _, args, _) ->
                            for arg in args do
                                match arg with
                                | Expr.Lambda(_, _, _, _, body, _, _) -> visitCalls body
                                | _ -> visitCalls arg
                        | DelegateInvokeExpr g (_, _, _, receiver, arg, _) ->
                            visitCallable receiver
                            visitCalls arg
                        | Expr.App(f, _, _, args, _) when not args.IsEmpty ->
                            visitCallable f

                            for arg in args do
                                visitCalls arg
                        | Expr.Lambda _
                        | Expr.TyLambda _
                        | Expr.Obj _
                        | Expr.Quote _ -> ()
                        | _ -> descend () expr
            }
            ()
            expr

    and visitCallable expr =
        stackGuard.Guard(fun () -> visitCallableCore expr)

    and visitCallableCore expr =
        FoldExpr
            { ExprFolder0 with
                exprIntercept =
                    fun _ descend () expr ->
                        match expr with
                        | _ when isRuntimeAsyncEntry g expr || isStateMachine expr -> ()
                        | Expr.Val(vref, _, _) when fragments.Contains vref.Stamp && called.Add vref.Stamp ->
                            visitCallable definitions[vref.Stamp]
                        | Expr.Lambda(_, _, _, _, body, _, _)
                        | Expr.TyLambda(_, _, body, _, _) -> visitCallable body
                        | NewDelegateExpr g (_, _, body, _, _) -> visitCallable body
                        | Expr.Quote _ -> ()
                        | _ -> descend () expr
            }
            ()
            expr

    FoldImplFile
        { ExprFolder0 with
            exprIntercept =
                fun _ descend () expr ->
                    match TryGetRuntimeAsyncReturn g expr, TryGetRuntimeAsyncSequence g expr with
                    | Some info, _ -> visitCalls info.Body
                    | _, Some(recipe, _) -> visitCallable recipe
                    | _ -> ()

                    descend () expr
        }
        ()
        implFile

    let outlinedType argTy resultTy m =
        let helper = g.cgh__runtimeAsyncOutline_vref

        let template =
            primMkApp (exprForValRef m helper, helper.Type) [ argTy; resultTy ] [] m

        match tryDestFunTy g (tyOfExpr g template) with
        | ValueSome(_, result) -> result
        | ValueNone -> error (InternalError("runtime-async outlining template is not a function", m))

    let fragmentTaskRef, fragmentResultRef =
        let m = g.cgh__runtimeAsyncOutline_vref.Range

        match stripTyEqns g (rangeOfFunTy g (outlinedType g.unit_ty g.unit_ty m)) with
        | AppTy g (taskRef, [ AppTy g (resultRef, [ _ ]) ]) -> taskRef, resultRef
        | _ -> error (InternalError("runtime-async outlining template has an unexpected result type", m))

    let isFunction ty =
        isFunTy g (snd (tryDestForallTy g ty)) || isFSharpDelegateTy g ty

    let fragmentResultType ty =
        match stripTyEqns g ty with
        | AppTy g (taskRef, [ AppTy g (resultRef, [ resultTy ]) ]) when
            tyconRefEq g taskRef fragmentTaskRef && tyconRefEq g resultRef fragmentResultRef
            ->
            resultTy
        | _ -> ty

    let canOutline expr parameters body m =
        let unsafeType ty = isByrefTy g ty || isByrefLikeTy g m ty
        let captures = (freeInExpr (CollectLocalsWithStackGuard()) expr).FreeLocals

        not (
            List.exists (fun (v: Val) -> unsafeType v.Type) parameters
            || unsafeType (tyOfExpr g body)
            || Zset.exists (fun (v: Val) -> v.IsFixed || v.IsPinning || unsafeType v.Type) captures
            || ExistsExpr
                (function
                | Expr.Let(TBind(v, _, _), _, _, _) -> v.IsFixed || v.IsPinning
                | _ -> false)
                body
        )

    let outlineLambda parameters body m =
        let resultTy = tyOfExpr g body
        let callback = mkMultiLambda m parameters (body, resultTy)
        let argTy = mkLambdaArgTy m (List.map (fun (v: Val) -> v.Type) parameters)
        let helper = g.cgh__runtimeAsyncOutline_vref

        primMkApp (exprForValRef m helper, helper.Type) [ argTy; resultTy ] [ callback ] m
        |> optimizeExpr true

    let rec adaptCallable expectedTy expr =
        stackGuard.Guard(fun () -> adaptCallableCore expectedTy expr)

    and adaptCallableCore expectedTy expr =
        let ty = tyOfExpr g expr

        if typeEquiv g ty expectedTy then
            expr
        else
            let m = expr.Range
            let saved, savedExpr = mkCompGenLocal m "runtimeAsyncCallable" ty

            let parameters, invocation =
                match tryDestFunTy g ty with
                | ValueSome(argTy, _) ->
                    let parameter, argument = mkCompGenLocal m "argument" argTy
                    [ parameter ], mkApps g ((savedExpr, ty), [], [ argument ], m)
                | ValueNone when isFSharpDelegateTy g ty ->
                    let (SigOfFunctionForDelegate(invoke, argTys, _, _)) =
                        GetSigOfFunctionForDelegate (InfoReader(g, amap)) ty m AccessorDomain.AccessibleFromEverywhere

                    let parameters, arguments =
                        argTys |> List.map (mkCompGenLocal m "argument") |> List.unzip

                    let parameters =
                        if parameters.IsEmpty then
                            [ fst (mkCompGenLocal m "argument" g.unit_ty) ]
                        else
                            parameters

                    parameters, MakeMethInfoCall amap m invoke [] (savedExpr :: arguments) None
                | _ -> error (InternalError("runtime-async branch result is not callable", m))

            let resultTy = fragmentResultType (rangeOfFunTy g expectedTy)
            let body = adaptCallable resultTy invocation
            mkCompGenLet m saved expr (outlineLambda parameters body m)

    let rec shape shapes expr =
        stackGuard.Guard(fun () -> shapeCore shapes expr)

    and shapeCore shapes expr =
        match expr with
        | Expr.Val(vref, _, _) -> Map.tryFind vref.Stamp shapes |> Option.defaultValue vref.Type
        | Expr.TyLambda(_, typars, body, _, _) -> mkForallTy typars (shape shapes body)
        | Expr.Lambda(_, None, None, parameters, body, m, _)
        | NewDelegateExpr g (_, parameters, body, m, _) when canOutline expr parameters body m ->
            let argTy = mkLambdaArgTy m (List.map (fun (v: Val) -> v.Type) parameters)
            outlinedType argTy (shape shapes body) m
        | Expr.Let(TBind(v, construction, _), body, _, _) ->
            let shapes =
                if isFunction v.Type && canRewriteFunctionUses g fragments.Contains false v body then
                    Map.add v.Stamp (shape shapes construction) shapes
                else
                    shapes

            shape shapes body
        | Expr.LetRec(_, body, _, _)
        | Expr.DebugPoint(_, body)
        | Expr.Sequential(_, body, NormalSeq, _) -> shape shapes body
        | DelegateInvokeExpr g (_, _, _, receiver, _, _) ->
            let ty = shape shapes receiver

            if typeEquiv g ty (tyOfExpr g receiver) then
                tyOfExpr g expr
            else
                fragmentResultType (rangeOfFunTy g ty)
        | Expr.App(f, fty, tyargs, args, _) ->
            let ty = shape shapes f

            if typeEquiv g ty fty then
                tyOfExpr g expr
            else
                (applyTyArgs g ty tyargs, args)
                ||> List.fold (fun ty _ -> fragmentResultType (rangeOfFunTy g ty))
        | Expr.Match(_, _, _, targets, _, ty) ->
            targets
            |> Array.map (fun target -> shape shapes target.TargetExpression)
            |> Array.tryFind (fun targetTy -> not (typeEquiv g targetTy ty))
            |> Option.defaultValue ty
        | _ -> tyOfExpr g expr

    let rec rewrite inContext callable inSequence replacements expr =
        let env =
            {
                PreIntercept = Some(intercept inContext callable inSequence replacements)
                PreInterceptBinding = None
                PostTransform = (fun _ -> None)
                RewriteQuotations = false
                StackGuard = stackGuard
            }

        RewriteExpr env expr

    and intercept inContext callable inSequence replacements _ expr =
        let rewriteBody = rewrite inContext false inSequence replacements

        let containsFragment =
            containsFragment (fun stamp -> Map.containsKey stamp replacements)

        match expr with
        | _ when inContext && isStateMachine expr -> Some(rewrite false false false replacements expr)
        | Expr.App(f, fty, tyargs, [ body ], m) when (TryGetRuntimeAsyncReturn g expr).IsSome ->
            RestoreRuntimeAsyncPinning body
            Some(Expr.App(f, fty, tyargs, [ rewrite true false false replacements body ], m))
        | Expr.App(f, fty, tyargs, [ recipe ], m) when (TryGetRuntimeAsyncSequence g expr).IsSome ->
            Some(Expr.App(f, fty, tyargs, [ rewrite true false true replacements recipe ], m))
        | Expr.Val(vref, flags, m) ->
            Map.tryFind vref.Stamp replacements
            |> Option.map (fun v -> Expr.Val(mkLocalValRef v, flags, m))
        | Expr.Op((TOp.While _ | TOp.IntegerForLoop _ | TOp.TryFinally _ | TOp.TryWith _) as op, tyargs, args, m) when inContext ->
            let args =
                args
                |> List.map (function
                    | Expr.Lambda(stamp, ctor, self, parameters, body, range, _) ->
                        let body = rewriteBody body
                        Expr.Lambda(stamp, ctor, self, parameters, body, range, tyOfExpr g body)
                    | arg -> rewriteBody arg)

            Some(Expr.Op(op, tyargs, args, m))
        | Expr.Op(op, tyargs, args, m) when inContext -> Some(Expr.Op(op, tyargs, List.map rewriteBody args, m))
        | Expr.Sequential(first, rest, NormalSeq, m) when inContext ->
            Some(Expr.Sequential(rewriteBody first, rewrite inContext callable inSequence replacements rest, NormalSeq, m))
        | Expr.TyLambda(stamp, typars, body, m, _) when inContext && callable ->
            let body = rewrite true true false replacements body
            Some(Expr.TyLambda(stamp, typars, body, m, tyOfExpr g body))
        | Expr.Lambda(_, None, None, parameters, body, m, _)
        | NewDelegateExpr g (_, parameters, body, m, _) when inContext && callable ->
            if not (canOutline expr parameters body m) then
                Some expr
            else
                let body = rewrite true (isFunction (tyOfExpr g body)) false replacements body
                Some(outlineLambda parameters body m)
        | Expr.Lambda(stamp, ctor, self, parameters, body, m, _) when inSequence ->
            let body = rewriteBody body
            Some(Expr.Lambda(stamp, ctor, self, parameters, body, m, tyOfExpr g body))
        | Expr.Lambda _ when inContext -> Some(rewrite false false false replacements expr)
        | Expr.App(f, fty, tyargs, args, m) when inSequence && isSequenceCodeCall g f ->
            Some(Expr.App(f, fty, tyargs, List.map rewriteBody args, m))
        | DelegateInvokeExpr g (invoke, fty, tyargs, receiver, arg, m) when inContext ->
            let outlined = rewrite true (containsFragment receiver) false replacements receiver
            let arg = rewrite inContext false false replacements arg

            if typeEquiv g (tyOfExpr g outlined) (tyOfExpr g receiver) then
                Some(Expr.App(invoke, fty, tyargs, [ outlined; arg ], m))
            else
                let invocation = mkApps g ((outlined, tyOfExpr g outlined), [], [ arg ], m)
                Some(awaitFragment g amap (fragmentResultType (tyOfExpr g invocation)) invocation m)
        | Expr.App(f, fty, tyargs, args, m) when inContext && not args.IsEmpty ->
            let f = rewrite true (containsFragment f) false replacements f
            let args = List.map (rewrite inContext false false replacements) args

            let rec apply f args =
                match args with
                | [] -> f
                | arg :: rest ->
                    let invocation = mkApps g ((f, tyOfExpr g f), [], [ arg ], m)
                    let ty = tyOfExpr g invocation
                    let resultTy = fragmentResultType ty

                    let result =
                        if typeEquiv g ty resultTy then
                            invocation
                        else
                            awaitFragment g amap resultTy invocation m

                    apply result rest

            if typeEquiv g (tyOfExpr g f) fty then
                Some(Expr.App(f, fty, tyargs, args, m))
            else
                let f = primMkApp (f, tyOfExpr g f) tyargs [] m
                Some(apply f args)
        | Expr.Let(TBind(v, construction, point), body, m, _) when inContext || called.Contains v.Stamp ->
            let candidate =
                isFunction v.Type
                && not v.IsMutable
                && (containsFragment construction || called.Contains v.Stamp)
                && canRewriteFunctionUses g fragments.Contains inSequence v body

            let construction =
                rewrite (inContext || candidate) candidate false replacements construction

            let ty = tyOfExpr g construction

            if not candidate || typeEquiv g ty v.Type then
                Some(mkLetBind m (TBind(v, construction, point)) (rewrite inContext callable inSequence replacements body))
            else
                let replacement, _ = mkLocal m v.LogicalName ty

                Some(
                    mkLet
                        point
                        m
                        replacement
                        construction
                        (rewrite inContext callable inSequence (Map.add v.Stamp replacement replacements) body)
                )
        | Expr.LetRec(bindings, body, m, _) when
            inContext
            || List.exists (fun (TBind(v, _, _)) -> called.Contains v.Stamp) bindings
            ->
            let uses = mkLetRecBinds m bindings body

            let replacements =
                (replacements, bindings)
                ||> List.fold (fun replacements (TBind(v, construction, _)) ->
                    if
                        (containsFragment construction || called.Contains v.Stamp)
                        && isFunction v.Type
                        && canRewriteFunctionUses g fragments.Contains inSequence v uses
                    then
                        let replacement, _ = mkLocal m v.LogicalName v.Type
                        Map.add v.Stamp replacement replacements
                    else
                        replacements)

            let mutable changed = true

            while changed do
                changed <- false

                for TBind(v, construction, _) in bindings do
                    match Map.tryFind v.Stamp replacements with
                    | Some replacement ->
                        let ty = shape (Map.map (fun _ (v: Val) -> v.Type) replacements) construction

                        if not (typeEquiv g ty replacement.Type) then
                            replacement.SetType ty
                            changed <- true
                    | None -> ()

            let bindings =
                bindings
                |> List.map (fun (TBind(v, construction, point)) ->
                    let replacement = Map.tryFind v.Stamp replacements
                    TBind(defaultArg replacement v, rewrite true replacement.IsSome false replacements construction, point))

            Some(mkLetRecBinds m bindings (rewrite inContext callable inSequence replacements body))
        | Expr.Match(point, matchRange, _, targets, m, _) when inContext && callable ->
            let tree =
                match rewriteBody expr with
                | Expr.Match(_, _, tree, _, _, _) -> tree
                | _ -> error (InternalError("runtime-async match rewriting changed the expression shape", m))

            let expectedTy = shape (Map.map (fun _ (v: Val) -> v.Type) replacements) expr

            TryMapRuntimeAsyncMatchTargets g (point, matchRange, tree, targets, m) (fun _ body ->
                Some(rewrite true true false replacements body |> adaptCallable expectedTy))
        | Expr.Obj _ when inContext -> Some(rewrite false false false replacements expr)
        | Expr.Quote _ -> Some expr
        | _ -> None

    RewriteImplFile
        {
            PreIntercept = Some(intercept false false false Map.empty)
            PreInterceptBinding = None
            PostTransform = (fun _ -> None)
            RewriteQuotations = false
            StackGuard = stackGuard
        }
        implFile

/// Prepares each runtime-async body once its final shape is known, innermost first.
let private prepareBodies g (reportedRanges: ConcurrentDictionary<range, unit>) implFile =
    let prepare body =
        for v in GetRuntimeAsyncNonPreservableUses g body do
            if reportedRanges.TryAdd(v.Range, ()) then
                errorR (Error(FSComp.SR.ilRuntimeAsyncLocalUsedAfterSuspension (RichText.mkText v.DisplayName), v.Range))

        RewriteRuntimeAsyncExceptionHandlers g body

    RewriteImplFile
        {
            PreIntercept = None
            PreInterceptBinding = None
            PostTransform =
                fun expr ->
                    match expr with
                    | Expr.App(f, fty, tyargs, [ body ], m) when (TryGetRuntimeAsyncReturn g expr).IsSome ->
                        Some(Expr.App(f, fty, tyargs, [ prepare body ], m))
                    | _ -> None
            RewriteQuotations = false
            StackGuard = StackGuard("PrepareRuntimeAsyncBodies")
        }
        implFile

let TransformImplFile (g: TcGlobals) amap optimizeExpr reportedRanges (implFile: CheckedImplFile) =
    if containsRuntimeAsyncEntry g implFile then
        // Bodies inlined from an assembly compiled with the feature are still prepared.
        let implFile =
            if g.langVersion.SupportsFeature LanguageFeature.RuntimeAsync then
                outlineApplications g amap optimizeExpr implFile
            else
                implFile

        prepareBodies g reportedRanges implFile
    else
        implFile
