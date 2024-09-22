module internal Inlining

open System.Collections.Generic
open Ast
open Analyzer

let private isTrivialExpr = function // "trivial" means "small enough to inline to multiple places".
    | Var v when v.Name = "true" || v.Name = "false" -> true
    | Int _
    | Float _ -> true
    | _ -> false

type VariableInlining(options: Options.Options) =

    // Return the list of variables used in the statements, with the number of references.
    let countReferences stmtList =
        let counts = Dictionary<VarDecl, int>()
        let collectLocalUses _ = function
            | ResolvedVariableUse (_, vd) as e ->
                counts.[vd] <- match counts.TryGetValue(vd) with _, n -> n + 1
                e
            | e -> e
        for expr in stmtList do
            options.visitor(collectLocalUses).iterStmt BlockLevel.Unknown expr
        counts

    let isEffectivelyConst (ident: Ident) =
        match ident.Declaration with
        | Declaration.Variable varDecl -> not varDecl.isEverWrittenAfterDecl
        | Declaration.UserFunction funDecl -> not funDecl.hasExternallyVisibleSideEffects
        | Declaration.BuiltinFunction -> Builtin.pureBuiltinFunctions.Contains ident.Name
        | _ -> false

    // Mark variables as inlinable when possible.
    // Variables are always safe to inline when all of:
    //  - the variable is used only once in the current block
    //  - the variable is not used in a loop sub-block, for runtime performance
    //  - the init value refers only to variables that are never written to, and functions that are builtin and pure
    let markSafelyInlinableLocals block =
        // Variables that are defined in this scope.
        // The boolean indicate if the variable initialization is const.
        let localDefs = Dictionary<string, (Ident * bool)>()
        for stmt: Stmt in block do
            match stmt with
            | Decl (_, declElts) ->
                for def in declElts do
                    // can only inline if it has a value
                    match def.init with
                    | None ->
                        localDefs.[def.name.Name] <- (def.name, true)
                    | Some init ->
                        let isConst = Analyzer(options).varUsesInStmt (Expr init) |> Seq.forall isEffectivelyConst
                        localDefs.[def.name.Name] <- (def.name, isConst)
            | _ -> ()
        // List of all expressions under the current block, but do not look in loops.
        let mutable localExprs = []
        let rec addLocalExprs stmt = 
            match stmt with
            | Decl (_, declElts) ->
                for def in declElts do
                    match def.init with
                    | None -> ()
                    | Some init -> localExprs <- init :: localExprs
            | Expr e
            | Jump (_, Some e) -> localExprs <- e :: localExprs
            | If (cond, th, el) ->
                localExprs <- cond :: localExprs
                addLocalExprs th
                el |> Option.iter addLocalExprs
            | Block stmts -> stmts |> Seq.iter addLocalExprs
            | Directive _ | Verbatim _ | Jump (_, None) | ForE _ | ForD _ | While _ | DoWhile _ | Switch _ -> ()
        for stmt: Stmt in block do
            addLocalExprs stmt

        let localReferences = countReferences [for e in localExprs -> Expr e]
        let allReferences = countReferences block

        for def in localDefs do
            let ident, isConst = def.Value
            let varDecl = ident.VarDecl.Value
            if not ident.DoNotInline && not ident.ToBeInlined && not varDecl.isEverWrittenAfterDecl then
                let decl = varDecl.decl
                match localReferences.TryGetValue(varDecl), allReferences.TryGetValue(varDecl) with
                | (_, 1), (_, 1) when isConst && decl.init <> None ->
                    options.trace  $"{ident.Loc}: inlining local variable '{Printer.debugIdent ident}' because it's safe to inline (const) and used only once"
                    ident.ToBeInlined <- true
                | (_, 0), (_, 0) ->
                    let ok = match decl.init with
                             | Some init -> Effects.isPure init
                             | None -> true
                    if ok then
                        options.trace $"{ident.Loc}: inlining (removing) local variable '{Printer.debugDecl decl}' because it's unused and the init is pure or missing"
                        ident.ToBeInlined <- true
                | _ -> ()

    // Detect if a variable can be inlined in multiple places, based on its value.
    let isSimpleEnoughToInline (init: Expr) =
        match init with
        | e when isTrivialExpr e -> true
        | _ when not options.aggroInlining -> false
        // Allow a few things to be inlined with aggroInlining (even if they have side effects!)
        | ResolvedVariableUse (v, vd)
        | Dot (ResolvedVariableUse (v, vd), _)
            when not vd.decl.name.ToBeInlined -> // Don't inline the use of a variable that's already marked for inlining!
            isEffectivelyConst v
        | FunCall(Op op, args) ->
            not (Builtin.assignOps.Contains op) &&
                args |> List.forall isTrivialExpr
        | FunCall(Var fct, args) ->
            Builtin.pureBuiltinFunctions.Contains fct.Name &&
                args |> List.forall isTrivialExpr
        | _  -> false

    // Inline global or local variables, regardless of where they are used or how often they are used, when all of:
    //  - it is not external
    //  - it is never written after declaration
    //  - it is either:
    //      - an uninitialized local (remove it). this breaks the shader if the local is read.
    //      - the init value is a simple constant, or with aggro inlining, it uses only builtin functions and variables never written to.
    let markUnwrittenVariablesWithSimpleInit level = function
        | (ty: Type, defs) when not ty.IsExternal ->
            for (def:DeclElt) in defs do
                if not def.name.ToBeInlined && // already done in a previous pass
                   not def.name.DoNotInline &&
                   not def.name.VarDecl.Value.isEverWrittenAfterDecl then
                    match def.init with
                    | None -> ()
                        // Top-level values are special, in particular in HLSL. Keep them for now.
                        // Never-written locals without init might be unused, but we don't know for sure here. Let safe inlining handle them.
                    | Some init ->
                        if isSimpleEnoughToInline init then
                            // Never-written locals and globals are inlined when their value is "simple enough".
                            // This can increase non-compressed size but decreases compressed size.
                            let varKind = match level with Level.TopLevel -> "global" | Level.InFunc -> "local"
                            options.trace $"{def.name.Loc}: inlining {varKind} variable '{Printer.debugDecl def}' because it's never written and has a 'simple' definition"
                            def.name.ToBeInlined <- true
        | _ -> ()

    let markSafelyInlinableVariables li =
        let mapStmt _ stmt =
            match stmt with
            | Block b -> markSafelyInlinableLocals b
            | _ -> ()
            stmt
        options.visitor(fStmt = mapStmt).iterTopLevel li
        ()

    let markSimpleInlinableVariables li =
        let mapStmt _ stmt =
            match stmt with
            | Decl d -> markUnwrittenVariablesWithSimpleInit Level.InFunc d
            | ForD (d, _, _, _) -> markUnwrittenVariablesWithSimpleInit Level.InFunc d
            | _ -> ()
            stmt
        // Visit locals
        options.visitor(fStmt = mapStmt).iterTopLevel li
        // Visit globals
        for tl in li do
            match tl with
            | TLDecl d -> markUnwrittenVariablesWithSimpleInit Level.TopLevel d; ()
            | _ -> ()
        ()
    
    member _.MarkInlinableVariables li =
        markSafelyInlinableVariables li
        // "simple" inlining must come after "safe" inlining, because it must check that it's not going to inline a var already being inlined.
        markSimpleInlinableVariables li


type FunctionInlining(options: Options.Options) =

    // To ensure correctness, we verify if it's safe to inline.
    //
    // [A] Only inline a function if it never refers to a global function or variable by a name that is shadowed by a local variable in scope at the call site.
    // [B] Only inline a function if it has only one call site.
    //     Exception: if the body is "trivial" it will be inlined at all call sites.
    // [C] Only inline a function if it is a single expression return.
    //     This also ensures the function does not declare any locals.
    // [D] Only inline a function if it uses its 'in' parameters at most once,
    //     or if the parameter is used multiple times but the argument expression can be duplicated at the call site.
    //     No attempt is made to inline in other cases. For example, it would be correct to inline
    //     when the parameter is written but the argument is a lvalue that doesn't make any side effect and is not used after the call site.
    //     Repeating the expression could increase the shader size or decrease run time performance.
    // [E] Only inline a function if its 'in' parameters are never written to (through assignOps or calling an out or inout function or operator).
    //     No attempt is made to find if the passed argument is an lvalue that's never used after calling the function to inline.
    //     No attempt is made to copy the argument into a newly declared local variable at the call site to get correct writing semantics.
    // [F] Only inline a function if it has no 'out' or 'inout' parameters.
    //     'out' or 'inout' parameters must be lvalues, which simplifies things. But there are problems to watch out for.
    //     Evaluating them could have side effects (e.g. a[b++]), which is a problem if they are used more than once.
    //     If the 'out' parameters are read from, inlining can change the behavior.
    //     It's fine if 'out' parameters are written in more than one place.
    // [G] Only inline a function if the argument expressions are pure.
    //     Inlining can change the evaluation order of the arguments, and will remove unused arguments.
    //     This is fine when they are pure. Except in one case:
    //     BUG: Function inlining can delay the evaluation order of an argument expression that reads a global,
    //     and the global can be modified by the inlined function before it's evaluated as part of the argument.
    //         int g = 0; int foo(int a) { return ++g - a; } int main() { return foo(g); } // `foo(g)` is 1, but `g++ - g` would be 0
    let verifyVarsAndParams funcInfo (callSites: CallSite list) =
        let paramUsageCounts = Dictionary<string, int>()
        let mutable shadowedGlobal = false
        let mutable paramIsWritten = false

        let visitVarUsesInBody _ = function
            | ResolvedVariableUse (v, vd) as e ->
                match vd.scope with
                | VarScope.Local ->
                    failwith "There shouldn't be any locals in a function whose body is a single return statement."
                | VarScope.Parameter ->
                    if vd.isEverWrittenAfterDecl then
                        paramIsWritten <- true
                    paramUsageCounts.[v.Name] <- match paramUsageCounts.TryGetValue(v.Name) with _, n -> n + 1
                | VarScope.Global ->
                    if callSites |> List.exists (fun callSite -> callSite.varsInScope |> List.contains v.Name) then
                        shadowedGlobal <- true
                e
            | e -> e
        options.visitor(visitVarUsesInBody).iterTopLevel [funcInfo.func]

        if paramIsWritten || // [E]
            shadowedGlobal // [A]
        then false
        else
            let canBeDuplicated = function
                | ResolvedVariableUse (v, vd) -> // allow non-global variable reads
                    not v.isVarWrite &&
                    not (vd.scope = VarScope.Global)
                | e -> isTrivialExpr e

            let paramNames = funcInfo.funcType.args |> List.map (fun (_, argDeclElts) ->
                match argDeclElts with
                | [declElt] -> declElt.name.Name
                | _ -> failwith "arguments have one declElt each.")

            let mutable hasAnyImpureArg = false
            let mutable cannotDuplicateArg = false
            for callSite in callSites do
                for argIndex, argExpr in callSite.argExprs |> Seq.indexed do
                    if not (Effects.isPure argExpr) then
                        hasAnyImpureArg <- true
                    let paramUsageCount = match paramUsageCounts.TryGetValue(paramNames.[argIndex]) with _, n -> n
                    if paramUsageCount > 1 && not (canBeDuplicated argExpr) then
                        cannotDuplicateArg <- true

            let ok =
                not hasAnyImpureArg && // [G]
                not cannotDuplicateArg // [D]
            ok

    let tryMarkFunctionToInline (funcInfo: FuncInfo) (callSites: CallSite list) =
        if not funcInfo.funcType.fName.DoNotInline && verifyVarsAndParams funcInfo callSites then
            // Mark only the function's ident, not the call sites.
            // simplifyExpr can't rely on call sites to be marked anyway, to support the function inline pragma.
            options.trace $"{funcInfo.funcType.fName.Loc}: inlining function '{Printer.debugFunc funcInfo.funcType}' into {callSites.Length} call sites"
            funcInfo.funcType.fName.ToBeInlined <- true

    let markInlinableFunctions code =
        let funcInfos = Analyzer(options).findFuncInfos code
        for funcInfo in funcInfos do
            let canBeRenamed = not (options.noRenamingList |> List.contains funcInfo.name) // noRenamingList includes "main"
            if canBeRenamed && not (funcInfo.funcType.isExternal(options)) && funcInfo.isResolvable then
                if not funcInfo.funcType.hasOutOrInoutParams then // [F]
                    // Find calls to this function. This works because we checked that the function is not overloaded ambiguously.
                    let callSites = funcInfos |> List.collect (fun n -> n.callSites)
                                              |> List.filter (fun callSite -> callSite.prototype = funcInfo.funcType.prototype)
                    if callSites.Length > 0 then // Unused function elimination is not handled here
                        match funcInfo.body.asStmtList with
                        | [Jump (JumpKeyword.Return, Some body)] -> // [C]
                            if callSites.Length = 1 || isTrivialExpr body then // [B]
                                tryMarkFunctionToInline funcInfo callSites
                        | _ -> ()

    member _.MarkInlinableFunctions = markInlinableFunctions


type [<NoComparison>] private Inlining = {
    func: TopLevel
    argIndex: int
    varDecl: VarDecl
    argExpr: Expr
}
// Inline the argument of a function call into the function body.
type ArgumentInlining(options: Options.Options) =

    let rec isInlinableExpr = function
        // This is different that purity: reading a variable is pure, but non-inlinable in general.
        | ResolvedVariableUse (_, vd) ->
            // 'in' uniforms are read-only globals, they can be inlined
            vd.scope = VarScope.Global && not vd.isEverWrittenAfterDecl
        | Var v when v.Name = "true" || v.Name = "false" -> true
        | Int _
        | Float _ -> true
        | FunCall(Var fct, args) -> Builtin.pureBuiltinFunctions.Contains fct.Name && List.forall isInlinableExpr args
        | FunCall(Op op, args) -> not (Builtin.assignOps.Contains op) && List.forall isInlinableExpr args
        | _ -> false

    // Find when functions are always called with the same trivial expr, that can be inlined into the function body.
    let findInlinings code: Inlining list =
        let mutable argInlinings = []
        Analyzer(options).resolve code
        Analyzer(options).markWrites code
        let funcInfos = Analyzer(options).findFuncInfos code
        for funcInfo in funcInfos do
            let canBeRenamed = not (options.noRenamingList |> List.contains funcInfo.name) // noRenamingList includes "main"
            // If the function is overloaded, removing a parameter could conflict with another overload.
            if canBeRenamed && not (funcInfo.funcType.isExternal(options)) && funcInfo.isOverloaded then
                let callSites = funcInfos |> List.collect (fun n -> n.callSites) |> List.filter (fun n -> n.prototype = funcInfo.funcType.prototype)
                for argIndex, (_, argDecl) in List.indexed funcInfo.funcType.parameters do
                    match argDecl.name.VarDecl with
                    | Some varDecl when not varDecl.ty.isOutOrInout -> // Only inline 'in' parameters.
                        let argExprs = callSites |> List.map (fun c -> c.argExprs |> List.item argIndex) |> List.distinct
                        match argExprs with
                        | [argExpr] when isInlinableExpr argExpr -> // The argExpr must always be the same at all call sites.
                            options.trace $"{varDecl.decl.name.Loc}: inlining expression '{Printer.exprToS argExpr}' into argument '{Printer.debugDecl varDecl.decl}' of '{Printer.debugFunc funcInfo.funcType}'"
                            argInlinings <- {func=funcInfo.func; argIndex=argIndex; varDecl=varDecl; argExpr=argExpr} :: argInlinings
                        | _ -> ()
                    | _ -> ()
        argInlinings

    let apply (didInline: bool ref) code =
        let argInlinings = findInlinings code

        let removeInlined func list =
            list
            |> List.indexed
            |> List.filter (fun (idx, _) -> not (argInlinings |> List.exists (fun inl -> inl.func = func && inl.argIndex = idx)))
            |> List.map snd

        let applyExpr _ = function
            | FunCall (Var v, argExprs) as f ->
                // Remove the argument expression from the call site.
                match v.Declaration with
                | Declaration.UserFunction fd -> FunCall (Var v, removeInlined fd.func argExprs)
                | _ -> f
            | x -> x

        let applyTopLevel = function
            | Function(fct, body) as f ->
                // Handle argument inlining for other functions called by f.
                let _, body = options.visitor(applyExpr).mapStmt (BlockLevel.FunctionRoot fct) body
                // Handle argument inlining for f. Remove the parameter from the declaration.
                let fct = {fct with args = removeInlined f fct.args}
                // Handle argument inlining for f. Insert in front of the body a declaration for each inlined argument.
                let decls =
                    argInlinings
                    |> List.filter (fun inl -> inl.func = f)
                    |> List.map (fun inl -> Decl (
                        {inl.varDecl.ty with typeQ = inl.varDecl.ty.typeQ |> List.filter ((=) "const")},
                        [{inl.varDecl.decl with init = Some inl.argExpr}]))
                Function(fct, Block (decls @ body.asStmtList))
            | tl -> tl

        if argInlinings.IsEmpty then
            code
        else
            let code = code |> List.map applyTopLevel
            didInline.Value <- true
            code
    member _.Apply = apply

