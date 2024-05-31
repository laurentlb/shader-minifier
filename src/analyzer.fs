module Analyzer

open System.Collections.Generic
open Ast

// The module performs some static analysis on the code and stores the
// information in the AST nodes, e.g. find which variables are modified,
// which declarations can be inlined.

let (|ResolvedVariableUse|_|) = function
    | Var v ->
        match v.Declaration with
        | Declaration.Variable vd -> Some (v, vd)
        | _ -> None
    | _ -> None

let rec sideEffects = function
    | Var _ -> []
    | Int _
    | Float _ -> []
    | Dot(v, _)  -> sideEffects v
    | Subscript(e1, e2) -> (e1 :: (Option.toList e2)) |> List.collect sideEffects
    | FunCall(Var fct, args) when Builtin.pureBuiltinFunctions.Contains(fct.Name) -> args |> List.collect sideEffects
    | FunCall(Var fct, args) as e ->
        match fct.Declaration with
        | Declaration.UserFunction fd when not fd.hasExternallyVisibleSideEffects -> args |> List.collect sideEffects
        | _ -> [e]
    | FunCall(Op "?:", [condExpr; thenExpr; elseExpr]) as e ->
        if sideEffects thenExpr = [] && sideEffects elseExpr = []
        then sideEffects condExpr
        else [e] // We could apply sideEffects to thenExpr and elseExpr, but the result wouldn't necessarily have the same type...
    | FunCall(Op op, args) when not(Builtin.assignOps.Contains(op)) -> args |> List.collect sideEffects
    | FunCall(Dot(_, field) as e, args) when field = "length" -> (e :: args) |> List.collect sideEffects
    | FunCall(Subscript _ as e, args) -> (e :: args) |> List.collect sideEffects
    | e -> [e]

let rec isPure e = sideEffects e = []

let varUsesInStmt options stmt = 
    let mutable idents = []
    let collectLocalUses _ = function
        | Var v as e -> idents <- v :: idents; e
        | e -> e
    mapStmt BlockLevel.Unknown (mapEnvExpr options collectLocalUses) stmt |> ignore<MapEnv * Stmt>
    idents

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
            mapStmt BlockLevel.Unknown (mapEnvExpr options collectLocalUses) expr |> ignore<MapEnv * Stmt>
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
    //  - the variable is not used in a sub-block (e.g. inside a loop), for runtime performance
    //  - the init value refers only to variables that are never written to, and functions that are builtin and pure
    let markSafelyInlinableLocals block =
        // Variables that are defined in this scope.
        // The boolean indicate if the variable initialization is const.
        let localDefs = Dictionary<string, (Ident * bool)>()
        // List of all expressions in the current block. Do not look in sub-blocks.
        let mutable localExpr = []
        for stmt: Stmt in block do
            match stmt with
            | Decl (_, declElts) ->
                for def in declElts do
                    // can only inline if it has a value
                    match def.init with
                    | None ->
                        localDefs.[def.name.Name] <- (def.name, true)
                    | Some init ->
                        localExpr <- init :: localExpr
                        let isConst = varUsesInStmt options (Expr init) |> Seq.forall isEffectivelyConst
                        localDefs.[def.name.Name] <- (def.name, isConst)
            | Expr e
            | Jump (_, Some e) -> localExpr <- e :: localExpr
            | Directive _ | Verbatim _ | Jump (_, None) | Block _ | If _| ForE _ | ForD _ | While _ | DoWhile _ | Switch _ -> ()

        let localReferences = countReferences [for e in localExpr -> Expr e]
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
                             | Some init -> isPure init
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
        mapTopLevel (mapEnv options (fun _ -> id) mapStmt) li |> ignore<TopLevel list>
        ()

    let markSimpleInlinableVariables li =
        let mapStmt _ stmt =
            match stmt with
            | Decl d -> markUnwrittenVariablesWithSimpleInit Level.InFunc d
            | ForD (d, _, _, _) -> markUnwrittenVariablesWithSimpleInit Level.InFunc d
            | _ -> ()
            stmt
        // Visit locals
        mapTopLevel (mapEnv options (fun _ -> id) mapStmt) li |> ignore<TopLevel list>
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


let markWrites options topLevel = // calculates hasExternallyVisibleSideEffects, for inlining
    let findWrites (env: MapEnv) = function
        | ResolvedVariableUse (v, vd) as e when env.isInWritePosition ->
            vd.isEverWrittenAfterDecl <- true
            v.isVarWrite <- true
            e
        | FunCall(Var v, args) as e ->
            match v.Declaration with
            | Declaration.UserFunction funcDecl when funcDecl.funcType.hasOutOrInoutParams ->
                // Writes through assignOps are already handled by mapEnv,
                // but we also need to handle variable writes through "out" or "inout" parameters.
                for arg, (ty, _) in List.zip args funcDecl.funcType.args do
                    let newEnv = if ty.isOutOrInout then {env with isInWritePosition = true} else env
                    (mapExpr newEnv arg: Expr) |> ignore<Expr>
                e
            | _ -> e
        | e -> e
    mapTopLevel (mapEnvExpr options findWrites) topLevel |> ignore<TopLevel list>

    let findExternallyVisibleSideEffect tl =
        let mutable hasExternallyVisibleSideEffect = false
        let findExprSideEffects _ = function
            | Var v as e ->
                let hasSideEffect =
                    match v.Declaration with
                    | Declaration.Variable d ->
                        match d.scope with
                        | VarScope.Global -> v.isVarWrite
                        | VarScope.Parameter -> d.ty.isOutOrInout
                        | VarScope.Local -> false
                    // functions are processed in order, so this is initialized before use
                    | Declaration.UserFunction f -> f.hasExternallyVisibleSideEffects
                    | Declaration.BuiltinFunction -> not(Builtin.pureBuiltinFunctions.Contains(v.Name))
                    | _ -> true
                hasExternallyVisibleSideEffect <- hasExternallyVisibleSideEffect || hasSideEffect
                e
            | e -> e
        let findStmtSideEffects _ = function
            // Side effects can hide in macros.
            | (Verbatim _ | Directive _) as s -> hasExternallyVisibleSideEffect <- true; s
            | s -> s
        mapTopLevel (mapEnv options findExprSideEffects findStmtSideEffects) [tl] |> ignore<TopLevel list>
        hasExternallyVisibleSideEffect

    for tl in topLevel do
        match tl with
        | Function (ft, _) ->
            match ft.fName.Declaration with
            | Declaration.UserFunction f ->
                f.hasExternallyVisibleSideEffects <- findExternallyVisibleSideEffect tl
            | _ -> ()
        | _ -> ()

// Create an ident.Declaration for each declaration in the file.
// Give each Ident a reference to that Declaration.
let resolve options topLevel =
    let resolveExpr (env: MapEnv) = function
        | FunCall (Var v, args) as e ->
            v.Declaration <-
                match env.fns.TryFind (v.Name, args.Length) with
                | Some [(ft, _)] -> ft.fName.Declaration
                | None when Builtin.builtinFunctions.Contains v.Name -> Declaration.BuiltinFunction
                | _ -> Declaration.UnknownFunction // TODO: support type-based disambiguation of user-defined function overloading
            e
        | Var v as e ->
            match env.vars.TryFind v.Name with
            | Some (_, decl) -> v.Declaration <- decl.name.Declaration
            | _ -> ()
            e
        | e -> e

    let resolveDecl scope (ty, li) =
        for elt in li do
            let varDecl = new VarDecl(ty, elt, scope)
            elt.name.Declaration <- Declaration.Variable varDecl

    let resolveStmt _ = function
        | Decl d as stmt -> resolveDecl VarScope.Local d; stmt
        | ForD(d, _, _, _) as stmt -> resolveDecl VarScope.Local d; stmt
        | x -> x

    let resolveGlobalsAndParameters = function
        | TLDecl decl -> resolveDecl VarScope.Global decl
        | Function (funcType, _) as tl ->
            for decl in funcType.args do resolveDecl VarScope.Parameter decl
            funcType.fName.Declaration <- Declaration.UserFunction (new FunDecl(tl, funcType))
        | _ -> ()

    // First visit all declarations, creating them.
    for tl in topLevel do
        resolveGlobalsAndParameters tl
    mapTopLevel (mapEnv options (fun _ -> id) resolveStmt) topLevel |> ignore<TopLevel list>
    // Then, visit all uses and associate them to their declaration.
    mapTopLevel (mapEnvExpr options resolveExpr) topLevel |> ignore<TopLevel list>


// findFuncInfos finds the call graph, and other related informations for function inlining.
type CallSite = {
    ident: Ident
    varsInScope: string list
    prototype: string * int
    argExprs: Expr list
}
type FuncInfo = {
    func: TopLevel
    funcType: FunctionType
    body: Stmt
    name: string
    callSites: CallSite list // calls to other user-defined functions, from inside this function.
    isResolvable: bool // Currently we cannot resolve overloaded functions based on argument types.
    isOverloaded: bool
}
let findFuncInfos options code =
    let findCallSites block = // Gets the list of call sites in this function
        let callSites = List()
        let collect (mEnv : MapEnv) = function
            | FunCall (Var id, argExprs) as e ->
                callSites.Add { ident = id; varsInScope = mEnv.vars.Keys |> Seq.toList; prototype = (id.Name, argExprs.Length); argExprs = argExprs }
                e
            | e -> e
        mapStmt BlockLevel.Unknown (mapEnvExpr options collect) block |> ignore<MapEnv * Stmt>
        callSites |> Seq.toList
    let functions = code |> List.choose (function
        | Function(funcType, block) as f -> Some (funcType, funcType.fName.Name, block, f)
        | _ -> None)
    let funcInfos = functions |> List.map (fun ((funcType, name, block, func) as f) ->
        let callSites = findCallSites block
                        // only return calls to user-defined functions
                        |> List.filter (fun callSite -> functions |> List.exists (fun (ft, _, _, _) -> callSite.prototype = ft.prototype))
        let isResolvable = not (functions |> List.except [f] |> List.exists (fun (ft, _, _, _) -> ft.prototype = funcType.prototype))
        let isOverloaded = not (functions |> List.except [f] |> List.exists (fun (ft, _, _, _) -> ft.fName = funcType.fName))
        { FuncInfo.func = func; funcType = funcType; name = name; callSites = callSites; body = block; isResolvable = isResolvable; isOverloaded = isOverloaded })
    funcInfos


type FunctionInlining(options: Options.Options) =

    // To ensure correctness, we verify if it's safe to inline.
    //
    // [A] Only inline a function if it never refers to a global function or variable by a name that is shadowed by a local variable in scope at the call site.
    // [B] Only inline a function if it has only one call site.
    //     Exception: if the body is "trivial" it will be inlined at all call sites.
    // [C] Only inline a function if it is a single expression return.
    //     This also ensures the function does not declare any locals.
    // [D] Only inline a function if it uses its 'in' parameters at most once.
    //     No attempt is made to inline in other cases. For example, it would be correct to inline
    //     when an 'in' parameter is read more than once but is an expression without side-effects,
    //     or when the parameter is written but the argument is a lvalue that doesn't make any side effect and is not used after the call site.
    //     Repeating the expression could increase the shader size or decrease run time performance.
    // [E] Only inline a function if its 'in' parameters are never written to (through assignOps or calling an out or inout function or operator).
    //     No attempt is made to find if the passed argument is an lvalue that's never used after calling the function to inline.
    //     No attempt is made to copy the argument into a newly declared local variable at the call site to get correct writing semantics.
    // [F] Only inline a function if it has no 'out' or 'inout' parameters.
    //     'out' or 'inout' parameters must be lvalues, which simplifies things. But there are problems to watch out for.
    //     Evaluating them could have side effects (e.g. a[b++]), which is a problem if they are used more than once.
    //     If the 'out' parameters are read from, inlining can change the behavior.
    //     It's fine if 'out' parameters are written in more than one place.
    let verifyArgsUses func callSites =
        let argUsageCounts = Dictionary<string, int>()
        let mutable shadowedGlobal = false
        let mutable argIsWritten = false

        let visitArgUses _ = function
            | ResolvedVariableUse (v, vd) as e ->
                match vd.scope with
                | VarScope.Local ->
                    failwith "There shouldn't be any locals in a function with a single return statement."
                | VarScope.Parameter ->
                    argIsWritten <- argIsWritten || vd.isEverWrittenAfterDecl
                    argUsageCounts.[v.Name] <- match argUsageCounts.TryGetValue(v.Name) with _, n -> n + 1
                | VarScope.Global ->
                    shadowedGlobal <- shadowedGlobal || (callSites |> List.exists (fun callSite ->
                        callSite.varsInScope |> List.contains v.Name))
                e
            | e -> e
        mapTopLevel (mapEnvExpr options visitArgUses) [func] |> ignore<TopLevel list>

        let argsAreUsedAtMostOnce = not (argUsageCounts.Values |> Seq.exists (fun n -> n > 1))
        let ok =
            argsAreUsedAtMostOnce && // [D]
            not argIsWritten && // [E]
            not shadowedGlobal // [A]
        ok

    let tryMarkFunctionToInline funcInfo callSites =
        if not funcInfo.funcType.fName.DoNotInline && verifyArgsUses funcInfo.func callSites then
            // Mark both the call site (so that simplifyExpr can remove it) and the function (to remember to remove it).
            // We cannot simply rely on unused functions removal, because it might be disabled through its own flag.
            options.trace $"{funcInfo.funcType.fName.Loc}: inlining function '{Printer.debugFunc funcInfo.funcType}' into {callSites.Length} call sites"
            for callSite in callSites do
                callSite.ident.ToBeInlined <- true
            funcInfo.funcType.fName.ToBeInlined <- true

    let markInlinableFunctions code =
        let funcInfos = findFuncInfos options code
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
