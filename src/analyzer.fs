module internal Analyzer

open System.Collections.Generic
open Ast

// The module performs some static analysis on the code and stores the
// information in the AST nodes, e.g. find which variables are modified,
// which declarations can be inlined.

module Effects =

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

type Analyzer(options: Options.Options) =

    // findFuncInfos finds the call graph, and other related informations for function inlining.
    member _.findFuncInfos code =
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

    member _.varUsesInStmt stmt = 
        let mutable idents = []
        let collectLocalUses _ = function
            | Var v as e -> idents <- v :: idents; e
            | e -> e
        mapStmt BlockLevel.Unknown (mapEnvExpr options collectLocalUses) stmt |> ignore<MapEnv * Stmt>
        idents

    member _.markWrites topLevel = // calculates hasExternallyVisibleSideEffects, for inlining
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
        iterTopLevel (mapEnvExpr options findWrites) topLevel

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
            iterTopLevel (mapEnv options findExprSideEffects findStmtSideEffects) [tl]
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
    member _.resolve topLevel =
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
        iterTopLevel (mapEnv options (fun _ -> id) resolveStmt) topLevel
        // Then, visit all uses and associate them to their declaration.
        iterTopLevel (mapEnvExpr options resolveExpr) topLevel
