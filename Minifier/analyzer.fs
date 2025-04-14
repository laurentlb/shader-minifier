module internal Analyzer

open System.Collections.Generic
open Ast

// We can visit Var uses in evaluation order (sometimes twice: read then write),
// and we know if they're read and/or written (by assignment operators or by in/out),
// if it's a field access, and if it's a declaration initialization.

type VarUse = {
    access: Access
    isFieldAccess: bool
    isDecl: bool
} with
    override this.ToString() = $"{this.access}" + (if this.isFieldAccess then " field" else "") + (if this.isDecl then " decl" else " use")

[<NoComparison; NoEquality>]
type VarVisitor(onVarUse: VarUse -> Ident -> VarDecl -> unit) =

    member this.onVisitVar = function
        | ResolvedVariableUse (v, vd) -> onVarUse this.varUse v vd
        | _ -> ()

    member val varUse: VarUse = { access = { isWrite = false; isRead = true }; isFieldAccess = false; isDecl = false } with get, set

    member this.using newContext go =
        let old = this.varUse
        this.varUse <- newContext
        try
            go()
        finally
            this.varUse <- old

    // Vars in the AST can be a few things:
    // [A] A declaration without initialization is neither a read nor a write.            int x;
    // [B] A declaration with initialization is a "isDecl" write.                         int x = 1;
    // [C] An assignment is a write.                                                      x = 1;
    // [D] An augmented assignment is a read and then a write.                            x += 1;
    // [E] A function name itself can contain a variable read                             x.length();
    // [F] An "in" parameter in a function call is a read.                                sin(x);
    // [G] An "out" parameter in a function call is a write.                              void f(out p) { p = 1; } ... f(x);
    // [H] An "inout" parameter in a function call is a read later followed by a write.   void f(inout p) { p += 1; } ... f(x);
    //   Note that even when passing a global as out or inout, no aliasing happens.
    //   The function works on a local copy, and copies back the value when exiting.
    // [I] Other stray var uses are reads.

    member this.visitExpr = function
        | FunCall(Op op as fct, first::args) when Builtin.assignOps.Contains op ->
            let alsoReadTheVar = op <> "=" // Augmented assignment or ++ or --
            if alsoReadTheVar then
                // Visit the lhs of the assignment as a read. [D]
                this.using { this.varUse with access.isWrite = false; access.isRead = true } (fun () ->
                    this.visitExpr first
                )
            List.iter this.visitExpr args // visit the rhs of the assignments
            this.visitExpr fct // visit the assignment op
            // Visit the lhs of the assignment as a write. [C] [D]
            this.using { this.varUse with access.isWrite = true; access.isRead = false } (fun () ->
                this.visitExpr first
            )
        | FunCall(fct, args) ->
            this.using { this.varUse with access.isWrite = false; access.isRead = true } (fun () -> // [E]
                this.visitExpr fct
            )
            // Handle in/out/inout parameters.
            let funcDecl =
                match fct with 
                | Var v -> 
                    match v.Declaration with
                    | Declaration.UserFunction funcDecl -> Some funcDecl
                    | _ -> None
                | _ -> None
            let paramAccessList =
                match funcDecl with
                | Some funcDecl -> funcDecl.funcType.args |> List.map (fun (ty, _) -> ty.access)
                | None -> args |> List.map (fun _ -> { isWrite = false; isRead = true })

            for arg, access in List.zip args paramAccessList do
                this.using { this.varUse with access = access } (fun () -> // [F] [G] [H]
                    this.visitExpr arg
                )
        | Subscript(arr, ind) ->
            this.visitExpr arr
            this.using { this.varUse with access.isWrite = false; access.isRead = true } (fun () -> // [I]
                Option.iter this.visitExpr ind
            )
        | Dot(expr, _field) ->
            this.using { this.varUse with isFieldAccess = true } (fun () ->
                this.visitExpr expr
            )
        | Cast(_, expr) -> this.visitExpr expr // The ident in a cast is not a Var.
        | VectorExp(li) -> List.iter this.visitExpr li
        | Var _ as e -> this.onVisitVar e
        | _ -> ()

    member this.visitDecl (_ty, declElts) =
        for declElt in declElts do
            // Visit the init expr first.
            this.using { this.varUse with access.isWrite = false; access.isRead = true } (fun () -> // [I]
                Option.iter this.visitExpr declElt.init
            )
            // Then visit the declared var (maybe as a write).
            this.using { this.varUse with isDecl = true } (fun () ->
                this.using { this.varUse with access.isWrite = declElt.init <> None; access.isRead = false } (fun () -> // [A] [B]
                    this.onVisitVar (Var declElt.name)
                )
            )

    member this.visitStmt stmt =
        this.using { this.varUse with access.isWrite = false; access.isRead = true } (fun () -> // [I]
            match stmt with
            | Block stmts -> List.iter this.visitStmt stmts
            | Expr e -> this.visitExpr e
            | Decl d -> this.visitDecl d
            | If(cond, th, el) ->
                this.visitExpr cond
                this.visitStmt th
                el |> Option.iter this.visitStmt
            | While(cond, body) ->
                this.visitExpr cond
                this.visitStmt body
            | DoWhile(cond, body) ->
                this.visitExpr cond
                this.visitStmt body
            | ForD(init, cond, inc, body) ->
                this.visitDecl init
                Option.iter this.visitExpr cond
                Option.iter this.visitExpr inc
                this.visitStmt body
            | ForE(init, cond, inc, body) ->
                Option.iter this.visitExpr init
                Option.iter this.visitExpr cond
                Option.iter this.visitExpr inc
                this.visitStmt body
            | Jump(_, e) -> Option.iter this.visitExpr e
            | (Verbatim _ | Directive _) -> ()
            | Switch(e, cl) ->
                this.visitExpr e
                let iterCase (l, sl) =
                    match l with
                    | Case e -> this.visitExpr e
                    | Default -> ()
                    List.iter this.visitStmt sl
                List.iter iterCase cl
        )

    member this.visitTopLevels = List.iter (function
        | TLDecl t -> this.visitDecl t
        | Function(fct, body) ->
            List.iter this.visitDecl fct.args
            this.visitStmt body
        | _ -> ())


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
        | FunCall(Dot(_, field) as e, args) when field.Name = "length" -> (e :: args) |> List.collect sideEffects
        | FunCall(Subscript _ as e, args) -> (e :: args) |> List.collect sideEffects
        | e -> [e]

    let rec isPure e = sideEffects e = []


// The Analyzer module performs some static analysis on the code and stores the
// information in the AST nodes, e.g. find which variables are modified,
// which declarations can be inlined.

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

[<System.Flags>]
type IdentKind =
    | Var = 0b1
    | Field = 0b10
    | Type = 0b100

type Analyzer(options: Options.Options) =

    // findFuncInfos finds the call graph, and other related information for function inlining.
    member _.findFuncInfos code =
        let findCallSites block = // Gets the list of call sites in this function
            let callSites = List()
            let collect (mEnv : MapEnv) = function
                | FunCall (Var id, argExprs) as e ->
                    callSites.Add { ident = id; varsInScope = mEnv.vars.Keys |> Seq.toList; prototype = (id.Name, argExprs.Length); argExprs = argExprs }
                    e
                | e -> e
            options.visitor(collect).iterStmt BlockLevel.Unknown block
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

    member _.identUsesInStmt (kind: IdentKind) stmt =
        let mutable idents = []
        let collectLocalUses _ = function
            | Var v as e when kind.HasFlag IdentKind.Var -> idents <- v :: idents; e
            | Dot (_, field) as e when kind.HasFlag IdentKind.Field -> idents <- field :: idents; e
            | e -> e
        let collectLocalUsesInStmt _ = function
            | Decl ({ name = TypeName typeName}, _) as e when kind.HasFlag IdentKind.Type ->
                idents <- typeName :: idents
                e
            | e -> e
        options.visitor(collectLocalUses, collectLocalUsesInStmt).iterStmt BlockLevel.Unknown stmt
        idents

    // recalculates hasExternallyVisibleSideEffects/isVarWrite/isEverWrittenAfterDecl, for inlining
    member _.markWrites topLevel =
        let findWrites varUse (var: Ident) (vd: VarDecl) =
            let isWriteAfterDecl = varUse.access.isWrite && not varUse.isDecl
            if isWriteAfterDecl then
                // this is initially set to false when `resolve` creates all Declarations
                vd.isEverWrittenAfterDecl <- true
            var.isVarWrite <- isWriteAfterDecl
        VarVisitor(findWrites).visitTopLevels topLevel

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
            options.visitor(findExprSideEffects, findStmtSideEffects).iterTopLevel [tl]
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
                let varDecl = VarDecl(ty, elt, scope)
                elt.name.Declaration <- Declaration.Variable varDecl

        let resolveStmt _ = function
            | Decl d as stmt -> resolveDecl VarScope.Local d; stmt
            | ForD(d, _, _, _) as stmt -> resolveDecl VarScope.Local d; stmt
            | x -> x

        let resolveGlobalsAndParameters = function
            | TLDecl decl -> resolveDecl VarScope.Global decl
            | Function (funcType, _) as tl ->
                for decl in funcType.args do resolveDecl VarScope.Parameter decl
                funcType.fName.Declaration <- Declaration.UserFunction (FunDecl(tl, funcType))
            | _ -> ()

        // First visit all declarations, creating them.
        for tl in topLevel do
            resolveGlobalsAndParameters tl
        options.visitor(fStmt = resolveStmt).iterTopLevel topLevel
        // Then, visit all uses and associate them to their declaration.
        options.visitor(resolveExpr).iterTopLevel topLevel

