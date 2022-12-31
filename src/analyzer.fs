module Analyzer

open System.Collections.Generic
open Ast
open Options.Globals

// The module performs some static analysis on the code and stores the
// information in the AST nodes, e.g. find which variables are modified,
// which declarations can be inlined.


// Return the list of variables used in the statements, with the number of references.
let collectReferences stmtList =
    let count = Dictionary<string, int>()
    let collectLocalUses _ = function
        | Var v as e ->
            match count.TryGetValue(v.Name) with
            | true, n -> count.[v.Name] <- n + 1
            | false, _ -> count.[v.Name] <- 1
            e
        | e -> e
    for expr in stmtList do
        mapStmt (mapEnv collectLocalUses id) expr |> ignore
    count

// Mark variables as inlinable when possible.
// Variables are always safe to inline when all of:
//  - the variable is used only once in the current block
//  - the variable is not used in a sub-block (e.g. inside a loop)
//  - the init value refers only to constants
// When aggressive inlining is enabled, additionally inline when all of:
//  - the variable never appears in an lvalue position (is never written to
//    after initalization)
//  - the init value is has no dependency
// The init is considered trivial when:
//  - it doesn't depend on a variable
//  - it depends only on variables proven constants
let findInlinable block =
    // Variables that are defined in this scope.
    // The booleans indicate if the variable initialization has dependencies / unsafe dependencies.
    let localDefs = Dictionary<string, (Ident * bool * bool)>()
    // List of expressions in the current block. Do not look in sub-blocks.
    let mutable localExpr = []
    for stmt: Stmt in block do
        match stmt with
        | Decl (_, li) ->
            for def in li do
                // can only inline if it has a value
                match def.init with
                | None -> ()
                | Some init ->
                    localExpr <- init :: localExpr
                    let deps = collectReferences [Expr init]
                    let hasUnsafeDep = deps |> Seq.exists (fun kv ->
                        if localDefs.ContainsKey kv.Key then
                            // A local variable not reassigned is effectively constant.
                            let ident, _, _ = localDefs.[kv.Key]
                            ident.IsLValue
                        else
                            true
                    )
                    localDefs.[def.name.Name] <- (def.name, deps.Count > 0, hasUnsafeDep)
        | Expr e
        | Jump (_, Some e) -> localExpr <- e :: localExpr
        | Verbatim _ | Jump (_, None) | Block _ | If _| ForE _ | ForD _ | While _ | DoWhile _ | Switch _ -> ()

    let localReferences = collectReferences [for e in localExpr -> Expr e]
    let allReferences = collectReferences block

    for def in localDefs do
        let ident, hasInitDeps, hasUnsafeDeps = def.Value
        if not ident.ToBeInlined && not ident.IsLValue then
            // AggroInlining could in theory do inlining when hasUnsafeDeps=false.
            // However, it seems to increase the compressed size, and might affect performance.
            if options.aggroInlining && not hasInitDeps then
                ident.ToBeInlined <- true

            match localReferences.TryGetValue(def.Key), allReferences.TryGetValue(def.Key) with
            | (true, 1), (true, 1) when not hasUnsafeDeps -> ident.ToBeInlined <- true
            | (false, _), (false, _) -> ident.ToBeInlined <- true
            | _ -> ()

let maybeInlineConsts li =
    let mapInnerDecl = function
        | ({typeQ = tyQ}, defs) as d when List.contains "const" tyQ ->
            for (def:DeclElt) in defs do
                // AggroInlining: unconditional inlining of anything marked "const".
                // Note: this is unsafe if the init value depends on something mutable.
                if options.aggroInlining then
                    def.name.ToBeInlined <- true
                // Otherwise, inline only trivial constants.
                else match def.init with
                        | Some (Var v) when v.Name = "true" || v.Name = "false" ->
                            def.name.ToBeInlined <- true
                        | Some (Int _)
                        | Some (Float _) ->
                            def.name.ToBeInlined <- true
                        | _ -> ()
            d
        | d -> d
    let mapStmt = function
        | Decl d -> Decl (mapInnerDecl d)
        | s -> s
    let mapExpr _ e = e
    let mapTLDecl = function
        | TLDecl d -> TLDecl (mapInnerDecl d)
        | d -> d
    li
    |> mapTopLevel (mapEnv mapExpr mapStmt)
    |> List.map mapTLDecl

let markLValues li =

    let findWrites (env: MapEnv) = function
        | Var v as e when env.isLValue ->
            match env.vars.TryFind v.Name with
            | Some (_, {name = vv}) -> vv.IsLValue <- true; e
            | _ -> e
        | FunCall(Var v, args) as e ->
            match env.fns.TryFind v.Name with
            | Some (fct, _) when fct.fName.IsLValue ->
                let newEnv = {env with isLValue = true}
                for arg in args do
                    (mapExpr newEnv arg: Expr) |> ignore
                e
            | _ -> e
        | e -> e

    // Mark functions.

    // If any parameter to the function could be written to (e.g., "out"), mark
    // the entire function. We don't attempt to match up param-for-param but
    // just mark everything if anything could write, for simplicity.
    let assignQuals = Set.ofList ["out"; "inout"]
    for tl in li do
        match tl with
        | Function({fName = id; args = args}, _) ->
            let argAssigns (ty, _) =
                List.exists (fun tyQ -> Set.contains tyQ assignQuals) ty.typeQ
            if List.exists argAssigns args then id.IsLValue <- true
        | _ -> ()

    // Mark bodies; when scanning bodies, we need to look up which functions
    // might write via "out" parameters.

    mapTopLevel (mapEnv findWrites id) li
