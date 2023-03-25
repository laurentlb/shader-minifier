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

let trigonometryFunctions = set([
    "acos"; "acosh"; "asin"; "asinh"; "atan"; "atanh"; "cos"; "cosh"; "degrees";
    "radians"; "sin"; "sinh"; "tan"; "tanh"])
let mathsFunctions = set([
    "abs"; "ceil"; "clamp"; "dFdx"; "dFdy"; "exp"; "exp2"; "floor"; "floor"; "fma";
    "fract"; "fwidth"; "inversesqrt"; "isinf"; "isnan"; "log"; "log2"; "max"; "min";
    "mix"; "mod"; "modf"; "noise"; "pow"; "round"; "roundEven"; "sign"; "smoothstep";
    "sqrt"; "step"; "trunc"])
let vectorFunctions = set([
    "cross"; "distance"; "dot"; "equal"; "faceforward"; "length"; "normalize";
    "notEqual"; "reflect"; "refract"])
let builtinTypes = set([
    yield! [ "void"; "bool"; "int"; "uint"; "float"; "double" ]
    for p in ["d"; "b"; "i"; "u"] do
        for n in ["2"; "3"; "4"] do
            yield p+"vec"+n
    for p in [""; "d"] do
        for n in ["2"; "3"; "4"] do
            yield p+"mat"+n
        for c in ["2"; "3"; "4"] do
            for r in ["2"; "3"; "4"] do
                yield p+"mat"+c+"x"+r
    ])
let castFunctions = builtinTypes - set ["void"]

let pureBuiltinFunctions = trigonometryFunctions + mathsFunctions + vectorFunctions + castFunctions

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
let markInlinableVariables block =
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
                            not (pureBuiltinFunctions.Contains kv.Key)
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
            | Some (_, {name = vv}, _) -> vv.IsLValue <- true; e
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



module private FindInlinableFunctions =

    type CallSite = {
        ident: Ident
        varsInScope: string list
        argCount: int
    }
    type FuncInfo = {
        func: TopLevel
        funcType: FunctionType
        body: Stmt
        name: string
        callSites: CallSite list
    }

    // get the list of external values the block depends on
    let computeDependencies block =
        let d = List()
        let collect (mEnv : MapEnv) = function
            | FunCall (Var id, argExprs) as e ->
                if not (mEnv.vars.ContainsKey(id.Name)) then // mEnv.fns is empty because we're only visiting this function.
                    d.Add { ident = id; varsInScope = mEnv.vars.Keys |> Seq.toList; argCount = argExprs.Length }
                e
            | e -> e
        mapStmt (mapEnv collect id) block |> ignore
        d |> Seq.toList

    // This function assumes that functions are NOT overloaded
    let computeAllDependencies code =
        let functions = code |> List.choose (function
            | Function(funcType, block) as f -> Some (funcType, funcType.fName.Name, block, f)
            | _ -> None)
        let nodes = functions |> List.map (fun (funcType, name, block, f) ->
            let callSites = computeDependencies block
                            |> List.filter (fun callSite -> functions |> List.exists (fun (_,n,_,_) -> callSite.ident.Name = n))
            { FuncInfo.func = f; funcType = funcType; name = name; callSites = callSites; body = block })
        nodes

    // To ensure correctness, we verify if it's safe to inline.
    //
    // [A] Only inline a function if it never refers to a global by a name function or variable that is shadowed by a local variable in scope at the call site.
    // [B] Only inline a function if it has only one call site.
    //     No attempt is made to detect if the function is "short enough" that it could
    //     benefit from inlining at multiple call sites (particularly probable with compression).
    // [C] Only inline a function if it is a single expression return.
    //     This also ensures the function does not declare any locals.
    // [D] Only inline a function if it uses its 'in' parameters at most once.
    //     It would be correct to inline when an 'in' parameter that is used more than once
    //     is passed as an lvalue that doesn't evaluate any expression with a side-effect (e.g. a[b++]).
    //     If the lvalue is long enough (e.g. `a[b].c.zyx`), its inlined repetition could also increase the shader size.
    // [E] Only inline a function if its 'in' parameters are never written to (through assignOps or calling an out or inout function or operator).
    //     No attempt is made to find if the passed argument is an lvalue that's never used after calling the function to inline.
    //     No attempt is made to copy the argument into a newly declared local variable at the call site to get correct writing semantics.
    // [F] Only inline a function if it has no 'out' or 'inout' parameters.
    //     'out' or 'inout' parameters must be lvalues, which simplifies things. But there are problems to watch out for.
    //     Evaluating them could have side effects (e.g. a[b++]), which is a problem if they are used more than once.
    //     If the 'out' parameters are read from, inlining can change the behavior.
    //     It's fine if 'out' parameters are written in more than one place.
    let verifyArgsUses func callSite code =
        let argUsageCounts = Dictionary<string, int>()
        let mutable shadowedGlobal = false
        let mutable argIsWritten = false

        let visitArgUses mEnv = function
            | Var id as e ->
                match mEnv.vars.TryFind(id.Name) with
                | Some (_, _, VarScope.Local) ->
                    failwith "There shouldn't be any locals in a function with a single return statement."
                | Some (_, _, VarScope.Parameter) ->
                    argIsWritten <- argIsWritten || mEnv.isLValue
                    argUsageCounts.[id.Name] <- match argUsageCounts.TryGetValue(id.Name) with _, n -> n + 1
                | Some (_, _, VarScope.Global) ->
                    // Here we're only visiting one function, so mEnv.vars contains no globals, and mEnv.fns is empty.
                    failwith "There shouldn't be any globals when visiting a single function."
                | None -> // function name, or global variable name.
                    let isGlobalVarOrFunc = code |> List.exists (function
                        | TLDecl(_, declElts) -> declElts |> List.exists (fun declElt -> declElt.name.Name = id.Name)
                        | Function(fct, _) -> fct.fName.Name = id.Name
                        | _ -> false // built-in function
                        )
                    if isGlobalVarOrFunc then
                        shadowedGlobal <- shadowedGlobal || (callSite.varsInScope |> List.contains id.Name)
                    ()
                e
            | e -> e
        mapTopLevel (mapEnv visitArgUses id) [func] |> ignore

        let argsAreUsedAtMostOnce = not (argUsageCounts.Values |> Seq.exists (fun n -> n > 1))
        let ok =
            argsAreUsedAtMostOnce && // [D]
            not argIsWritten && // [E]
            not shadowedGlobal // [A]
        ok

    let tryMarkFunctionToInline node callSite code =
        // We also need to check that inlining won't fail (inlining can fail because it can be forced by "i_").
        if node.funcType.args.Length = callSite.argCount then
            if verifyArgsUses node.func callSite code then
                // Mark both the call site (so that simplifyExpr can remove it) and the function (to remember to remove it).
                // We cannot simply rely on unused functions removal, because it might be disabled through its own flag.
                callSite.ident.ToBeInlined <- true
                node.funcType.fName.ToBeInlined <- true

    let markInlinableFunctions code =
        let nodes = computeAllDependencies code
        for node in nodes do
            let canBeRenamed = not (options.noRenamingList |> List.contains node.name) // noRenamingList includes "main"
            let isExternal = options.hlsl && node.funcType.semantics <> []
            if canBeRenamed && not isExternal then
                let hasOnlyInParameters =
                    let typeQualifiers = set (List.concat [for (ty, _) in node.funcType.args do yield ty.typeQ])
                    (Set.intersect typeQualifiers (set ["out"; "inout"])).IsEmpty
                if hasOnlyInParameters then // [F]
                    let callSites = nodes |> List.map (fun n -> n.callSites) |> List.concat |> List.filter (fun n -> n.ident.Name = node.name)
                    let isCalledExactlyOnce = callSites.Length = 1
                    if isCalledExactlyOnce then // [B]
                        match node.body with
                        | Jump (JumpKeyword.Return, Some _)
                        | Block [Jump (JumpKeyword.Return, Some _)] -> // [C]
                            tryMarkFunctionToInline node callSites.Head code
                        | _ -> ()

let markInlinableFunctions = FindInlinableFunctions.markInlinableFunctions
