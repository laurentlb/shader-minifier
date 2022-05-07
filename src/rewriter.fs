module Rewriter

open System
open System.Collections.Generic
open Ast
open Options.Globals

                                (* ** Rewrite tricks ** *)


let renameField field =
    let transform = function
        | 'r' | 'x' | 's' -> options.canonicalFieldNames.[0]
        | 'g' | 'y' | 't' -> options.canonicalFieldNames.[1]
        | 'b' | 'z' | 'p' -> options.canonicalFieldNames.[2]
        | 'a' | 'w' | 'q' -> options.canonicalFieldNames.[3]
        | c -> failwithf "Internal error: transform('%c')" c
    if Seq.forall (fun c -> Seq.exists ((=) c) "rgba") field ||
        Seq.forall (fun c -> Seq.exists ((=) c) "xyzw") field ||
        Seq.forall (fun c -> Seq.exists ((=) c) "stpq") field
    then
        field |> String.map transform
    else field

// Remove useless spaces in macros
let private stripSpaces str =
    let result = Text.StringBuilder()

    let mutable last = '\n'
    let write c =
        last <- c
        result.Append(c) |> ignore
    let isId c = Char.IsLetterOrDigit c || c = '_' || c = '('
    // hack because we can't remove space in "#define foo (1+1)"

    let mutable space = false
    let mutable macro = false
    for c in str do
        if c = '\n' then
            if macro then write '\n'
            else space <- true
            macro <- false

        elif Char.IsWhiteSpace(c) then
            space <- true
        else
            if not macro && c = '#' then
                macro <- true
                if last <> '\n' then write '\n'

            if space && isId c && isId last then
                write ' '
            write c
            space <- false

    if macro then result.Append("\n") |> ignore
    result.ToString()


let private declsNotToInline (d: Ast.DeclElt list) = d |> List.filter (fun x -> not x.name.ToBeInlined)

let private bool = function
    | true -> Var (Ident "true") // Int (1, "")
    | false -> Var (Ident "false") // Int (0, "")

let rec private simplifyExpr env = function
    | FunCall(Op "-", [Int (i1, su)]) -> Int (-i1, su)
    | FunCall(Op "-", [FunCall(Op "-", [e])]) -> e
    | FunCall(Op "+", [e]) -> e

    | FunCall(Op ",", [e1; FunCall(Op ",", [e2; e3])]) ->
        FunCall(Op ",", [simplifyExpr env (FunCall(Op ",", [e1; e2])); e3])

    | FunCall(Op "-", [x; Float (f, s)]) when f < 0.M ->
        FunCall(Op "+", [x; Float (-f, s)]) |> simplifyExpr env
    | FunCall(Op "-", [x; Int (i, s)]) when i < 0 ->
        FunCall(Op "+", [x; Int (-i, s)]) |> simplifyExpr env

    // Boolean simplifications (let's ignore the suffix)
    | FunCall(Op "<",  [Int (i1, _); Int (i2, _)]) -> bool(i1 < i2)
    | FunCall(Op ">",  [Int (i1, _); Int (i2, _)]) -> bool(i1 > i2)
    | FunCall(Op "<=", [Int (i1, _); Int (i2, _)]) -> bool(i1 <= i2)
    | FunCall(Op ">=", [Int (i1, _); Int (i2, _)]) -> bool(i1 <= i2)
    | FunCall(Op "==", [Int (i1, _); Int (i2, _)]) -> bool(i1 = i2)
    | FunCall(Op "!=", [Int (i1, _); Int (i2, _)]) -> bool(i1 <> i2)

    | FunCall(Op "<", [Float (i1,_); Float (i2,_)]) -> bool(i1 < i2)
    | FunCall(Op ">", [Float (i1,_); Float (i2,_)]) -> bool(i1 > i2)
    | FunCall(Op "<=", [Float (i1,_); Float (i2,_)]) -> bool(i1 <= i2)
    | FunCall(Op ">=", [Float (i1,_); Float (i2,_)]) -> bool(i1 <= i2)
    | FunCall(Op "==", [Float (i1,_); Float (i2,_)]) -> bool(i1 = i2)
    | FunCall(Op "!=", [Float (i1,_); Float (i2,_)]) -> bool(i1 <> i2)

    // Stupid simplifications (they can be useful to simplify rewritten code)
    | FunCall(Op "/", [e; Float (1.M,_)]) -> e
    | FunCall(Op "*", [e; Float (1.M,_)]) -> e
    | FunCall(Op "*", [Float (1.M,_); e]) -> e
    | FunCall(Op "*", [_; Float (0.M,_) as e]) -> e
    | FunCall(Op "*", [Float (0.M,_) as e; _]) -> e
    | FunCall(Op "+", [e; Float (0.M,_)]) -> e
    | FunCall(Op "+", [Float (0.M,_); e]) -> e
    | FunCall(Op "-", [e; Float (0.M,_)]) -> e
    | FunCall(Op "-", [Float (0.M,_); e]) -> FunCall(Op "-", [e])

    // No simplification when numbers have different suffixes
    | FunCall(_, [Int (_, su1); Int (_, su2)]) as e when su1 <> su2 -> e
    | FunCall(_, [Float (_, su1); Float (_, su2)]) as e when su1 <> su2 -> e

    | FunCall(Op "-", [Int (i1, su); Int (i2, _)]) -> Int (i1 - i2, su)
    | FunCall(Op "+", [Int (i1, su); Int (i2, _)]) -> Int (i1 + i2, su)
    | FunCall(Op "*", [Int (i1, su); Int (i2, _)]) -> Int (i1 * i2, su)
    | FunCall(Op "/", [Int (i1, su); Int (i2, _)]) -> Int (i1 / i2, su)
    | FunCall(Op "%", [Int (i1, su); Int (i2, _)]) -> Int (i1 % i2, su)

    | FunCall(Op "-", [Float (0.M,su)]) -> Float (0.M, su)
    | FunCall(Op "-", [Float (f1,su)]) -> Float (-f1, su)
    | FunCall(Op "-", [Float (i1,su); Float (i2,_)]) -> Float (i1 - i2, su)
    | FunCall(Op "+", [Float (i1,su); Float (i2,_)]) -> Float (i1 + i2, su)
    | FunCall(Op "*", [Float (i1,su); Float (i2,_)]) -> Float (i1 * i2, su)
    | FunCall(Op "/", [Float (i1,su); Float (i2,_)]) as e ->
        let div = Float (i1 / i2, su)
        if (Printer.exprToS e).Length <= (Printer.exprToS div).Length then e
        else div

    // iq's smoothstep trick: http://www.pouet.net/topic.php?which=6751&page=1#c295695
    | FunCall(Var var, [Float (0.M,_); Float (1.M,_); _]) as e when var.Name = "smoothstep" -> e
    | FunCall(Var var, [a; b; x]) when var.Name = "smoothstep" && options.smoothstepTrick ->
        let sub1 = FunCall(Op "-", [x; a])
        let sub2 = FunCall(Op "-", [b; a])
        let div  = FunCall(Op "/", [sub1; sub2]) |> mapExpr env
        FunCall(Var (Ident "smoothstep"),  [Float (0.M,""); Float (1.M,""); div])

    | Dot(e, field) when options.canonicalFieldNames <> "" -> Dot(e, renameField field)

    | Var s as e ->
        match env.vars.TryFind s.Name with
        | Some (_, {name = id; init = Some init}) when id.ToBeInlined -> init |> mapExpr env
        | _ -> e

    // pi is acos(-1), pi/2 is acos(0)
    | Float(f, _) when float f = 3.141592653589793 -> FunCall(Var (Ident "acos"), [Float (-1.M, "")])
    | Float(f, _) when float f = 6.283185307179586 -> FunCall(Op "*", [Float (2.M, ""); FunCall(Var (Ident "acos"), [Float (-1.M, "")])])
    | Float(f, _) when float f = 1.5707963267948966 -> FunCall(Var (Ident "acos"), [Float (0.M, "")])

    | e -> e

// Squeeze declarations: "float a=2.; float b;" => "float a=2.,b;"
let rec private squeezeDeclarations = function
    | []-> []
    | Decl(ty1, li1) :: Decl(ty2, li2) :: l when ty1 = ty2 ->
        squeezeDeclarations (Decl(ty1, li1 @ li2) :: l)
    | e::l -> e :: squeezeDeclarations l

// Squeeze top-level declarations, e.g. uniforms
let rec private squeezeTLDeclarations = function
    | []-> []
    | TLDecl(ty1, li1) :: TLDecl(ty2, li2) :: l when ty1 = ty2 ->
        squeezeTLDeclarations (TLDecl(ty1, li1 @ li2) :: l)
    | e::l -> e :: squeezeTLDeclarations l

let private rwTypeSpec = function
    | TypeName n -> TypeName (stripSpaces n)
    | x -> x // structs

let rwType (ty: Type) =
    makeType (rwTypeSpec ty.name) (List.map stripSpaces ty.typeQ)

let rwFType fct =
    // The default for function parameters is "in", we don't need it.
    let rwFTypeType ty = {ty with typeQ = List.except ["in"] ty.typeQ}
    let rwFDecl (ty, elts) = (rwFTypeType ty, elts)
    {fct with args = List.map rwFDecl fct.args}

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
// For now, only mark a variable when:
//  - the variable is used only once in the current block
//  - the variable is not used in a sub-block (e.g. inside a loop)
//  - the init value is trivial (doesn't depend on a variable)
let findInlinable foundInlinable block =
    // Variables that are defined in this scope.
    // The boolean indicates if the variable initialization has dependencies.
    let localDefs = Dictionary<string, (Ident * bool)>()
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
                    // Inline only if the init value doesn't depend on other variables.
                    let deps = collectReferences [Expr init]
                    localDefs.[def.name.Name] <- (def.name, deps.Count > 0)
        | Expr e
        | Jump (_, Some e) -> localExpr <- e :: localExpr
        | Verbatim _ | Jump (_, None) | Block _ | If _| ForE _ | ForD _ | While _ | DoWhile _ | Switch _ -> ()

    let localReferences = collectReferences (List.map Expr localExpr)
    let allReferences = collectReferences block
    
    for def in localDefs do
        let ident, hasInitDeps = def.Value
        if not ident.ToBeInlined then
            match localReferences.TryGetValue(def.Key), allReferences.TryGetValue(def.Key) with
            | (true, 1), (true, 1) when not hasInitDeps -> ident.Inline(); foundInlinable := true
            | (false, _), (false, _) -> ident.Inline(); foundInlinable := true
            | _ -> ()

let private simplifyStmt = function
    | Block [] as e -> e
    | Block b ->
        // Remove dead code after return/break/...
        let endOfCode = Seq.tryFindIndex (function Jump _ -> true | _ -> false) b
        let b = match endOfCode with None -> b | Some x -> b |> Seq.truncate (x+1) |> Seq.toList

        // Remove inner empty blocks
        let b = b |> List.filter (function Block [] | Decl (_, []) -> false | _ -> true)
        
        // Try to remove blocks by using the comma operator
        let returnExp = b |> Seq.tryPick (function Jump(JumpKeyword.Return, e) -> e | _ -> None)
        let canOptimize = b |> List.forall (function
            | Expr _ -> true
            | Jump(JumpKeyword.Return, Some _) -> true
            | _ -> false)

        if not options.noSequence && canOptimize then
            let li = List.choose (function Expr e -> Some e | _ -> None) b
            match returnExp with
            | None ->
                if li.IsEmpty then Block []
                else Expr (List.reduce (fun acc x -> FunCall(Op ",", [acc;x])) li)
            | Some e ->
               let expr = List.reduce (fun acc x -> FunCall(Op ",", [acc;x])) (li@[e])
               Jump(JumpKeyword.Return, Some expr)
        else
            match squeezeDeclarations b with
            | [stmt] -> stmt
            | stmts -> Block stmts
    | Decl (ty, li) -> Decl (rwType ty, declsNotToInline li)
    | ForD((ty, d), cond, inc, body) -> ForD((rwType ty, declsNotToInline d), cond, inc, body)
    // FIXME: properly handle booleans
    | If(Var var, e1, _) when var.Name = "true" -> e1
    | If(Var var, _, Some e2) when var.Name = "false" -> e2
    | If(Var var, _, None) when var.Name = "false" -> Block []
    | If(c, b, Some (Block [])) -> If(c, b, None)
    | Verbatim s -> Verbatim (stripSpaces s)
    | e -> e

let reorderTopLevel t =
    if options.reorderDeclarations then
        let externals, functions = List.partition (function TLDecl _ -> true | _ -> false) t
        List.sort externals @ functions
    else
        t

let rec iterateSimplifyAndInline li =
    let foundInlinable =
        if options.noInlining then
            false
        else
            let foundInlinableRef = ref false
            let mapExpr _ e = e
            let mapStmt = function
                | Block b as e -> findInlinable foundInlinableRef b; e
                | e -> e
            mapTopLevel (mapEnv mapExpr mapStmt) li |> ignore
            !foundInlinableRef
    let simplified = mapTopLevel (mapEnv simplifyExpr simplifyStmt) li
    if foundInlinable then iterateSimplifyAndInline simplified else simplified

let simplify li =
    li
    |> reorderTopLevel
    |> iterateSimplifyAndInline
    |> List.map (function
        | TLDecl (ty, li) -> TLDecl (rwType ty, declsNotToInline li)
        | TLVerbatim s -> TLVerbatim (stripSpaces s)
        | Function (fct, body) -> Function (rwFType fct, body)
        | e -> e
    )
    |> squeezeTLDeclarations

          (* Reorder functions because of forward declarations *)


let rec private findRemove callback = function
    | (name, [], content) :: l ->
        //printfn "=> %s" name
        callback name content
        l
    | [] -> failwith "Cannot reorder functions (probably because of a recursion)."
    | x :: l -> x :: findRemove callback l

// slow, but who cares?
let private graphReorder deps =
    let mutable list = []
    let mutable lastName = ""

    let rec loop deps =
        let deps = findRemove (fun (s: Ident) x -> lastName <- s.Name; list <- x :: list) deps
        let deps = deps |> List.map (fun (n, d, c) -> n, List.filter ((<>) lastName) d, c)
        if deps <> [] then loop deps

    loop deps
    list |> List.rev


// get the list of external values the block depends on
let private computeDependencies block =
    let d = HashSet()
    let collect mEnv = function
        | Var id as e ->
            if not (mEnv.vars.ContainsKey(id.Name)) then d.Add id.Name |> ignore
            e
        | e -> e
    mapStmt (mapEnv collect id) block |> ignore
    d |> Seq.toList

// This function assumes that functions are NOT overloaded
let private computeAllDependencies code =
    let fct = code |> List.choose (function
        | Function(fct, block) as f -> Some (fct.fName, block, f)
        | _ -> None)
    let deps = fct |> List.map (fun (name, block, f) ->
        let dep = computeDependencies block
                  |> List.filter (fun name -> fct |> List.exists (fun (x,_,_) -> name = x.Name))
        name, dep, f)
    deps

// reorder functions if there were forward declarations
let reorder code =
    if options.reorderFunctions then
        if options.verbose then
            printfn "Reordering functions because of forward declarations."
        let order = code |> computeAllDependencies |> graphReorder
        let rest = code |> List.filter (function Function _ -> false | _ -> true)
        rest @ order
    else
        code
