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


let private declsNotToInline (d: Ast.DeclElt list) = d |> List.filter (fun x -> not x.name.MustBeInlined)

let private bool = function
    | true -> Var (Ident "true") // Int (1, "")
    | false -> Var (Ident "false") // Int (0, "")

let rec private simplifyExpr env = function
    | FunCall(Op "-", [Int (i1, su)]) -> Int (-i1, su)
    | FunCall(Op "-", [FunCall(Op "-", [e])]) -> e
    | FunCall(Op "+", [e]) -> e

    | FunCall(Op ",", [e1; FunCall(Op ",", [e2; e3])]) ->
        FunCall(Op ",", [simplifyExpr env (FunCall(Op ",", [e1; e2])); e3])

    | FunCall(Op "-", [x; Float (f, s)]) when f < 0. ->
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
    | FunCall(Op "/", [e; Float (1.,_)]) -> e
    | FunCall(Op "*", [e; Float (1.,_)]) -> e
    | FunCall(Op "*", [Float (1.,_); e]) -> e
    | FunCall(Op "*", [_; Float (0.,_) as e]) -> e
    | FunCall(Op "*", [Float (0.,_) as e; _]) -> e
    | FunCall(Op "+", [e; Float (0.,_)]) -> e
    | FunCall(Op "+", [Float (0.,_); e]) -> e
    | FunCall(Op "-", [e; Float (0.,_)]) -> e
    | FunCall(Op "-", [Float (0.,_); e]) -> FunCall(Op "-", [e])

    // No simplification when numbers have different suffixes
    | FunCall(_, [Int (_, su1); Int (_, su2)]) as e when su1 <> su2 -> e
    | FunCall(_, [Float (_, su1); Float (_, su2)]) as e when su1 <> su2 -> e

    | FunCall(Op "-", [Int (i1, su); Int (i2, _)]) -> Int (i1 - i2, su)
    | FunCall(Op "+", [Int (i1, su); Int (i2, _)]) -> Int (i1 + i2, su)
    | FunCall(Op "*", [Int (i1, su); Int (i2, _)]) -> Int (i1 * i2, su)
    | FunCall(Op "/", [Int (i1, su); Int (i2, _)]) -> Int (i1 / i2, su)
    | FunCall(Op "%", [Int (i1, su); Int (i2, _)]) -> Int (i1 % i2, su)

    | FunCall(Op "-", [Float (0.0,su)]) -> Float (0.0, su)
    | FunCall(Op "-", [Float (f1,su)]) -> Float (-f1, su)
    | FunCall(Op "-", [Float (i1,su); Float (i2,_)]) -> Float (i1 - i2, su)
    | FunCall(Op "+", [Float (i1,su); Float (i2,_)]) -> Float (i1 + i2, su)
    | FunCall(Op "*", [Float (i1,su); Float (i2,_)]) -> Float (i1 * i2, su)
    | FunCall(Op "/", [Float (i1,su); Float (i2,_)]) as e ->
        let div = Float (i1 / i2, su)
        if (Printer.exprToS e).Length <= (Printer.exprToS div).Length then e
        else div

    // iq's smoothstep trick: http://www.pouet.net/topic.php?which=6751&page=1#c295695
    | FunCall(Var var, [Float (0.,_); Float (1.,_); _]) as e when var.Name = "smoothstep" -> e
    | FunCall(Var var, [a; b; x]) when var.Name = "smoothstep" && options.smoothstepTrick ->
        let sub1 = FunCall(Op "-", [x; a])
        let sub2 = FunCall(Op "-", [b; a])
        let div  = FunCall(Op "/", [sub1; sub2]) |> mapExpr env
        FunCall(Var (Ident "smoothstep"),  [Float (0.,""); Float (1.,""); div])

    | Dot(e, field) when options.canonicalFieldNames <> "" -> Dot(e, renameField field)

    | Var s as e when s.MustBeInlined ->
      match env.vars.TryFind s.Name with
        | Some (_, _, Some init) -> init |> mapExpr env
        | _ -> e

    // pi is acos(-1), pi/2 is acos(0)
    | Float(f, _) when f = 3.141592653589793 -> FunCall(Var (Ident "acos"), [Float (-1., "")])
    | Float(f, _) when f = 6.283185307179586 -> FunCall(Op "*", [Float (2., ""); FunCall(Var (Ident "acos"), [Float (-1., "")])])
    | Float(f, _) when f = 1.5707963267948966 -> FunCall(Var (Ident "acos"), [Float (0., "")])

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
    makeType (rwTypeSpec ty.name) (Option.map stripSpaces ty.typeQ)

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

let simplify li =
    li
    |> reorderTopLevel
    |> mapTopLevel (mapEnv simplifyExpr simplifyStmt)
    |> List.map (function
        | TLDecl (ty, li) -> TLDecl (rwType ty, declsNotToInline li)
        | TLVerbatim s -> TLVerbatim (stripSpaces s)
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
