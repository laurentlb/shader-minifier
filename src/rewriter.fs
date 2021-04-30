module Rewriter

open System
open System.Collections.Generic
open Ast

                                (* ** Rewrite tricks ** *)


let renameField field =
    let transform = function
        | 'r' | 'x' | 's' -> fieldNames.[0]
        | 'g' | 'y' | 't' -> fieldNames.[1]
        | 'b' | 'z' | 'p' -> fieldNames.[2]
        | 'a' | 'w' | 'q' -> fieldNames.[3]
        | c -> failwithf "Internal error: transform('%c')" c
    if Seq.forall (fun c -> Seq.exists ((=) c) "rgba") field ||
        Seq.forall (fun c -> Seq.exists ((=) c) "xyzw") field ||
        Seq.forall (fun c -> Seq.exists ((=) c) "stpq") field
    then
        field |> String.map transform
    else field

// Remove useless spaces in macros
let stripSpaces str =
    let result = Text.StringBuilder()

    let last = ref '\n'
    let write c =
        last := c
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
                if !last <> '\n' then write '\n'

            if space && isId c && isId (!last) then
                write ' '
            write c
            space <- false

    if macro then result.Append("\n") |> ignore
    result.ToString()

let hasInlinePrefix (s:string) = s.StartsWith("i_")
let declsNotToInline d = d |> List.filter (fun x -> not (hasInlinePrefix x.name))

let bool = function
    | true -> Var "true" // Int (1, "")
    | false -> Var "false" // Int (0, "")

let rec expr env = function
    | FunCall(Var "-", [Int (i1, su)]) -> Int (-i1, su)
    | FunCall(Var "-", [FunCall(Var "-", [e])]) -> e
    | FunCall(Var "+", [e]) -> e

    | FunCall(Var ",", [e1; FunCall(Var ",", [e2; e3])]) ->
        FunCall(Var ",", [expr env (FunCall(Var ",", [e1; e2])); e3])

    | FunCall(Var "-", [x; Float (f, s)]) when f < 0. ->
          FunCall(Var "+", [x; Float (-f, s)]) |> expr env
    | FunCall(Var "-", [x; Int (i, s)]) when i < 0 ->
          FunCall(Var "+", [x; Int (-i, s)]) |> expr env

    // Boolean simplifications (let's ignore the suffix)
    | FunCall(Var "<",  [Int (i1, _); Int (i2, _)]) -> bool(i1 < i2)
    | FunCall(Var ">",  [Int (i1, _); Int (i2, _)]) -> bool(i1 > i2)
    | FunCall(Var "<=", [Int (i1, _); Int (i2, _)]) -> bool(i1 <= i2)
    | FunCall(Var ">=", [Int (i1, _); Int (i2, _)]) -> bool(i1 <= i2)
    | FunCall(Var "==", [Int (i1, _); Int (i2, _)]) -> bool(i1 = i2)
    | FunCall(Var "!=", [Int (i1, _); Int (i2, _)]) -> bool(i1 <> i2)

    | FunCall(Var "<", [Float (i1,_); Float (i2,_)]) -> bool(i1 < i2)
    | FunCall(Var ">", [Float (i1,_); Float (i2,_)]) -> bool(i1 > i2)
    | FunCall(Var "<=", [Float (i1,_); Float (i2,_)]) -> bool(i1 <= i2)
    | FunCall(Var ">=", [Float (i1,_); Float (i2,_)]) -> bool(i1 <= i2)
    | FunCall(Var "==", [Float (i1,_); Float (i2,_)]) -> bool(i1 = i2)
    | FunCall(Var "!=", [Float (i1,_); Float (i2,_)]) -> bool(i1 <> i2)

    // Stupid simplifications (they can be useful to simplify rewritten code)
    | FunCall(Var "/", [e; Float (1.,_)]) -> e
    | FunCall(Var "*", [e; Float (1.,_)]) -> e
    | FunCall(Var "*", [Float (1.,_); e]) -> e
    | FunCall(Var "*", [_; Float (0.,_) as e]) -> e
    | FunCall(Var "*", [Float (0.,_) as e; _]) -> e
    | FunCall(Var "+", [e; Float (0.,_)]) -> e
    | FunCall(Var "+", [Float (0.,_); e]) -> e
    | FunCall(Var "-", [e; Float (0.,_)]) -> e
    | FunCall(Var "-", [Float (0.,_); e]) -> FunCall(Var "-", [e])

    // No simplification when numbers have different suffixes
    | FunCall(_, [Int (_, su1); Int (_, su2)]) as e when su1 <> su2 -> e
    | FunCall(_, [Float (_, su1); Float (_, su2)]) as e when su1 <> su2 -> e

    | FunCall(Var "-", [Int (i1, su); Int (i2, _)]) -> Int (i1 - i2, su)
    | FunCall(Var "+", [Int (i1, su); Int (i2, _)]) -> Int (i1 + i2, su)
    | FunCall(Var "*", [Int (i1, su); Int (i2, _)]) -> Int (i1 * i2, su)
    | FunCall(Var "/", [Int (i1, su); Int (i2, _)]) -> Int (i1 / i2, su)
    | FunCall(Var "mod", [Int (i1, su); Int (i2, _)]) -> Int (i1 % i2, su)

    | FunCall(Var "-", [Float (f1,su)]) -> Float (-f1, su)
    | FunCall(Var "-", [Float (i1,su); Float (i2,_)]) -> Float (i1 - i2, su)
    | FunCall(Var "+", [Float (i1,su); Float (i2,_)]) -> Float (i1 + i2, su)
    | FunCall(Var "*", [Float (i1,su); Float (i2,_)]) -> Float (i1 * i2, su)
    | FunCall(Var "/", [Float (i1,su); Float (i2,_)]) as e ->
        let div = Float (i1 / i2, su)
        if (Printer.exprToS e).Length <= (Printer.exprToS div).Length then e
        else div

    // iq's smoothstep trick: http://www.pouet.net/topic.php?which=6751&page=1#c295695
    | FunCall(Var "smoothstep", [Float (0.,_); Float (1.,_); _]) as e -> e
    | FunCall(Var "smoothstep", [a; b; x]) when Ast.smoothstepTrick ->
        let sub1 = FunCall(Var "-", [x; a])
        let sub2 = FunCall(Var "-", [b; a])
        let div  = FunCall(Var "/", [sub1; sub2]) |> mapExpr env
        FunCall(Var "smoothstep",  [Float (0.,""); Float (1.,""); div])

    | Dot(e, field) when fieldNames <> "" -> Dot(e, renameField field)

    | Var s as e when hasInlinePrefix s ->
      match Map.tryFind s env.vars with
        | Some (_, _, Some init) -> init
        | _ -> e

    | e -> e

// Squeeze declarations: "float a=2.; float b;" => "float a=2.,b;"
let rec squeezeDeclarations = function
    |[]-> []
    |Decl(ty1, li1) :: Decl(ty2, li2) :: l when ty1 = ty2 ->
      squeezeDeclarations (Decl(ty1, li1 @ li2) :: l)
    |e::l -> e :: squeezeDeclarations l

// Squeeze top-level declarations, e.g. uniforms
let rec squeezeTLDeclarations = function
    |[]-> []
    |TLDecl(ty1, li1) :: TLDecl(ty2, li2) :: l when ty1 = ty2 ->
      squeezeTLDeclarations (TLDecl(ty1, li1 @ li2) :: l)
    |e::l -> e :: squeezeTLDeclarations l

let rwTypeSpec = function
    | TypeName n -> TypeName (stripSpaces n)
    | x -> x // structs

let rwType (ty: Type) =
    makeType (rwTypeSpec ty.name) (Option.map stripSpaces ty.typeQ)

let instr = function
    | Block [] as e -> e
    | Block b ->
        // Remove dead code after return/break/...
        let endOfCode = Seq.tryFindIndex (function Keyword(_, _) -> true | _ -> false) b
        let b = match endOfCode with None -> b | Some x -> b |> Seq.truncate (x+1) |> Seq.toList

        // Remove inner empty blocks
        let b = b |> List.filter (function Block [] -> false | _ -> true)

        // Try to remove blocks by using the comma operator
        let returnExp = b |> Seq.tryPick (function Keyword("return", e) -> e | _ -> None)
        let canOptimize = b |> List.forall (function
            | Expr _ -> true
            | Keyword("return", Some _) -> true
            | _ -> false)

        if not Ast.noSequence && canOptimize then
            let li = List.choose (function Expr e -> Some e | _ -> None) b
            match returnExp with
            | None ->
                if li = [] then Block []
                else Expr (List.reduce (fun acc x -> FunCall(Var ",", [acc;x])) li)
            | Some e ->
               let expr = List.reduce (fun acc x -> FunCall(Var ",", [acc;x])) (li@[e])
               Keyword("return", Some expr)
        else
            Block (squeezeDeclarations b)
    | Decl (ty, li) -> Decl (rwType ty, declsNotToInline li)
    | ForD((ty, d), cond, inc, body) -> ForD((rwType ty, declsNotToInline d), cond, inc, body)
    // FIXME: properly handle booleans
    | If(Var "true", e1, _) -> e1
    | If(Var "false", _, Some e2) -> e2
    | If(Var "false", _, None) -> Block []
    | If(c, b, Some (Block [])) -> If(c, b, None)
    | Verbatim s -> Verbatim (stripSpaces s)
    | e -> e

let reorderTopLevel t =
    if reorderDeclarations then
        let externals, functions = List.partition (function TLDecl _ -> true | _ -> false) t
        List.sort externals @ functions
    else
        t

let apply li =
    li
    |> reorderTopLevel
    |> mapTopLevel (mapEnv expr instr)
    |> List.map (function
        | TLDecl (ty, li) -> TLDecl (rwType ty, declsNotToInline li)
        | TLVerbatim s -> TLVerbatim (stripSpaces s)
        | e -> e
    )
    |> squeezeTLDeclarations

          (* Reorder functions because of forward declarations *)


let rec findRemove callback = function
    | (name, [], content) :: l ->
        //printfn "=> %s" name
        callback name content
        l
    | [] -> failwith "Cannot reorder functions (probably because of a recursion)."
    | x :: l -> x :: findRemove callback l

// slow, but who cares?
let graphReorder deps =
    let list = ref []
    let lastName = ref ""

    let rec loop deps =
        let deps = findRemove (fun s x -> lastName := s; list := x :: !list) deps
        let deps = deps |> List.map (fun (n, d, c) -> n, List.filter ((<>) !lastName) d, c)
        if deps <> [] then loop deps

    loop deps
    !list |> List.rev


// get the list of external values the block depends on
let computeDependencies block =
    let d = HashSet()
    let collect mEnv = function
        | Var id as e ->
            if not (mEnv.vars.ContainsKey(id)) then d.Add id |> ignore
            e
        | e -> e
    mapInstr (mapEnv collect id) block |> ignore
    d |> Seq.toList

// This function assumes that functions are NOT overloaded
let computeAllDependencies code =
    let fct = code |> List.choose (function
        | Function(fct, block) as f -> Some (fct.fName, block, f)
        | _ -> None)
    let deps = fct |> List.map (fun (name, block, f) ->
        let dep = computeDependencies block
               |> List.filter (fun name -> List.exists (fun (x,_,_) -> name = x) fct)
        name, dep, f)
    deps

// reorder functions if there were forward declarations
let reorder code =
    if Ast.reorderFunctions then
        if Ast.verbose then
            printfn "Reordering functions because of forward declarations."
        let order = code |> computeAllDependencies |> graphReorder
        let rest = code |> List.filter (function Function _ -> false | _ -> true)
        rest @ order
    else
        code
