module Rewriter

open System
open System.Collections.Generic
open Ast
open Options.Globals

                                (* ** Rewrite tricks ** *)

let private isFieldSwizzle s =
    Seq.forall (fun c -> Seq.contains c "rgba") s ||
    Seq.forall (fun c -> Seq.contains c "xyzw") s ||
    Seq.forall (fun c -> Seq.contains c "stpq") s

let private renameSwizzle field =
    let transform = function
        | 'r' | 'x' | 's' -> options.canonicalFieldNames.[0]
        | 'g' | 'y' | 't' -> options.canonicalFieldNames.[1]
        | 'b' | 'z' | 'p' -> options.canonicalFieldNames.[2]
        | 'a' | 'w' | 'q' -> options.canonicalFieldNames.[3]
        | c -> failwithf "Internal error: transform('%c')" c
    field |> String.map transform

let renameField field =
    if isFieldSwizzle field then renameSwizzle field
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
    let mutable wasNewline = false
    for c in str do
        if c = '\n' then
            if not wasNewline then
                write '\n'
            space <- false
            wasNewline <- true

        elif Char.IsWhiteSpace(c) then
            space <- true
            wasNewline <- false
        else
            wasNewline <- false
            if space && isId c && isId last then
                write ' '
            write c
            space <- false

    result.ToString()


let private declsNotToInline (d: DeclElt list) = d |> List.filter (fun x -> not x.name.ToBeInlined)

let private bool = function
    | true -> Var (Ident "true") // Int (1, "")
    | false -> Var (Ident "false") // Int (0, "")

let private inlineFn (declArgs:Decl list) passedArgs bodyExpr =
    let mutable argMap = Map.empty
    for declArg, passedArg in List.zip declArgs passedArgs do
        let declElt = List.exactlyOne (snd declArg)
        argMap <- argMap.Add(declElt.name.Name, passedArg)
    let mapInline _ = function
        | Var iv as ie ->
            match argMap.TryFind iv.Name with
            | Some inlinedExpr -> inlinedExpr
            | _ -> ie
        | ie -> ie
    mapExpr (mapEnv mapInline id) bodyExpr

/// Expression that doesn't need parentheses around it.
let (|NoParen|_|) = function
    | Int _ | Float _ | Dot _ | Var _ | FunCall (Var _, _) | Subscript _ as x -> Some x
    | _ -> None

/// Expression that statically evaluates to boolean value.
let (|True|False|NotABool|) = function
    | Int (i, _) when i <> 0 -> True
    | Int (i, _) when i = 0 -> False
    | Float (f, _) when f <> 0.M -> True
    | Float (f, _) when f = 0.M -> False
    | Var var when var.Name = "true" -> True
    | Var var when var.Name = "false" -> False
    | _ -> NotABool

let (|Number|_|) = function
    | Int (i, _) -> Some (decimal i)
    | Float (f, _) -> Some f
    | _ -> None

let private simplifyOperator env = function
    | FunCall(Op "-", [Int (i1, su)]) -> Int (-i1, su)
    | FunCall(Op "-", [FunCall(Op "-", [e])]) -> e
    | FunCall(Op "+", [e]) -> e

    | FunCall(Op ",", [e1; FunCall(Op ",", [e2; e3])]) ->
        FunCall(Op ",", [env.fExpr env (FunCall(Op ",", [e1; e2])); e3])

    | FunCall(Op "-", [x; Float (f, s)]) when f < 0.M ->
        FunCall(Op "+", [x; Float (-f, s)]) |> env.fExpr env
    | FunCall(Op "-", [x; Int (i, s)]) when i < 0 ->
        FunCall(Op "+", [x; Int (-i, s)]) |> env.fExpr env

    // Boolean simplifications (let's ignore the suffix)
    | FunCall(Op "<", [Number n1; Number n2]) -> bool(n1 < n2)
    | FunCall(Op ">", [Number n1; Number n2]) -> bool(n1 > n2)
    | FunCall(Op "<=", [Number n1; Number n2]) -> bool(n1 <= n2)
    | FunCall(Op ">=", [Number n1; Number n2]) -> bool(n1 <= n2)
    | FunCall(Op "==", [Number n1; Number n2]) -> bool(n1 = n2)
    | FunCall(Op "!=", [Number n1; Number n2]) -> bool(n1 <> n2)

    // Conditionals
    | FunCall(Op "?:", [True; x; _]) -> x
    | FunCall(Op "?:", [False; _; x]) -> x
    | FunCall(Op "&&", [True; x]) -> x
    | FunCall(Op "&&", [False; _]) -> bool false
    | FunCall(Op "&&", [x; True]) -> x
    | FunCall(Op "||", [True; _]) -> bool true
    | FunCall(Op "||", [False; x]) -> x
    | FunCall(Op "||", [x; False]) -> x

    // Stupid simplifications (they can be useful to simplify rewritten code)
    | FunCall(Op "/", [e; Number 1M]) -> e
    | FunCall(Op "*", [e; Number 1M]) -> e
    | FunCall(Op "*", [Number 1M; e]) -> e
    // | FunCall(Op "*", [_; Number 0M as zero]) -> zero // unsafe
    // | FunCall(Op "*", [Number 0M as zero; _]) -> zero // unsafe
    | FunCall(Op "+", [e; Number 0M]) -> e
    | FunCall(Op "+", [Number 0M; e]) -> e
    | FunCall(Op "-", [e; Number 0M]) -> e
    | FunCall(Op "-", [Number 0M; e]) -> FunCall(Op "-", [e])

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

    // Swap operands to get rid of parentheses
    // x*(y*z) -> y*z*x
    | FunCall(Op "*", [NoParen x; FunCall(Op "*", [y; z])]) ->
        FunCall(Op "*", [FunCall(Op "*", [y; z]); x]) |> env.fExpr env
    // x+(y+z) -> y+z+x
    // x+(y-z) -> y-z+a
    | FunCall(Op "+", [NoParen x; FunCall(Op ("+"|"-") as op, [y; z])]) ->
        FunCall(Op "+", [FunCall(op, [y; z]); x]) |> env.fExpr env
    // x-(y+z) -> x-y-z
    | FunCall(Op "-", [x; FunCall(Op "+", [y; z])]) ->
        FunCall(Op "-", [FunCall(Op "-", [x; y]); z]) |> env.fExpr env
    // x-(y-z) -> x-y+z
    | FunCall(Op "-", [x; FunCall(Op "-", [y; z])]) ->
        FunCall(Op "+", [FunCall(Op "-", [x; y]); z]) |> env.fExpr env
    | e -> e


// Simplify calls to the vec constructor.
let private simplifyVec constr args =
    // Combine swizzles, e.g.
    //    vec4(v1.x, v1.z, v2.r, v2.t)  =>  vec4(v1.xz, v2.xy)
    let rec combineSwizzles = function
        | [] -> []
        | Dot (Var v1, field1) :: Dot (Var v2, field2) :: args
            when isFieldSwizzle field1 && isFieldSwizzle field2 && v1.Name = v2.Name ->
                combineSwizzles (Dot (Var v1, field1 + field2) :: args)
        | e::l -> e :: combineSwizzles l

    // vec2(1.0, 2.0)  =>  vec2(1, 2)
    // According to the spec, this is safe:
    // "If the basic type (bool, int, float, or double) of a parameter to a constructor does not match the
    // basic type of the object being constructed, the scalar construction rules (above) are used to convert
    // the parameters."
    let useInts = function
        | Float (f, _) when Decimal.Round(f) = f -> Int (int f, "")
        | e -> e

    let args = combineSwizzles args |> List.map useInts
    match args with
    | [Dot (_, field) as arg] when field.Length > 1 && isFieldSwizzle field ->
        // vec3(v.xxy)  =>  v.xxy
        // However, vec3(v.x) should be preserved.
        arg
    | _ -> FunCall (Var constr, args)

let private simplifyExpr (didInline: bool ref) env = function
    | FunCall(Var v, passedArgs) as e when v.ToBeInlined ->
        match env.fns.TryFind v.Name with
        | None -> e
        | Some ({args = declArgs}, body) ->
            match body with
            | Jump (JumpKeyword.Return, Some bodyExpr)
            | Block [Jump (JumpKeyword.Return, Some bodyExpr)] ->
                didInline.Value <- true
                inlineFn declArgs passedArgs bodyExpr
            // Don't yell if we've done some inlining this pass -- maybe it
            // turned the function into a one-liner, so allow trying again on
            // the next pass. (If it didn't, we'll yell next pass.)
            | _ when didInline.Value -> e
            | _ -> failwithf "Cannot inline %s since it consists of more than a single return" v.Name

    | FunCall(Op _, _) as op -> simplifyOperator env op
    | FunCall(Var constr, args) when constr.Name = "vec2" || constr.Name = "vec3" || constr.Name = "vec4" ->
        simplifyVec constr args

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
        | Some (_, {name = id; init = Some init}) when id.ToBeInlined ->
            didInline.Value <- true
            init |> mapExpr env
        | _ -> e

    // pi is acos(-1), pi/2 is acos(0)
    | Float(f, _) when Decimal.Round(f, 8) = 3.14159265M -> FunCall(Var (Ident "acos"), [Float (-1.M, "")])
    | Float(f, _) when Decimal.Round(f, 8) = 6.28318531M -> FunCall(Op "*", [Float (2.M, ""); FunCall(Var (Ident "acos"), [Float (-1.M, "")])])
    | Float(f, _) when Decimal.Round(f, 8) = 1.57079633M -> FunCall(Var (Ident "acos"), [Float (0.M, "")])

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
    makeType (rwTypeSpec ty.name) (List.map stripSpaces ty.typeQ) ty.arraySizes

let rwFType fct =
    // The default for function parameters is "in", we don't need it.
    let rwFTypeType ty = {ty with typeQ = List.except ["in"] ty.typeQ}
    let rwFDecl (ty, elts) = (rwFTypeType ty, elts)
    {fct with args = List.map rwFDecl fct.args}

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
    | If(True, e1, _) -> e1
    | If(False, _, Some e2) -> e2
    | If(False, _, None) -> Block []
    | If(c, b, Some (Block [])) -> If(c, b, None)
    | Verbatim s -> Verbatim (stripSpaces s)
    | e -> e

let rec iterateSimplifyAndInline li =
    if not options.noInlining then
        let mapExpr _ e = e
        let mapStmt = function
            | Block b as e -> Analyzer.findInlinable b; e
            | e -> e
        mapTopLevel (mapEnv mapExpr mapStmt) li |> ignore
    let didInline = ref false
    let simplified = mapTopLevel (mapEnv (simplifyExpr didInline) simplifyStmt) li
    if didInline.Value then iterateSimplifyAndInline simplified else simplified

let simplify li =
    li
    // markLValues doesn't change the AST so we could do it unconditionally,
    // but we only need the information for aggroInlining so don't bother if
    // it's off.
    |> Analyzer.markLValues
    |> if options.aggroInlining then Analyzer.inlineAllConsts else id
    |> iterateSimplifyAndInline
    |> List.choose (function
        | TLDecl (ty, li) -> TLDecl (rwType ty, declsNotToInline li) |> Some
        | TLVerbatim s -> TLVerbatim (stripSpaces s) |> Some
        | Function (fct, _) when fct.fName.ToBeInlined -> None
        | Function (fct, body) -> Function (rwFType fct, body) |> Some
        | e -> e |> Some
    )
    |> squeezeTLDeclarations

          (* Reorder functions because of forward declarations *)


type CallGraphNode = {
    func: TopLevel
    funcType: FunctionType
    name: string
    callees: string list
}

let rec private findRemove callback = function
    | node :: l when node.callees.IsEmpty ->
        //printfn "=> %s" name
        callback node
        l
    | [] -> failwith "Cannot reorder functions (probably because of a recursion)."
    | x :: l -> x :: findRemove callback l

// slow, but who cares?
let private graphReorder nodes =
    let mutable list = []
    let mutable lastName = ""

    let rec loop nodes =
        let nodes = findRemove (fun node -> lastName <- node.name; list <- node.func :: list) nodes
        let nodes = nodes |> List.map (fun n -> { n with callees = List.except [lastName] n.callees })
        if nodes <> [] then loop nodes

    if nodes <> [] then loop nodes
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
    let functions = code |> List.choose (function
        | Function(funcType, block) as f -> Some (funcType, funcType.fName.Name, block, f)
        | _ -> None)
    let nodes = functions |> List.map (fun (funcType, name, block, f) ->
        let callees = computeDependencies block
                      |> List.filter (fun name2 -> functions |> List.exists (fun (_,n,_,_) -> name2 = n))
        { CallGraphNode.func = f; funcType = funcType; name = name; callees = callees })
    nodes


let removeUnused code =
    let nodes = computeAllDependencies code
    let isUnused node =
        let canBeRenamed = not (options.noRenamingList |> List.contains node.name) // noRenamingList includes "main"
        let isCalled = (nodes |> List.exists (fun n -> n.callees |> List.contains node.name))
        let isExternal = options.hlsl && node.funcType.semantics <> []
        canBeRenamed && not isCalled && not isExternal
    let unused = set [for node in nodes do if isUnused node then yield node.func]
    code |> List.filter (function
        | Function _ as t -> not (unused |> Set.contains t)
        | _ -> true)

// reorder functions if there were forward declarations
let reorder code =
    if options.verbose then
        printfn "Reordering functions because of forward declarations."
    let order = code |> computeAllDependencies |> graphReorder
    let rest = code |> List.filter (function Function _ -> false | _ -> true)
    rest @ order
