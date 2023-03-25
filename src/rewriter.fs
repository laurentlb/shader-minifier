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

let private swizzleIndex = function
    | 'r' | 'x' | 's' -> 0
    | 'g' | 'y' | 't' -> 1
    | 'b' | 'z' | 'p' -> 2
    | 'a' | 'w' | 'q' -> 3
    | c -> failwithf "not a swizzle (%c) " c

let private renameSwizzle field =
    field |> String.map (fun c -> options.canonicalFieldNames.[swizzleIndex c])

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

let augmentableOperators = set ["+"; "-"; "*"; "/"; "%"; "<<"; ">>"; "&"; "^"; "|"]


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
    | FunCall(Op ">=", [Number n1; Number n2]) -> bool(n1 >= n2)
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

     // -(x-y) -> -x+y
    | FunCall(Op "-", [FunCall(Op "-", [x; y])]) ->
        let minusX = FunCall(Op "-", [x]) |> env.fExpr env
        FunCall(Op "+", [minusX; y]) |> env.fExpr env
    // -(x+y) -> -x-y
    | FunCall(Op "-", [FunCall(Op "+", [x; y])]) ->
        let minusX = FunCall(Op "-", [x]) |> env.fExpr env
        FunCall(Op "-", [minusX; y]) |> env.fExpr env
    // -(x*y) -> -x*y
    | FunCall(Op "-", [FunCall(Op "*", [x; y])]) ->
        let minusX = FunCall(Op "-", [x]) |> env.fExpr env
        FunCall(Op "*", [minusX; y]) |> env.fExpr env
    // -(x/y) -> -x/y
    | FunCall(Op "-", [FunCall(Op "/", [x; y])]) ->
        let minusX = FunCall(Op "-", [x]) |> env.fExpr env
        FunCall(Op "/", [minusX; y]) |> env.fExpr env

    // Match:  x = x + ...
    | FunCall(Op "=", [Var x; FunCall(Op op, [Var y; e])])
            when x.Name = y.Name && augmentableOperators.Contains op ->
        FunCall(Op (op + "="), [Var x; e])
    // Unsafe when x contains NaN or Inf values.
    //| FunCall(Op "=", [Var x; FunCall(Var fctName, [Int (0, _)])])
    //    when List.contains fctName.Name ["vec2"; "vec3"; "vec4"; "ivec2"; "ivec3"; "ivec4"] ->
    //    FunCall(Op "-=", [Var x; Var x]) // x=vec3(0);  ->  x-=x;
    | e -> e


// Simplify calls to the vec constructor.
let private simplifyVec (constr: Ident) args =
    let vecSize = try int(constr.Name.[constr.Name.Length - 1]) - int '0' with _ -> 0

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
        | Float (f, _) as e when Decimal.Round(f) = f ->
            try
                let candidate = Int (int f, "")
                if (Printer.exprToS candidate).Length <= (Printer.exprToS e).Length then
                    candidate
                else
                    e
            with
                // the conversion to int might fail (especially on 32-bit)
                | :? System.OverflowException -> e

        | e -> e

    // vec3(1,1,1)  =>  vec3(1)
    // For safety, do not merge if there are function calls, e.g. vec2(rand(), rand()).
    // Iterate over the args as long as the arguments are equal.
    let rec mergeAllEquals allArgs = function
        | FunCall _ :: _ -> allArgs
        | e1 :: e2 :: rest when Printer.exprToS e1 = Printer.exprToS e2 ->
            mergeAllEquals allArgs (e2 :: rest)
        | [e] -> [e]
        | _ -> allArgs

    // vec3(a.x, b.xy) => vec3(a.x, b)
    let rec dropLastSwizzle = function
        | [Dot (expr, field) as last] ->
            match [for c in field -> swizzleIndex c] with
            | [0] | [0; 1] | [0; 1; 2] | [0; 1; 2; 3] -> [expr]
            | _ -> [last]
        | e1 :: rest -> e1 :: dropLastSwizzle rest
        | x -> x

    let args = combineSwizzles args |> List.map useInts
    let args = if args.Length = vecSize then mergeAllEquals args args else args
    let args = dropLastSwizzle args
    FunCall (Var constr, args)

let private simplifyExpr (didInline: bool ref) env = function
    | FunCall(Var v, passedArgs) as e when v.ToBeInlined ->
        match env.fns.TryFind v.Name with
        | None -> e
        | Some ({args = declArgs}, body) ->
            if List.length declArgs <> List.length passedArgs then
                failwithf "Cannot inline %s since it doesn't have the right number of arguments" v.Name
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
        FunCall(Var (Ident "smoothstep"), [Float (0.M,""); Float (1.M,""); div])

    | Dot(e, field) when options.canonicalFieldNames <> "" -> Dot(e, renameField field)

    | Var s as e ->
        match env.vars.TryFind s.Name with
        | Some (_, {name = id; init = Some init}, _) when id.ToBeInlined ->
            didInline.Value <- true
            init |> mapExpr env
        | _ -> e

    // pi is acos(-1), pi/2 is acos(0)
    | Float(f, _) when Decimal.Round(f, 8) = 3.14159265M -> FunCall(Var (Ident "acos"), [Float (-1.M, "")])
    | Float(f, _) when Decimal.Round(f, 8) = 6.28318531M -> FunCall(Op "*", [Float (2.M, ""); FunCall(Var (Ident "acos"), [Float (-1.M, "")])])
    | Float(f, _) when Decimal.Round(f, 8) = 1.57079633M -> FunCall(Var (Ident "acos"), [Float (0.M, "")])

    | e -> e

// Group declarations within a block. For example, all the float variables will
// be declared at the same time, the first time a float variable is initialized.
// Const variables are ignored, because they must be initialized immediately.
let groupDeclarations stmts =
    let declarations = new Dictionary<Type, DeclElt list>()
    let types = new Dictionary<string, Type * DeclElt>()
    for stmt in stmts do
        match stmt with
        | Decl (ty, li) when not (List.contains "const" ty.typeQ) ->
            for variable in li do
                types.[variable.name.Name] <- (ty, variable)
            if not (declarations.ContainsKey ty) then
                declarations[ty] <- li
            else
                // Remove the init of the new items; we'll use an assignment instead.
                let li = li |> List.map (function decl -> {decl with init = None})
                declarations.[ty] <- declarations.[ty] @ li
        | _ -> ()

    let replacements = function
        | Decl (ty, _) as decl when List.contains "const" ty.typeQ ->
            [decl]
        | Decl (ty, _) when declarations.ContainsKey ty ->
            // Insert all declarations.
            let decl = declarations[ty]
            declarations.Remove ty |> ignore<bool>
            [Decl (ty, decl)]
        | Decl (_, li) ->
            // Replace the declarations (they were already inserted) with assignments.
            li |> List.choose (function
                | {name = name; init = Some init} ->
                    Some (Expr (FunCall (Op "=", [Var name; init])))
                | _ -> None
            )
        | e -> [e]

    List.collect replacements stmts

// Squeeze declarations: "float a=2.; float b;" => "float a=2.,b;"
let rec private squeezeConsecutiveDeclarations = function
    | []-> []
    | Decl(ty1, li1) :: Decl(ty2, li2) :: l when ty1 = ty2 ->
        squeezeConsecutiveDeclarations (Decl(ty1, li1 @ li2) :: l)
    | e::l -> e :: squeezeConsecutiveDeclarations l

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

let squeezeBlockWithComma = function
    | Block b as stmt when not options.noSequence ->
        let canOptimize = b |> List.forall (function
            | Expr _ -> true
            | Jump(JumpKeyword.Return, Some _) -> true
            | _ -> false)
        // Try to remove blocks by using the comma operator
        if canOptimize then
            let li = List.choose (function Expr e -> Some e | _ -> None) b
            let returnExp = b |> Seq.tryPick (function Jump(JumpKeyword.Return, e) -> e | _ -> None)
            match returnExp with
            | None ->
                if li.IsEmpty then Block []
                else Expr (List.reduce (fun acc x -> FunCall(Op ",", [acc;x])) li)
            | Some e ->
               let expr = List.reduce (fun acc x -> FunCall(Op ",", [acc;x])) (li@[e])
               Jump(JumpKeyword.Return, Some expr)
        else stmt
    | stmt -> stmt

let hasNoDecl = List.forall (function Decl _ -> false | _ -> true)

let rec hasNoContinue stmts = stmts |> List.forall (function
    | Jump (JumpKeyword.Continue, _) -> false
    | If (_cond, bodyT, bodyF) -> hasNoContinue [bodyT] && hasNoContinue (Option.toList bodyF)
    | Switch (_e, cases) -> cases |> List.forall (fun (_label, stmts) -> hasNoContinue stmts)
    | _ -> true)

let private simplifyBlock stmts = 
    let b = stmts
    // Avoid some optimizations when there are preprocessor directives.
    let hasPreprocessor = Seq.exists (function Verbatim _ -> true | _ -> false) b

    // Remove dead code after return/break/...
    let endOfCode = Seq.tryFindIndex (function Jump _ -> true | _ -> false) b
    let b = match endOfCode with
            | Some x when not hasPreprocessor -> List.truncate (x+1) b
            | _ -> b

    // Remove "empty" declarations of vars that were inlined. This is mandatory for correctness, not an optional optimization.
    let b = b |> List.filter (function
        | Decl (_, []) -> false
        | _ -> true)

    // Merge two consecutive items into one, everywhere possible in a list.
    let rec squeeze (f : 'a * 'a -> 'a option) = function
        | h1 :: h2 :: t ->
            match f (h1, h2) with
                | Some x -> squeeze f (x :: t)
                | None -> h1 :: (squeeze f (h2 :: t))
        | h :: t -> h :: t
        | [] -> []

    // Merge preceding expression into a for's init.
    let b = b |> squeeze (function
        | (Expr e, ForE (None, cond, inc, body)) -> // a=0;for(;i<5;++i);  ->  for(a=0;i<5;++i);
            Some (ForE (Some e, cond, inc, body))
        | (Expr e, While (cond, body)) -> // a=0;while(i<5);  ->  for(a=0;i<5;);
            Some (ForE(Some e, Some cond, None, body))
        | _ -> None)

    let b = b |> squeeze (function
        | Expr (FunCall(Op "=", [Var d1; e1])),
           (Expr (FunCall(Op "=", [Var d2; e2])) as stmt2) when d1.Name = d2.Name ->
            let refs = Analyzer.collectReferences [Expr e2]
            match refs.TryGetValue d1.Name with
            | false, _ -> Some stmt2
            | true, 1 ->
                let inliner _ = function
                    Var v when v.Name = d1.Name -> e1 | e -> e
                let e2 = mapExpr (mapEnv inliner id) e2
                Some (Expr (FunCall(Op "=", [Var d2; e2])))
            | _ -> None
        | _ -> None)

    // Inline inner decl-less blocks. (Presence of decl could lead to redefinitions.)  a();{b();}c();  ->  a();b();c();
    let b = b |> List.collect (function
        | Block b when hasNoDecl b -> b
        | e -> [e])

    // Remove useless else after a if that returns.
    // if(c)return a();else b();  ->  if(c)return a();b();
    let rec endsWithReturn = function
        | Jump(JumpKeyword.Return, _) -> true
        | Block stmts when not stmts.IsEmpty -> stmts |> List.last |> endsWithReturn
        | _ -> false
    let removeUselessElseAfterReturn = List.collect (function
        | If (cond, bodyT, Some bodyF) when endsWithReturn bodyT ->
            let bodyF = match bodyF with
                        | Block b when hasNoDecl b -> b // inline inner empty blocks without variable
                        | Decl _ as d -> [Block [d]] // a decl must stay isolated in a block, for the same reason
                        | s -> [s]
            If (cond, bodyT, None) :: bodyF
        | e -> [e])
    let b = removeUselessElseAfterReturn b

    // if(a)return b;return c;  ->  return a?b:c;
    let rec replaceIfReturnsWithReturnTernary = function
        | If (cond, Jump(JumpKeyword.Return, Some retT), None) :: Jump(JumpKeyword.Return, Some retF) :: _rest ->
            [Jump(JumpKeyword.Return, Some (FunCall(Op "?:", [cond; retT; retF])))]
        | stmt :: rest -> stmt :: replaceIfReturnsWithReturnTernary rest
        | stmts -> stmts
    let b = replaceIfReturnsWithReturnTernary b

    // Consecutive declarations of the same type become one.  float a;float b;  ->  float a,b;
    let b = squeezeConsecutiveDeclarations b

    // Group declarations, optionally (may compress poorly).  float a,f();float b=4.;  ->  float a,b;f();b=4.;
    let b = if hasPreprocessor || not options.moveDeclarations then b else groupDeclarations b
    b

let private simplifyStmt = function
    | Block [] as e -> e
    | Block b -> match simplifyBlock b with
                    | [stmt] -> stmt
                    | stmts -> Block stmts
    | Decl (ty, li) -> Decl (rwType ty, declsNotToInline li)
    | ForD ((ty, d), cond, inc, body) -> ForD((rwType ty, declsNotToInline d), cond, inc, squeezeBlockWithComma body)
    | ForE (init, cond, inc, body) -> ForE(init, cond, inc, squeezeBlockWithComma body)
    | While (cond, body) ->
        match body with
        | Expr e -> ForE (None, Some cond, Some e, Block []) // while(c)b();  ->  for(;c;b());
        | Block stmts ->
            match List.rev stmts with
            | Expr last :: revBody when hasNoContinue stmts && hasNoDecl stmts ->
                // This rewrite is only valid if:
                // * continue is never used in this loop, and
                // * the last expression of the body does not use any Decl from the body.
                let block = match (List.rev revBody) with
                            | [stmt] -> stmt
                            | stmts -> Block stmts
                ForE (None, Some cond, Some last, block) // while(c){a();b();}  ->  for(;c;b())a();
            | _ -> ForE (None, Some cond, None, squeezeBlockWithComma (Block stmts))
        | _ -> ForE (None, Some cond, None, squeezeBlockWithComma body)
    | DoWhile (cond, body) -> DoWhile (cond, squeezeBlockWithComma body)
    | If (True, e1, _) -> squeezeBlockWithComma e1
    | If (False, _, Some e2) -> squeezeBlockWithComma e2
    | If (False, _, None) -> Block []
    | If (cond, Block [], None) -> Expr cond // if(c);  ->  c;
    | If (cond, Block [], Some (Block [])) -> Expr cond // if(c)else{};  ->  c;
    | If (c, b, Some (Block [])) -> If(c, b, None) // "else{}"  ->  ""
    | If (cond, body1, body2) ->
        let (body1, body2) = squeezeBlockWithComma body1, Option.map squeezeBlockWithComma body2

        let (cond, body1, body2) = // if(!c)a();else b();  ->  if(c)b();else a();
            match (cond, body1, body2) with
            | FunCall (Op "!", [e]), bodyT, Some bodyF -> e, bodyF, Some bodyT
            | _ -> cond, body1, body2

        match (body1, body2) with
            | (Expr eT, Some (Expr eF)) ->
                let tryCollapseToAssignment : Expr -> (Ident * Expr) option = function
                    | FunCall (Op "=", [Var name; init]) -> Some (name, init)
                    | FunCall (Op ",", list) -> // f(),c=d  ->  c=f(),d
                        match List.last list with
                        | FunCall (Op "=", [Var name; init]) ->
                                let mutableList = new System.Collections.Generic.List<Expr>(list)
                                mutableList[mutableList.Count - 1] <- init
                                Some (name, FunCall (Op ",", Seq.toList(mutableList)))
                        | _ -> None
                    | _ -> None
                match (tryCollapseToAssignment eT, tryCollapseToAssignment eF) with
                    // turn if-else of assignments into assignment of ternary
                    | Some (nameT, initT), Some (nameF, initF) when nameT.Name = nameF.Name ->
                        // if(c)x=y;else x=z;  ->  x=c?y:z;
                        Expr (FunCall (Op "=", [Var nameT; FunCall(Op "?:", [cond; initT; initF])]))
                    // turn if-else of expressions into ternary statement
                    | _ ->
                        // if(c)x();else y();  ->  c?x():y();
                        // This transformation is not legal when x() and y() have different types.
                        // Expr (FunCall(Op "?:", [cond; eT; eF]))
                        If (cond, body1, body2)
            | _ -> If (cond, body1, body2)
    | Verbatim s -> Verbatim (stripSpaces s)
    | e -> e

let rec iterateSimplifyAndInline li =
    if not options.noInlining then
        Analyzer.markInlinableFunctions li
        let mapExpr _ e = e
        let mapStmt = function
            | Block b as e -> Analyzer.markInlinableVariables b; e
            | e -> e
        mapTopLevel (mapEnv mapExpr mapStmt) li |> ignore
    let didInline = ref false
    let simplified = mapTopLevel (mapEnv (simplifyExpr didInline) simplifyStmt) li

    // now that the functions were inlined, we can remove them
    let simplified = simplified |> List.filter (function
        | Function (funcType, _) -> not funcType.fName.ToBeInlined || funcType.fName.Name.StartsWith("i_")
        | _ -> true)

    if didInline.Value then iterateSimplifyAndInline simplified else simplified

let simplify li =
    li
    // markLValues doesn't change the AST so we could do it unconditionally,
    // but we only need the information for aggroInlining so don't bother if
    // it's off.
    |> Analyzer.markLValues
    |> Analyzer.maybeInlineConsts
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


type private CallGraphNode = {
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


// get the list of external values the function depends on
let private computeDependencies f =
    let d = HashSet()
    let collect mEnv = function
        | Var id as e ->
            if not (mEnv.vars.ContainsKey(id.Name)) then d.Add id.Name |> ignore
            e
        | e -> e
    mapTopLevel (mapEnv collect id) [f] |> ignore
    d |> Seq.toList

// This function assumes that functions are NOT overloaded
let private computeAllDependencies code =
    let functions = code |> List.choose (function
        | Function(funcType, _) as f -> Some (funcType, funcType.fName.Name, f)
        | _ -> None)
    let nodes = functions |> List.map (fun (funcType, name, f) ->
        let callees = computeDependencies f
                      |> List.filter (fun name2 -> functions |> List.exists (fun (_,n,_) -> name2 = n))
        { CallGraphNode.func = f; funcType = funcType; name = name; callees = callees })
    nodes


let rec removeUnused code =
    let nodes = computeAllDependencies code
    let isUnused node =
        let canBeRenamed = not (options.noRenamingList |> List.contains node.name) // noRenamingList includes "main"
        let isCalled = (nodes |> List.exists (fun n -> n.callees |> List.contains node.name))
        let isExternal = options.hlsl && node.funcType.semantics <> []
        canBeRenamed && not isCalled && not isExternal
    let unused = set [for node in nodes do if isUnused node then yield node.func]
    let mutable edited = false
    let code = code |> List.filter (function
        | Function _ as t -> if Set.contains t unused then edited <- true; false else true
        | _ -> true)
    if edited then removeUnused code else code

// reorder functions if there were forward declarations
let reorder code =
    if options.verbose then
        printfn "Reordering functions because of forward declarations."
    let order = code |> computeAllDependencies |> graphReorder
    let rest = code |> List.filter (function Function _ -> false | _ -> true)
    rest @ order
