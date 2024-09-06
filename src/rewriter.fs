﻿module internal Rewriter

open System
open System.Collections.Generic
open Builtin
open Ast
open Inlining
open Analyzer

let private commaSeparatedExprs = List.reduce (fun a b -> FunCall(Op ",", [a; b]))

let private isKnownToHaveTypeFloat = function
    | Float _ -> true
    | ResolvedVariableUse (_, vd) -> vd.decl.name.Name = "float"
    | _ -> false

[<RequireQualifiedAccess>] 
type private OptimizationPass =
    | First
    | Second // adds "var reuse"

type private RewriterImpl(options: Options.Options, optimizationPass: OptimizationPass) =

    // Remove useless spaces in macros
    let stripSpaces str =
        let result = Text.StringBuilder()

        let mutable last = '\n'
        let write c =
            last <- c
            result.Append(c) |> ignore
        let isId c = Char.IsLetterOrDigit c || c = '_'

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

    let stripDirectiveSpaces = function
        | ["#define"; name; value] ->
            // we need to distinguish between " #define f (x)" and "#define f(x)"
            let spacePrefix = if value.StartsWith(" ") then " " else ""
            ["#define"; name; spacePrefix + stripSpaces value]
        | li -> List.map stripSpaces li

    let declsNotToInline (d: DeclElt list) = d |> List.filter (fun x -> not x.name.ToBeInlined)

    let bool = function
        | true -> Var (Ident "true") // Int (1, "")
        | false -> Var (Ident "false") // Int (0, "")

    let inlineFn (declArgs:Decl list) passedArgs bodyExpr =
        let mutable argMap = Map.empty
        for declArg, passedArg in List.zip declArgs passedArgs do
            let declElt = List.exactlyOne (snd declArg)
            argMap <- argMap.Add(declElt.name.Name, passedArg)
        let mapInline _ = function
            | Var iv ->
                match argMap.TryFind iv.Name with
                | Some inlinedExpr -> inlinedExpr
                // This var isn't an argument to the inlined function (must be a
                // global or similar). We need to create a brand-new ident.  This is
                // because the renamer does its work via mutation on the ident. So
                // if this function gets inlined in more than one place, we don't
                // mutations to affect all of the inlined idents.
                | None -> Var (Ident (iv.Name, iv.Loc.line, iv.Loc.col))
            | ie -> ie
        options.visitor(mapInline).mapExpr bodyExpr

    // Expression that doesn't need parentheses around it.
    let (|NoParen|_|) = function
        | Int _ | Float _ | Dot _ | Var _ | FunCall (Var _, _) | Subscript _ as x -> Some x
        | _ -> None

    // Expression that statically evaluates to a boolean value.
    let (|True|False|NotABool|) = function
        | Int (i, _) when i <> 0 -> True
        | Int (i, _) when i = 0 -> False
        | Float (f, _) when f <> 0.M -> True
        | Float (f, _) when f = 0.M -> False
        | Var var when var.Name = "true" -> True
        | Var var when var.Name = "false" -> False
        | _ -> NotABool

    // Expression that statically evaluates to a numeric value.
    let (|Number|_|) = function
        | Int (i, _) -> Some (decimal i)
        | Float (f, _) -> Some f
        | _ -> None
    
    // Expression that is equivalent to an assignment.
    let (|Assignment|_|) = function
        | FunCall (Op "=", [Var v; e]) -> Some (v, e)
        | FunCall (Op op, [Var name; e]) when Builtin.assignOps.Contains op ->
            let baseOp = op.TrimEnd('=')
            if not (Builtin.augmentableOperators.Contains baseOp)
            then None
            else let augmentedE = FunCall (Op baseOp, [Var name; e])
                 Some (name, augmentedE)
        | _ -> None

    let simplifyOperator env = function
        | FunCall(Op "-", [Int (i1, su)]) -> Int (-i1, su)
        | FunCall(Op "-", [FunCall(Op "-", [e])]) -> e
        | FunCall(Op "+", [e]) -> e
        // e1 - - e2 -> e1 + e2
        | FunCall(Op "-", [e1; FunCall(Op "-", [e2])]) ->
            FunCall(Op "+", [e1; e2]) |> env.fExpr env
        // e1 + - e2 -> e1 - e2
        | FunCall(Op "+", [e1; FunCall(Op "-", [e2])]) ->
            FunCall(Op "-", [e1; e2]) |> env.fExpr env

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
        | FunCall(Op "/", [Int (i1, su); Int (i2, _)]) when i2 <> 0 -> Int (i1 / i2, su)
        | FunCall(Op "%", [Int (i1, su); Int (i2, _)]) -> Int (i1 % i2, su)

        | FunCall(Op "-", [Float (0.M,su)]) -> Float (0.M, su)
        | FunCall(Op "-", [Float (f1,su)]) -> Float (-f1, su)
        | FunCall(Op "-", [Float (i1,su); Float (i2,_)]) -> Float (i1 - i2, su)
        | FunCall(Op "+", [Float (i1,su); Float (i2,_)]) -> Float (i1 + i2, su)
        | FunCall(Op "*", [Float (i1,su); Float (i2,_)]) -> Float (i1 * i2, su)
        | FunCall(Op "/", [Float (i1,su); Float (i2,_)]) as e when i2 <> 0m ->
            let div = Float (i1 / i2, su)
            if (Printer.exprToS e).Length <= (Printer.exprToS div).Length then e
            else div

        // Swap operands to get rid of parentheses
        // x*(y*z) -> y*z*x
        | FunCall(Op "*", [NoParen x; FunCall(Op "*", [y; z])])
            when Effects.isPure x && Effects.isPure y && Effects.isPure z ->
            FunCall(Op "*", [FunCall(Op "*", [y; z]); x]) |> env.fExpr env
        // x+(y+z) -> x+y+z
        // x+(y-z) -> x+y-z
        | FunCall(Op "+", [NoParen x; FunCall(Op ("+"|"-") as op, [y; z])]) ->
            FunCall(op, [FunCall(Op "+", [x; y]); z]) |> env.fExpr env
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

        // a=a;  ->  a
        | FunCall(Op "=", [Var x; Var y]) when x.Name = y.Name -> Var y
        // x=x+...  ->  x+=...
        | FunCall(Op "=", [Var x; FunCall(Op op, [Var y; e])])
                when x.Name = y.Name && Builtin.augmentableOperators.Contains op ->
            FunCall(Op (op + "="), [Var x; e])

        // x=...+x  ->  x+=...
        // Works only if the operator is commutative. * is not commutative with vectors and matrices.
        | FunCall(Op "=", [ResolvedVariableUse (v, vd); FunCall(Op ("+"|"*"|"&"|"^"|"|" as op), [e; Var y])])
                when v.Name = y.Name
                && (op <> "*" || vd.ty.isScalar) // * is commutative when at least one operand is scalar
                ->
            FunCall(Op (op + "="), [Var v; e])

        // Unsafe when x contains NaN or Inf values.
        //| FunCall(Op "=", [Var x; FunCall(Var fctName, [Int (0, _)])])
        //    when List.contains fctName.Name ["vec2"; "vec3"; "vec4"; "ivec2"; "ivec3"; "ivec4"] ->
        //    FunCall(Op "-=", [Var x; Var x]) // x=vec3(0);  ->  x-=x;
        | e -> e


    // Simplify calls to the vec constructor.
    let simplifyVec (constr: Ident) args =
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
            | Float (f, _) as e when optimizationPass = OptimizationPass.Second // only do this after other transforms that can apply only to floats.
                && Decimal.Round(f) = f ->
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
        let rec dropLastSwizzle n = function
            | [Dot (expr, field) as last] when isFieldSwizzle field ->
                match [for c in field -> swizzleIndex c] with
                | [0] when n = 1 -> [expr]
                | [0; 1] | [0; 1; 2] | [0; 1; 2; 3] -> [expr]
                | _ -> [last]
            | e1 :: rest -> e1 :: dropLastSwizzle (n-1) rest
            | x -> x

        let args = combineSwizzles args |> List.map useInts
        let args = if args.Length = vecSize then mergeAllEquals args args else args
        let args = dropLastSwizzle vecSize args
        FunCall (Var constr, args)

    let simplifyVecDot (vecName: string) args (field: string) e =
        let vecSize = vecName.ToCharArray() |> Array.last |> string |> int
        if not (
            args |> Seq.forall Effects.isPure && // check that arguments can be reordered
            args |> List.length = vecSize // check that the Nth swizzle index maps to the Nth arg
        )
        then e
        else
            let indexes = field.ToCharArray() |> Seq.map Builtin.swizzleIndex |> Seq.toList
            match indexes |> List.length with
            | 1 -> // vec3(a,b,c).y  ->  b
                match List.tryItem indexes.Head args with
                | Some arg when isKnownToHaveTypeFloat arg -> arg
                | _ -> e
            | 2 | 3 | 4 -> // vec3(a,b,c).yx  ->  vec2(b,a)
                // find whether the repeated fields are repeatable exprs (don't repeat function calls or long exprs)
                let repeatedIndexes = indexes |> List.countBy id |> List.filter (fun (_, count) -> count > 1) |> List.map (fun (key, _) -> key)
                let isRepeatableExpr = function
                    | Var _ | Int _ | Float _ -> true
                    | _ -> false
                if repeatedIndexes |> List.forall (fun index -> isRepeatableExpr args[index]) then
                    let constructor = Ident("vec" + string (indexes |> List.length))
                    FunCall(Var constructor, indexes |> List.map (fun index -> args[index]))
                else e
            | _ -> e

    let simplifyExpr (didInline: bool ref) env = function
        | FunCall(Var v, passedArgs) as e when v.ToBeInlined ->
            match env.fns.TryFind (v.Name, passedArgs.Length) with
            | Some ([{args = declArgs}, body]) ->
                if List.length declArgs <> List.length passedArgs then
                    failwithf "Cannot inline function %s since it doesn't have the right number of arguments" v.Name
                match body.asStmtList with
                | [Jump (JumpKeyword.Return, Some bodyExpr)] ->
                    didInline.Value <- true
                    inlineFn declArgs passedArgs bodyExpr
                // Don't yell if we've done some inlining this pass -- maybe it
                // turned the function into a one-liner, so allow trying again on
                // the next pass. (If it didn't, we'll yell next pass.)
                | _ when didInline.Value -> e
                | _ -> failwithf "Cannot inline function %s since it consists of more than a single return" v.Name
            | None -> failwithf "Cannot inline function %s because it's a builtin" v.Name
            | _ -> failwithf "Cannot inline function %s because type-based disambiguation of user-defined function overloading is not supported" v.Name

        | FunCall(Op _, _) as op -> simplifyOperator env op

        | FunCall(Var dist, [arg1; arg2]) when dist.Name = "distance" ->
            let len = Ident("length", dist.Loc.line, dist.Loc.col)
            FunCall(Var len, [FunCall (Op "-", [arg1; arg2])])

        | FunCall(Var constr, args) when constr.Name = "vec2" || constr.Name = "vec3" || constr.Name = "vec4" ->
            simplifyVec constr args

        | Dot(FunCall(Var constr, args), field) as e when (constr.Name = "vec2" || constr.Name = "vec3" || constr.Name = "vec4") ->
            let e = simplifyVecDot constr.Name args field e
            match e with
            | Dot(e, field) when options.canonicalFieldNames <> "" -> Dot(e, options.renameField field)
            | _ -> e
        | Dot(e, field) when options.canonicalFieldNames <> "" -> Dot(e, options.renameField field)

        | ResolvedVariableUse (_, vd) as e when vd.decl.name.ToBeInlined ->
            // Replace uses of inlined variables.
            match vd.decl.init with
            | Some init ->
                didInline.Value <- true
                init |> env.mapExpr
            | _ -> e

        | FunCall(Var var, [e; Number 1.M]) when var.Name = "pow" -> e // pow(x, 1.)  ->  x

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

    // Squeeze declarations: "float a=2.; float b;"  ->  "float a=2.,b;"
    let rec squeezeConsecutiveDeclarations = function
        | []-> []
        | Decl(ty1, li1) :: Decl(ty2, li2) :: l when ty1 = ty2 ->
            squeezeConsecutiveDeclarations (Decl(ty1, li1 @ li2) :: l)
        | e::l -> e :: squeezeConsecutiveDeclarations l

    // Squeeze top-level declarations, e.g. uniforms
    let rec squeezeTLDeclarations = function
        | [] -> []
        | TLDecl(ty1, li1) :: TLDecl(ty2, li2) :: l when ty1 = ty2 ->
            squeezeTLDeclarations (TLDecl(ty1, li1 @ li2) :: l)
        | e::l -> e :: squeezeTLDeclarations l

    let rwTypeSpec = function
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
        | Block stmts -> hasNoContinue stmts
        // No need to search in nested for loops, because their continue wouldn't apply to this level.
        | _ -> true)

    // Reuse an existing local variable declaration that won't be used anymore, instead of introducing a new one.
    // The reused identifier gets compressed better, and the declaration is sometimes removed.
    // float d1 = f(); float d2 = g();  ->  float d1 = f(); d1 = g();
    let reuseExistingVarDecl blockLevel b =
        let tryReplaceWithPrecedingAndFollowing f (xs : Stmt list) =
            let rec go f preceding = function
                | head :: tail ->
                    match f (preceding, head, tail) with
                    | None -> head :: go f (head :: preceding) tail // preceding is reversed, that's good
                    | Some xs -> xs @ tail
                | [] -> []
            go f [] xs
        b |> tryReplaceWithPrecedingAndFollowing (function
        | (preceding2, Decl (ty2, declElts), following2) ->
            let findAssignmentReplacementFor declElt2 declBefore2 declAfter2 =
                // Collect previous declarations of the same type.
                let localDecls = (preceding2 @ declBefore2) |> List.collect (function Decl (ty1, declElts1) when ty2 = ty1 -> declElts1 | _ -> [])
                let args =
                    match blockLevel with
                    | BlockLevel.FunctionRoot fct ->
                        fct.parameters |> List.choose (function ty, decl -> if not ty.isOutOrInout && ty = ty2 then Some decl else None)
                    | _ -> []

                let compatibleDeclElt = (localDecls @ args) |> List.tryFind (fun declElt1 ->
                    declElt1.size = declElt2.size &&
                    declElt1.semantics = declElt2.semantics &&
                    // The first variable must not be used after the second is declared.
                    Analyzer(options).varUsesInStmt (Block (declAfter2 @ following2)) |> List.forall (fun i -> i.Name <> declElt1.name.Name)
                )

                match compatibleDeclElt with
                | None -> None
                | Some declElt1 ->
                    options.trace $"{declElt2.name.Loc}: eliminating local variable '{declElt2.name}' by reusing existing local variable '{declElt1.name}'"
                    for v in Analyzer(options).varUsesInStmt (Block (declAfter2 @ following2)) do // Rename all uses of var2 to use var1 instead.
                        if v.Name = declElt2.name.Name then
                            v.Rename(declElt1.name.Name)
                    match declElt2.init with
                    | Some init -> Some [Expr (FunCall (Op "=", [Var declElt1.name; init]))]
                    | None -> Some []
            // For a decl sandwiched between others, we could consider moving the assignment into a comma-expr of the init of the next decl, but this adds parentheses.
            match declElts with
            | [declElt2]  -> // float d1=f(); ...; float d2=g();  ->  ...; d1=g();
                match findAssignmentReplacementFor declElt2 [] [] with
                | Some stmts -> Some stmts // Replace the Decl with the assignment. Largest win
                | None -> None
            | declElt2 :: others -> // float d1=f(); ...; float d2=g(),d3=h();  ->  ...; d1=g(); float d3=h();
                let restOfTheDecl = [Decl (ty2, others)]
                match findAssignmentReplacementFor declElt2 [] restOfTheDecl with
                | Some stmts -> Some (stmts @ restOfTheDecl) // Keep the Decl, add an assignment before it.
                | None ->
                    match declElts |> List.rev with
                    | declElt2 :: reversedOthers -> // float d1=f(); ...; float d3=h(),d2=g();  ->  ...; float d3=h(); d1=g();
                        let restOfTheDecl = [Decl (ty2, reversedOthers |> List.rev)]
                        match findAssignmentReplacementFor declElt2 restOfTheDecl [] with
                        | Some stmts -> Some (restOfTheDecl @ stmts) // Keep the Decl, add an assignment after it.
                        | None -> None
                    | _ -> None
            | _ -> None
        | _ -> None)

    let simplifyBlock blockLevel stmts = 
        let b = stmts
        // Avoid some optimizations when there are preprocessor directives.
        let hasPreprocessor = Seq.exists (function Verbatim _ | Directive _ -> true | _ -> false) b

        // Remove dead code after return/break/...
        let endOfCode = Seq.tryFindIndex (function Jump _ -> true | _ -> false) b
        let b = match endOfCode with
                | Some x when not hasPreprocessor -> List.truncate (x+1) b
                | _ -> b

        // Remove "empty" declarations of vars that were inlined. This is mandatory for correctness, not an optional optimization.
        let b = b |> List.filter (function
            | Decl (_, []) -> false
            | _ -> true)

        let countUsesOfIdentName expr identName =
            Analyzer(options).varUsesInStmt (Expr expr) |> List.filter (fun i -> i.Name = identName) |> List.length

        let replaceUsesOfIdentByExpr expr identName replacement =
            let visitAndReplace _ = function
                | Var v when v.Name = identName -> replacement
                | e -> e
            options.visitor(visitAndReplace).mapExpr expr

        // Merge two consecutive items into one, everywhere possible in a list.
        let rec squeeze (f : 'a * 'a -> 'a list option) = function
            | h1 :: h2 :: t ->
                match f (h1, h2) with
                    | Some xs -> squeeze f (xs @ t)
                    | None -> h1 :: (squeeze f (h2 :: t))
            | h :: t -> h :: t
            | [] -> []
        let b = b |> squeeze (function
            // Merge preceding expression into a for's init.
            | (Expr e, ForE (None, cond, inc, body)) -> // a=0;for(;i<5;++i);  ->  for(a=0;i<5;++i);
                Some [ForE (Some e, cond, inc, body)]
            | (Expr e, While (cond, body)) -> // a=0;while(i<5);  ->  for(a=0;i<5;);
                Some [ForE(Some e, Some cond, None, body)]
            | Decl (_, [declElt]), Jump(JumpKeyword.Return, Some (Var v)) // int x=f();return x;  ->  return f();
                when v.Name = declElt.name.Name && declElt.init.IsSome ->
                    Some [Jump(JumpKeyword.Return, declElt.init)]
            | Expr (Assignment (v1, e)), Jump(JumpKeyword.Return, Some (ResolvedVariableUse (v2, vd))) // x=f();return x;  ->  return f();
                when v1.Name = v2.Name && vd.scope <> VarScope.Global && not (vd.ty.isOutOrInout) ->
                    Some [Jump(JumpKeyword.Return, Some e)]
            // assignment to a local immediately followed by re-assignment
            | Expr (Assignment (name, init1)), (Expr (Assignment (name2, init2)) as assign2)
                when name.Name = name2.Name ->
                match name.Declaration with
                | Declaration.Variable decl when decl.scope = VarScope.Global ->
                    // The assignment should not be removed if init2 calls a function that reads the global variable.
                    None // Safely assume it could happen.
                | Declaration.Variable _ ->
                    match countUsesOfIdentName init2 name.Name with
                    // Remove unused assignment immediately followed by re-assignment:  m=14.;m=58.;  ->  14.;m=58.;
                    | 0 -> Some [Expr init1; assign2] // Transform is safe even if the var is an out parameter.
                    // Inline this single use of a used-once assignment into the immediately following re-assignment:  m=14.;m=58.-m;  ->  m=58.-14.;
                    | 1 when Effects.isPure init1 && Effects.isPure init2 -> // This is ok only if init1 is pure and the part of init2 before using m is pure.
                        let newInit2 = replaceUsesOfIdentByExpr init2 name.Name init1
                        options.trace $"{name.Loc}: merge consecutive pure assignments to the same local '{name}'"
                        Some [Expr (FunCall (Op "=", [Var name2; newInit2]))]
                    | _ -> None
                | _ -> None
            // declaration immediately followed by re-assignment
            | Decl (ty, [declElt]), Expr (Assignment (name2, init2))
                when declElt.name.Name = name2.Name ->
                    match countUsesOfIdentName init2 declElt.name.Name with
                    | 0 ->
                        match declElt.init |> Option.map Effects.sideEffects |> Option.defaultValue [] with
                        // float m=14;m=58.;  ->  float m=58.;
                        | [] -> Some [Decl (ty, [{declElt with init = Some init2}])]
                        // float m=f();m=58.;  ->  float m=(f(),58.);
                        | es -> Some [Decl (ty, [{declElt with init = Some (commaSeparatedExprs (es @ [init2]))}])]
                    | 1 when Option.forall Effects.isPure declElt.init && Effects.isPure init2 ->
                        match declElt.init with
                        | None -> None // can't replace  float a;a=f(a);  by  float a=f(a);
                        | Some init1 -> // float m=14.;m=58.-m;  ->  float m=58.-14.;
                            options.trace $"{declElt.name.Loc}: merge assignment with preceding local declaration '{Printer.debugDecl declElt}'"
                            let newInit = replaceUsesOfIdentByExpr init2 declElt.name.Name init1
                            Some [Decl (ty, [{declElt with init = Some newInit}])]
                    | _ -> None
            | _ -> None)

        // Reduces impure expression statements to their side effects.
        let b = b |> List.collect (function
            | Expr e ->
                match Effects.sideEffects e with
                | [] -> [] // Remove pure statements.
                | [e] -> [Expr e]
                | sideEffects -> [Expr (commaSeparatedExprs sideEffects)]
            | s -> [s])

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

        let b =
            if optimizationPass <> OptimizationPass.Second &&
                // We ensure control flow analysis is trivial by only doing this at the root block level.
                (match blockLevel with BlockLevel.FunctionRoot _ -> false | _ -> true) || hasPreprocessor
            then b
            else reuseExistingVarDecl blockLevel b

        // Consecutive declarations of the same type become one.  float a;float b;  ->  float a,b;
        let b = squeezeConsecutiveDeclarations b

        // Group declarations, optionally (may compress poorly).  float a,f();float b=4.;  ->  float a,b;f();b=4.;
        let b = if hasPreprocessor || not options.moveDeclarations then b else groupDeclarations b
        b

    let simplifyStmt (env : MapEnv) = function
        | Block [] as e -> e
        | Block b -> match simplifyBlock env.blockLevel b with
                        | [stmt] as b when hasNoDecl b -> stmt
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
                        | Assignment (name, init) -> Some (name, init)
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
        | Directive d -> Directive (stripDirectiveSpaces d)
        | e -> e

    static let rec removeUnusedFunctions (options: Options.Options) code =
        let funcInfos = Analyzer(options).findFuncInfos code
        let isUnused (funcInfo : FuncInfo) =
            let canBeRenamed = not (options.noRenamingList |> List.contains funcInfo.name) // noRenamingList includes "main"
            let isCalled = funcInfos |> List.exists (fun n ->
                n.callSites
                |> List.map (fun c -> c.prototype)
                |> List.contains funcInfo.funcType.prototype) // when in doubt wrt overload resolution, keep the function.
            canBeRenamed && not isCalled && not (funcInfo.funcType.isExternal(options))
        let unused = [for funcInfo in funcInfos do if isUnused funcInfo then yield funcInfo]
        if not unused.IsEmpty then
            options.trace($"removing unused functions: " + String.Join(", ", unused |> List.map (fun fi -> Printer.debugFunc fi.funcType)))
        let unused = unused |> List.map (fun fi -> fi.func) |> set
        let mutable edited = false
        let code = code |> List.filter (function
            | Function _ as t -> if Set.contains t unused then edited <- true; false else true
            | _ -> true)
        if edited then removeUnusedFunctions options code else code

    let cleanup tl =
        tl |> List.choose (function
            | TLDecl (ty, li) ->
                let li = declsNotToInline li
                if li = [] then None else TLDecl (rwType ty, li) |> Some
            | TLVerbatim s -> TLVerbatim (stripSpaces s) |> Some
            | TLDirective d -> TLDirective (stripDirectiveSpaces d) |> Some
            | Function (fct, _) when fct.fName.ToBeInlined -> None
            | Function (fct, body) -> Function (rwFType fct, body) |> Some
            | e -> e |> Some
        )
        |> squeezeTLDeclarations

    member _.Cleanup = cleanup 
    member _.SimplifyExpr = simplifyExpr
    member _.SimplifyStmt = simplifyStmt
    static member RemoveUnusedFunctions = removeUnusedFunctions


// reorder functions if there were forward declarations
let reorderFunctions (options: Options.Options) code =
    if options.verbose then
        printfn "Reordering functions because of forward declarations."
    
    let rec graphReorder = function // slow, but who cares?
        | [] -> []
        | (nodes : FuncInfo list) ->
            // Find a function that doesn't call anything else
            let node = nodes |> List.tryFind (fun node -> node.callSites.IsEmpty)
                             |> Option.defaultWith (fun _ -> failwith "Cannot reorder functions (probably because of a recursion).")
            // Remove that function from the graph
            let nodes = nodes |> List.except [node]
            // Remove that function from the callSites. This step assumes no type-based overloading.
            let nodes = nodes |> List.map (fun n -> { n with callSites = List.filter (fun c -> c.prototype <> node.funcType.prototype) n.callSites })
            // Recurse
            node.func :: graphReorder nodes

    let order = code |> Analyzer(options).findFuncInfos |> graphReorder
    let rest = code |> List.filter (function Function _ -> false | _ -> true)
    rest @ order


let rec private iterateSimplifyAndInline (options: Options.Options) optimizationPass passCount li =
    let li = if not options.noRemoveUnused then RewriterImpl.RemoveUnusedFunctions options li else li
    Analyzer(options).resolve li
    Analyzer(options).markWrites li
    if not options.noInlining then
        FunctionInlining(options).MarkInlinableFunctions li
        VariableInlining(options).MarkInlinableVariables li
    let didInline = ref false
    let before = Printer.print li
    let rewriter = RewriterImpl(options, optimizationPass)
    let li = options.visitor(rewriter.SimplifyExpr didInline, rewriter.SimplifyStmt).mapTopLevel li

    // now that the functions were inlined, we can remove them
    let li = li |> List.filter (function
        | Function (funcType, _) -> not funcType.fName.ToBeInlined || funcType.fName.Name.StartsWith("i_")
        | _ -> true)
    
    let li = if options.noInlining then li else ArgumentInlining(options).Apply didInline li

    if passCount > 20 then
        options.trace $"! possible unstable loop in change detection. stopping analysis."
        li
    else
        let after = Printer.print li
        if after <> before then
            options.trace $"- significant changes happened: running analysis again..."
            iterateSimplifyAndInline options optimizationPass (passCount + 1) li
        else li

let simplify options li =
    li
    |> iterateSimplifyAndInline options OptimizationPass.First 1
    |> iterateSimplifyAndInline options OptimizationPass.Second 1
    |> RewriterImpl(options, OptimizationPass.First).Cleanup
