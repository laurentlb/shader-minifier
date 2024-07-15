module Ast

[<RequireQualifiedAccess>]
type VarScope = Global | Local | Parameter

[<StructuredFormatDisplay("{line}:{col}")>]
type Location = { line: int; col: int }

type Ident(name: string) =
    let mutable newName = name

    member this.Name = newName
    member this.OldName = name
    member this.Rename(n) = newName <- n
    member val ToBeInlined = newName.StartsWith("i_") with get, set
    // This prefix disables function inlining and variable inlining.
    member this.DoNotInline = this.OldName.StartsWith("noinline_")

    member val Loc = {line = -1; col = -1} with get, set

    new(name, lineNb, colNb) as this = Ident(name) then
        this.Loc <- {line = lineNb; col = colNb}

    //member val isVarRead: bool = false with get, set
    member val isVarWrite: bool = false with get, set
    member val Declaration: Declaration = Declaration.Unknown with get, set
    member this.VarDecl = match this.Declaration with
                          | Declaration.Variable rv -> Some rv
                          | _ -> None

    // Real identifiers cannot start with a digit, but the temporary ids of the rename pass are numbers.
    member this.IsUniqueId = System.Char.IsDigit this.Name.[0]

    interface System.IComparable with
        member this.CompareTo other =
            match other with
            | :? Ident as o -> this.Name.CompareTo(o.Name)
            | _ -> failwith "not comparable"
    override this.Equals other =
        match other with
        | :? Ident as o -> this.Name = o.Name  
        | _ -> false
    override this.GetHashCode() = name.GetHashCode()
    override this.ToString() = $"<{name}>"

and [<NoComparison>] [<RequireQualifiedAccess>] Declaration =
    | Unknown
    | Variable of VarDecl
    | UserFunction of FunDecl
    | BuiltinFunction
    | UnknownFunction // ambiguous (type-based) overloading, builtin unknown to minifier, or builtin redefinition
and VarDecl(ty, decl, scope) =
    member val ty: Type = ty with get
    member val decl = decl: DeclElt with get
    member val scope = scope: VarScope with get
    member val isEverWrittenAfterDecl = false with get, set
and FunDecl(func, funcType) =
    member val func: TopLevel = func with get
    member val funcType: FunctionType = funcType with get
    member val hasExternallyVisibleSideEffects = false with get, set
and [<RequireQualifiedAccess>] JumpKeyword = Break | Continue | Discard | Return

and Expr =
    | Int of int64 * string
    | Float of decimal * string
    | Var of Ident // 'Var' can be an identifier referencing a variable or a function! (or a macro)
    | Op of string
    | FunCall of Expr * Expr list // The first Expr of a FunCall can be: Op, Var, Subscript, or Dot.
    | Subscript of Expr * Expr option
    | Dot of Expr * string
    | Cast of Ident * Expr  // hlsl
    | VectorExp of Expr list // hlsl
    | VerbatimExp of string
    // Examples:
    // * "i++" = FunCall (Op "$++", [Var ident: i])
    // * "sin(1.0)" = FunCall (Var ident: sin, [Float (1.0M, "")])
    // * "float[2](8.,9.)" = FunCall (Subscript (Var ident: float, Some (Int (2L, ""))), [Float (8.0M, ""); Float (9.0M, "")])
    // * "array.length()" = FunCall (Dot (Var ident: array, "length"), [])

and TypeSpec =
    | TypeName of string
    // struct or interface block, e.g. struct foo { float x; float y; }
    | TypeBlock of string(*type*) * Ident option(*name*) * Decl list
    // An interface block followed by an instance name (in a TLDecl), like structs, declares an instance.
    // An interface block without an instance name (in a TypeDecl), unlike structs, introduces a set of external global variables.

and Type = {
    name: TypeSpec // e.g. int
    typeQ: string list // type qualifiers, e.g. const, uniform, out, inout...
    arraySizes: Expr list // e.g. [3][5]
} with
    member this.isOutOrInout =
        not (Set.intersect (set this.typeQ) (set ["out"; "inout"])).IsEmpty
    member this.IsExternal =
        List.exists (fun s -> Set.contains s Builtin.externalQualifiers) this.typeQ
    member this.isScalar =
        match this.name with
            | TypeName n -> Builtin.builtinScalarTypes.Contains n
            | _ -> false
    override t.ToString() =
        let name = match t.name with
                   | TypeName n -> n
                   | TypeBlock _ -> $"{t.name}"
        if t.typeQ.IsEmpty && t.arraySizes.IsEmpty
            then $"{name}"
            else $"<{t.typeQ} {name} {t.arraySizes}>"

and DeclElt = {
    name: Ident // e.g. foo
    size: Expr option // e.g. [3]
    semantics: Expr list // e.g. : color
    init: Expr option // e.g. = f(x)
} with override t.ToString() =
        let size = if t.size = None then "" else $"[{t.size}]"
        let init = if t.init = None then "" else $" = {t.init}"
        let sem = if t.semantics.IsEmpty then "" else $": {t.semantics}" in
        $"{t.name}{size}{init}{sem}"

and Decl = Type * DeclElt list

and Stmt =
    | Block of Stmt list
    | Decl of Decl
    | Expr of Expr
    | If of Expr * Stmt (*then*) * Stmt option (*else*)
    | ForD of Decl * Expr option * Expr option * Stmt (*for loop starting with a declaration*)
    | ForE of Expr option * Expr option * Expr option * Stmt (*for loop starting with an expression*)
    | While of Expr * Stmt
    | DoWhile of Expr * Stmt
    | Jump of JumpKeyword * Expr option (*break, continue, return (expr)?, discard*)
    | Verbatim of string
    | Directive of string list // ["#define"; "name"; "value"]
    | Switch of Expr * (CaseLabel * Stmt list) list
with member this.asStmtList = match this with
                              | Block stmts -> stmts
                              | stmt -> [stmt]

and CaseLabel =
    | Case of Expr
    | Default

and FunctionType = {
    retType: Type (*return*)
    fName: Ident (*name*)
    args: Decl list (*args*)
    semantics: Expr list (*semantics*)
} with
    member this.isExternal(options: Options.Options) = options.hlsl && this.semantics <> []
    member this.hasOutOrInoutParams =
        let typeQualifiers = set [for (ty, _) in this.args do yield! ty.typeQ]
        not (Set.intersect typeQualifiers (set ["out"; "inout"])).IsEmpty
    member this.prototype = (this.fName.Name, this.args.Length)
    member this.parameters: (Type * DeclElt) list =
        // Helper to extract the single DeclElt for each arg.
        [ for (ty, declElts) in this.args do
          yield match declElts with
                | [declElt] -> ty, declElt
                | _ -> failwith "invalid declElt for function argument" ]
    override t.ToString() =
        let sem = if t.semantics.IsEmpty then "" else $": {t.semantics}" in
        let args = System.String.Join(", ", t.args |> List.map (function
            | ty, [d] -> $"{ty} {d}"
            | _ -> $"{t.args}"
            ))
        $"{t.retType} {t.fName} ({args}){sem}"

and TopLevel =
    | TLVerbatim of string
    | TLDirective of string list
    | Function of FunctionType * Stmt
    | TLDecl of Decl
    | TypeDecl of TypeSpec // structs or interface blocks
    | Precision of Type

let makeType name tyQ sizes = {Type.name=name; typeQ=tyQ; arraySizes=sizes}
let makeDecl name size sem init = {name=name; size=size; semantics=sem; init=init}
let makeFunctionType ty name args sem =
    {retType=ty; fName=name; args=args; semantics=sem}

// An ExportedName is a name that is used outside of the shader code (e.g. uniform and attribute
// values). We need to provide accessors for the developer (e.g. create macros for C/C++).
type ExportPrefix =
    | Variable
    | HlslFunction
type ExportedName = {
    prefix: ExportPrefix
    name: string
    newName: string
}

type Shader = {
    filename: string
    code: TopLevel list
    forbiddenNames: string list
    reorderFunctions: bool  // set to true if we saw a forward declaration
} with member this.mangledFilename = (System.IO.Path.GetFileName this.filename).Replace(".", "_")

// mapEnv is a kind of visitor that applies transformations to statements and expressions,
// while also collecting visible variable and function declarations along the way.

[<NoComparison>] [<RequireQualifiedAccess>]
type BlockLevel = FunctionRoot of FunctionType | Nested | Unknown

[<RequireQualifiedAccess>]
type Level = TopLevel | InFunc

[<NoComparison; NoEquality>]
type MapEnv = {
    fExpr: MapEnv -> Expr -> Expr
    fStmt: MapEnv -> Stmt -> Stmt
    vars: Map<string, Type * DeclElt>
    fns: Map<(string * int), (FunctionType * Stmt) list> // This doesn't support type-based disambiguation of user-defined function overloading
    isInWritePosition: bool // used for findWrites only
    blockLevel: BlockLevel
    options: Options.Options
} with
    member this.withFunction(fct: FunctionType, body, replaceMostRecentOverload) =
        let oldFnsList = (this.fns.TryFind(fct.prototype) |> Option.defaultValue [])
        let newFnsList = (fct, body) :: (if replaceMostRecentOverload then oldFnsList.Tail else oldFnsList)
        {this with fns = this.fns.Add(fct.prototype, newFnsList)}

let mapEnv options fe fi = {
    fExpr = fe
    fStmt = fi
    vars = Map.empty
    fns = Map.empty
    isInWritePosition = false
    blockLevel = BlockLevel.Unknown
    options = options
}
let mapEnvExpr options fe = mapEnv options fe (fun _ -> id)

let foldList env fct li =
    let mutable env = env
    let res = li |> List.map (fun i ->
        let newEnv, x = fct env i
        env <- newEnv
        x)
    env, res

// Applies env.fExpr recursively on all nodes of an expression.
let rec mapExpr env = function
    | FunCall(Op o as fct, first::args) when Builtin.assignOps.Contains o ->
        let first = mapExpr {env with isInWritePosition = true} first
        env.fExpr env (FunCall(mapExpr env fct, first :: List.map (mapExpr env) args))
    | FunCall(fct, args) ->
        let env = {env with isInWritePosition = false}
        env.fExpr env (FunCall(mapExpr env fct, List.map (mapExpr env) args))
    | Subscript(arr, ind) ->
        let indexEnv = {env with isInWritePosition = false}
        env.fExpr env (Subscript(mapExpr env arr, Option.map (mapExpr indexEnv) ind))
    | Dot(e,  field) -> env.fExpr env (Dot(mapExpr env e, field))
    | Cast(id, e) -> env.fExpr env (Cast(id, mapExpr env e))
    | VectorExp(li) ->
        env.fExpr env (VectorExp(List.map (mapExpr env) li))
    | e -> env.fExpr env e

and mapDecl env (ty, vars) =
    let aux env (decl: DeclElt) =
        // First visit the initialization value, then add the decl to the env
        // e.g. in `float x = x + 1`, the two `x` are not the same!
        let ret = {
            decl with
                size=Option.map (mapExpr env) decl.size
                init=Option.map (mapExpr env) decl.init}
        let env = {env with vars = env.vars.Add(decl.name.Name, (ty, decl))}
        env, ret
    let env, vars = foldList env aux vars
    env, (ty, vars)

let rec mapStmt blockLevel env stmt =
    let mapStmt' = mapStmt BlockLevel.Nested
    let env = {env with blockLevel = BlockLevel.Nested}
    let aux = function
        | Block stmts ->
            let _, stmts = foldList env mapStmt' stmts
            env, Block stmts
        | Expr e -> env, Expr (mapExpr env e)
        | Decl d ->
            let env, res = mapDecl env d
            env, Decl res
        | If(cond, th, el) ->
            env, If (mapExpr env cond, snd (mapStmt' env th), Option.map (mapStmt' env >> snd) el)
        | While(cond, body) ->
            env, While (mapExpr env cond, snd (mapStmt' env body))
        | DoWhile(cond, body) ->
            env, DoWhile (mapExpr env cond, snd (mapStmt' env body))
        | ForD(init, cond, inc, body) ->
            let env', decl = mapDecl env init
            let res = ForD (decl, Option.map (mapExpr env') cond,
                            Option.map (mapExpr env') inc, snd (mapStmt' env' body))
            if env.options.hlsl then env', res
            else env, res
        | ForE(init, cond, inc, body) ->
            let res = ForE (Option.map (mapExpr env) init, Option.map (mapExpr env) cond,
                            Option.map (mapExpr env) inc, snd (mapStmt' env body))
            env, res
        | Jump(k, e) ->
            env, Jump (k, Option.map (mapExpr env) e)
        | (Verbatim _ | Directive _) as v -> env, v
        | Switch(e, cl) ->
            let mapLabel = function
                | Case e -> Case (mapExpr env e)
                | Default -> Default
            let mapCase (l, sl) =
                let _, sl = foldList env mapStmt' sl
                (mapLabel l, sl)
            env, Switch (mapExpr env e, List.map mapCase cl)
    let env, res = aux stmt
    let env = {env with blockLevel = blockLevel}
    env, env.fStmt env res

let mapTopLevel env li =
    let _, res = li |> foldList env (fun env tl ->
        match tl with
        | TLDecl t ->
            let env, res = mapDecl env t
            env, TLDecl res
        | Function(fct, body) ->
            // Back up the vars without the parameters.
            let varsWithoutParameters = env.vars
            // Add the function to env.fns, to have it when transforming the parameters.
            let env = env.withFunction(fct, body, replaceMostRecentOverload = false)
            // Transform the parameters and add them to env.vars, to have them when transforming the body.
            let env, args = foldList env mapDecl fct.args
            // Update env.fns with the transformed parameters.
            let fct = { fct with args = args }
            let env = env.withFunction(fct, body, replaceMostRecentOverload = true)

            // Transform the body. The env modifications (local variables) are discarded.
            let _, body = mapStmt (BlockLevel.FunctionRoot fct) env body
            // Update env.fns with the transformed body.
            let env = env.withFunction(fct, body, replaceMostRecentOverload = true)

            // Remove the parameters from env.vars, so that following functions don't see them.
            let env = {env with vars = varsWithoutParameters}
            env, Function(fct, body)
        | e -> env, e)
    res

let jumpKeywordToString = function
    | JumpKeyword.Break -> "break"
    | JumpKeyword.Continue -> "continue"
    | JumpKeyword.Discard -> "discard"
    | JumpKeyword.Return -> "return"
let stringToJumpKeyword = function
    | "break" -> JumpKeyword.Break
    | "continue" -> JumpKeyword.Continue
    | "discard" -> JumpKeyword.Discard
    | "return" -> JumpKeyword.Return
    | s -> failwith ("not a keyword: " + s)

let (|ResolvedVariableUse|_|) = function
    | Var v ->
        match v.Declaration with
        | Declaration.Variable vd -> Some (v, vd)
        | _ -> None
    | _ -> None
