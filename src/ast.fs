module Ast

open Options.Globals

type Ident(name: string) =
    let mutable newName = name

    member this.Name = newName
    member this.OldName = name
    member this.Rename(n) = newName <- n
    member val ToBeInlined = newName.StartsWith("i_") with get, set
    member val IsLValue = false with get, set
    member val IsConst = false with get, set

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


[<RequireQualifiedAccess>]
type JumpKeyword = Break | Continue | Discard | Return
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

type Expr =
    | Int of int * string
    | Float of decimal * string
    | Var of Ident
    | Op of string
    | FunCall of Expr * Expr list
    | Subscript of Expr * Expr option
    | Dot of Expr * string
    | Cast of Ident * Expr  // hlsl
    | VectorExp of Expr list // hlsl
    | VerbatimExp of string

and TypeSpec =
    | TypeName of string
    | TypeStruct of string(*type*) * Ident option(*name*) * Decl list

and Type = {
    name: TypeSpec // e.g. int
    typeQ: string list // e.g. const, uniform
    arraySizes: Expr list // e.g. [3][5]
}

and DeclElt = {
    name: Ident // e.g. foo
    size: Expr option // e.g. [3]
    semantics: Expr list // e.g. : color
    init: Expr option // e.g. = f(x)
}

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
    | Switch of Expr * (CaseLabel * Stmt list) list

and CaseLabel =
    | Case of Expr
    | Default

and FunctionType = {
    retType: Type (*return*)
    fName: Ident (*name*)
    args: Decl list (*args*)
    semantics: Expr list (*semantics*)
}

and TopLevel =
    | TLVerbatim of string
    | Function of FunctionType * Stmt
    | TLDecl of Decl
    | TypeDecl of TypeSpec // structs
    | Precision of Type

let makeType name tyQ sizes = {Type.name=name; typeQ=tyQ; arraySizes=sizes}
let makeDecl name size sem init = {name=name; size=size; semantics=sem; init=init}
let makeFunctionType ty name args sem =
    {retType=ty; fName=name; args=args; semantics=sem}

// An ExportedName is a name that is used outside of the shader code (e.g. uniform and attribute
// values). We need to provide accessors for the developer (e.g. create macros for C/C++).
type ExportedName = {
    ty: string  // "F" for hlsl functions, empty for vars
    name: string
    newName: string
}
       
type Shader = {
    filename: string
    mutable code: TopLevel list
    forbiddenNames: string list
    reorderFunctions: bool  // set to true if we saw a forward declaration
}

// mapEnv is a kind of visitor that applies transformations to statements and expressions,
// while also collecting variable declarations along the way.

[<NoComparison; NoEquality>]
type MapEnv = {
    fExpr: MapEnv -> Expr -> Expr
    fStmt: Stmt -> Stmt
    vars: Map<string, Type * DeclElt>
    fns: Map<string, FunctionType * Stmt>
}

let mapEnv fe fi = {fExpr = fe; fStmt = fi; vars = Map.empty; fns = Map.empty}

let foldList env fct li =
    let mutable env = env
    let res = li |> List.map (fun i ->
        let x = fct env i
        env <- fst x
        snd x)
    env, res

// Applies env.fExpr recursively on all nodes of an expression.
let rec mapExpr env = function
    | FunCall(fct, args) ->
        env.fExpr env (FunCall(mapExpr env fct, List.map (mapExpr env) args))
    | Subscript(arr, ind) ->
        env.fExpr env (Subscript(mapExpr env arr, Option.map (mapExpr env) ind))
    | Dot(e,  field) -> env.fExpr env (Dot(mapExpr env e, field))
    | Cast(id, e) -> env.fExpr env (Cast(id, mapExpr env e))
    | VectorExp(li) ->
        env.fExpr env (VectorExp(List.map (mapExpr env) li))
    | e -> env.fExpr env e

and mapDecl env (ty, vars) =
    let aux env (decl: DeclElt) =
        let env = {env with vars = env.vars.Add(decl.name.Name, (ty, decl))}
        env, {decl with
                size=Option.map (mapExpr env) decl.size
                init=Option.map (mapExpr env) decl.init}
    let env, vars = foldList env aux vars
    env, (ty, vars)

let rec mapStmt env i =
    let aux = function
        | Block b ->
            let _, b = foldList env mapStmt b
            env, Block b
        | Expr e -> env, Expr (mapExpr env e)
        | Decl d ->
            let env, res = mapDecl env d
            env, Decl res
        | If(cond, th, el) ->
            env, If (mapExpr env cond, snd (mapStmt env th), Option.map (mapStmt env >> snd) el)
        | While(cond, body) ->
            env, While (mapExpr env cond, snd (mapStmt env body))
        | DoWhile(cond, body) ->
            env, DoWhile (mapExpr env cond, snd (mapStmt env body))
        | ForD(init, cond, inc, body) ->
            let env', decl = mapDecl env init
            let res = ForD (decl, Option.map (mapExpr env') cond,
                            Option.map (mapExpr env') inc, snd (mapStmt env' body))
            if options.hlsl then env', res
            else env, res
        | ForE(init, cond, inc, body) ->
            let res = ForE (Option.map (mapExpr env) init, Option.map (mapExpr env) cond,
                            Option.map (mapExpr env) inc, snd (mapStmt env body))
            env, res
        | Jump(k, e) ->
            env, Jump (k, Option.map (mapExpr env) e)
        | Verbatim _ as v -> env, v
        | Switch(e, cl) ->
            let mapLabel = function
                | Case e -> Case (mapExpr env e)
                | Default -> Default
            let mapCase (l, sl) =
                let _, sl = foldList env mapStmt sl
                (mapLabel l, sl)
            env, Switch (mapExpr env e, List.map mapCase cl)
    let env, res = aux i
    env, env.fStmt res

let mapTopLevel env li =
    let _, res = li |> foldList env (fun env tl ->
        match tl with
        | TLDecl t ->
            let env, res = mapDecl env t
            env, TLDecl res
        | Function(fct, body) ->
            let env = {env with fns = env.fns.Add(fct.fName.Name, (fct, body))}
            env, Function(fct, snd (mapStmt env body))
        | e -> env, e)
    res
