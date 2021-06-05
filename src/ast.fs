module Ast

open Options.Globals

type Ident = string

// Real identifiers cannot start with a digit, but the temporary ids of the rename pass are numbers.
let isUniqueId (ident: Ident) = System.Char.IsDigit ident.[0]

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
    | Float of float * string
    | Var of Ident
    | Op of string
    | FunCall of Expr * Expr list
    | Subscript of Expr * Expr option
    | Dot of Expr * Ident
    | Cast of Ident * Expr  // hlsl
    | VectorExp of Expr list // hlsl

and TypeSpec =
    | TypeName of string
    | TypeStruct of string(*type*) * Ident option(*name*) * Decl list

and Type = {
    name: TypeSpec // e.g. int
    typeQ: string option // e.g. const, uniform
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

let makeType name tyQ = {Type.name=name; typeQ=tyQ}
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
}

// mapEnv is a kind of visitor that applies transformations to statements and expressions,
// while also collecting variable declarations along the way.

[<NoComparison; NoEquality>]
type MapEnv = {
    fExpr: MapEnv -> Expr -> Expr
    fStmt: Stmt -> Stmt
    vars: Map<Ident, Type * Expr option * Expr option >
}

let mapEnv fe fi = {fExpr = fe; fStmt = fi; vars = Map.empty}

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
        let env = {env with vars = env.vars.Add(decl.name, (ty, decl.size, decl.init))}
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
    let env, res = aux i
    env, env.fStmt res

let mapTopLevel env li =
    let _, res = li |> foldList env (fun env tl ->
        match tl with
        | TLDecl t ->
            let env, res = mapDecl env t
            env, TLDecl res
        | Function(fct, body) -> env, Function(fct, snd (mapStmt env body))
        | e -> env, e)
    res
