module Ast

open System.Collections.Generic

type targetOutput = Text | CHeader | CList | JS | Nasm

let version = "1.1.4" // Shader Minifer version
let debugMode = false

let mutable outputName = "shader_code.h"
let mutable targetOutput = CHeader
let mutable verbose = false
let mutable smoothstepTrick = false
let mutable fieldNames = "xyzw"
let mutable macroThreshold = 10000
let mutable preserveExternals = false
let mutable preserveAllGlobals = false
let generatedMacros = Dictionary<string, int>()
let mutable reorderDeclarations = false
let mutable reorderFunctions = false
let mutable hlsl = false
let mutable noSequence = false
let mutable noRenaming = false
let mutable noRenamingList = [ "main" ]
let mutable forbiddenNames = [ "if"; "in"; "do" ]
let addForbiddenName s = forbiddenNames <- s :: forbiddenNames

type Ident = string

type Expr =
  | Int of int * string
  | Float of float * string
  | Var of Ident
  | FunCall of Expr * Expr list
  | Subscript of Expr * Expr
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

and Instr =
  | Block of Instr list
  | Decl of Decl
  | Expr of Expr
  | If of Expr * Instr (*then*) * Instr option (*else*)
  | ForD of Decl * Expr option * Expr option * Instr (*for loop starting with a declaration*)
  | ForE of Expr option * Expr option * Expr option * Instr (*for loop starting with an expression*)
  | While of Expr * Instr
  | DoWhile of Expr * Instr
  | Keyword of string * Expr option (*break, continue, return, discard*)
  | Verbatim of string

and FunctionType = {
    retType: Type (*return*)
    fName: Ident (*name*)
    args: Decl list (*args*)
    semantics: Expr list (*semantics*)
  }

and TopLevel =
  | TLVerbatim of string
  | Function of FunctionType * Instr
  | TLDecl of Decl
  | TypeDecl of TypeSpec // structs

let makeType name tyQ = {Type.name=name; typeQ=tyQ}
let makeDecl name size sem init = {name=name; size=size; semantics=sem; init=init}
let makeFunctionType ty name args sem =
  {retType=ty; fName=name; args=args; semantics=sem}

type MapEnv = {
  fExpr: MapEnv -> Expr -> Expr
  fInstr: Instr -> Instr
  vars: Map<Ident, Type * Expr option * Expr option >
}

let mapEnv fe fi = {fExpr = fe; fInstr = fi; vars = Map.empty}

let foldList env fct li =
  let env = ref env
  let res = li |> List.map ((fun i -> // FIXME: use List.fold is cleaner :)
    let x = fct !env i
    env := fst x
    snd x) : 'a -> 'a)
  !env, res

// Applies env.fExpr recursively on all nodes of an expression.
let rec mapExpr env = function
  | FunCall(fct, args) ->
     env.fExpr env (FunCall(mapExpr env fct, List.map (mapExpr env) args))
  | Subscript(arr, ind) ->
     env.fExpr env (Subscript(mapExpr env arr, mapExpr env ind))
  | Dot(e,  field) -> env.fExpr env (Dot(mapExpr env e, field))
  | Cast(id, e) -> env.fExpr env (Cast(id, mapExpr env e))
  | VectorExp(li) ->
     env.fExpr env (VectorExp(List.map (mapExpr env) li))
  | e -> env.fExpr env e

and mapDecl env (ty, vars) =
  let aux env decl =
    let env = {env with vars = env.vars.Add(decl.name, (ty, decl.size, decl.init))}
    env, {decl with size=Option.map (mapExpr env) decl.size; init=Option.map (mapExpr env) decl.init}
  let env, vars = foldList env aux vars
  env, (ty, vars)

let rec mapInstr env i =
  let aux = function
    | Block b ->
        let _, b = foldList env mapInstr b
        env, Block b
    | Expr e -> env, Expr (mapExpr env e)
    | Decl d ->
        let env, res = mapDecl env d
        env, Decl res
    | If(cond, th, el) ->
        env, If (mapExpr env cond, snd (mapInstr env th), Option.map (mapInstr env >> snd) el)
    | While(cond, body) ->
        env, While (mapExpr env cond, snd (mapInstr env body))
    | DoWhile(cond, body) ->
        env, DoWhile (mapExpr env cond, snd (mapInstr env body))
    | ForD(init, cond, inc, body) ->
        let env', decl = mapDecl env init
        let res = ForD (decl, Option.map (mapExpr env') cond,
                        Option.map (mapExpr env') inc, snd (mapInstr env' body))
        if hlsl then env', res
        else env, res
    | ForE(init, cond, inc, body) ->
        let res = ForE (Option.map (mapExpr env) init, Option.map (mapExpr env) cond,
                        Option.map (mapExpr env) inc, snd (mapInstr env body))
        env, res
    | Keyword(k, e) ->
        env, Keyword (k, Option.map (mapExpr env) e)
    | Verbatim _ as v -> env, v
  let env, res = aux i
  env, env.fInstr res

let mapTopLevel env li =
  let env, res = li |> foldList env (fun env tl ->
    match tl with
    | TLDecl t ->
        let env, res = mapDecl env t
        env, TLDecl res
    | Function(fct, body) -> env, Function(fct, snd (mapInstr env body))
    | e -> env, e)
  res


let countIdent code =
  let count = ref 0
  let add li = count := !count + List.length li
  let mapInstr = function
    | Decl (_, li) as e -> add li; e
    | ForD((_, li), _, _, _) as e -> add li; e
    | e -> e
  let mapExpr _ e = e
  let mapTL = function
    | Function(fct, _) -> incr count; add fct.args
    | TLDecl (_, li) -> add li
    | _ -> ()

  mapTopLevel (mapEnv mapExpr mapInstr) code |> ignore
  List.iter mapTL code
  !count
