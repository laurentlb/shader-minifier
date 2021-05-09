module Renamer

open System.Collections.Generic
open Ast
open Options.Globals

type RenameMode = Unambiguous | Frequency | Context

let mutable private renameMode = Unambiguous

let doNotOverloadList = options.noRenamingList

      (* Contextual renaming *)

let contextTable = new HashMultiMap<(char*char), int>(HashIdentity.Structural)

// This function is called when all 1-char ident are already used
let makeLetterIdent =
    let chars = [| 'a' .. 'z' |]
    let mutable first = 0
    let mutable second = 0
    fun () ->
        second <- second + 1
        if second >= chars.Length then second <- 0; first <- first + 1
        string(chars.[first]) + string(chars.[second])

let computeContextTable code =
    Printer.printText code |> Seq.pairwise |> Seq.iter (fun (prev, next) ->
        match contextTable.TryFind (prev, next) with
        | Some n -> contextTable.[(prev, next)] <- n + 1
        | None -> contextTable.[(prev, next)] <- 1
    )
    //let chars, n = Seq.maxBy snd [for pair in contextTable -> pair.Key, pair.Value]
    //printfn "max occ: %A -> %d" chars n

// /!\ This function is a performance bottleneck.
let chooseIdent ident candidates =
    let allChars = [char 32 .. char 127] // printable chars
    let prevs = allChars |> Seq.choose (fun c ->
        match contextTable.TryFind (c, ident) with
        | Some occ -> Some (c, occ)
        | None -> None
    )
    let nexts = allChars |> Seq.choose (fun c ->
        match contextTable.TryFind (ident, c) with
        | Some occ -> Some (c, occ)
        | None -> None
    )

    let mutable best = -1000, "a"
    for word in candidates do
        let letter = (word : string).[0] // FIXME: use both first and last letter to compute stats
        let mutable score = 0
        for c, _ in prevs do
            match contextTable.TryFind (c, letter) with
            | None -> ()
            | Some occ2 -> score <- score + occ2 // * occ

        for c, _ in nexts do
            match contextTable.TryFind (letter, c) with
            | None -> ()
            | Some occ2 -> score <- score + occ2 // * occ

        if score > fst best then best <- score, word

          // failwith ("No 1-letter name available. " +
          //           "Try to remove identifiers or reduce scope of variables. " +
          //           "If it is a problem for you, please send a bug report!")
    let bestS =
        if fst best = -1000 then
            makeLetterIdent ()
        else
            snd best

    let bestC = bestS.[0] // FIXME: doesn't work when ident have more than 1-char!

    // update table
    for c in allChars do
      match contextTable.TryFind (c, ident), contextTable.TryFind (c, bestC) with
      | None, _ -> ()
      | Some n1, None -> contextTable.[(c, bestC)] <- n1
      | Some n1, Some n2 -> contextTable.[(c, bestC)] <- n1 + n2
      match contextTable.TryFind (ident, c), contextTable.TryFind (bestC, c) with
      | None, _ -> ()
      | Some n1, None -> contextTable.[(bestC, c)] <- n1
      | Some n1, Some n2 -> contextTable.[(bestC, c)] <- n1 + n2

    bestS


                                (* ** Renamer ** *)

// Environment for renamer
// int means the scope number (fun with n args = n + 1)
type Env = {
    map: Map<Ident, Ident>
    max: int
    fct: Map<Ident, Map<int, Ident>>
    reusable: Ident list
}

let mutable numberOfUsedIdents = 0

let alwaysNewName env id =
    numberOfUsedIdents <- numberOfUsedIdents + 1
    let newName = sprintf "%04d" numberOfUsedIdents
    let env = {env with map = Map.add id newName env.map; max = env.max + 1}
    env, newName

let optimizeFrequency env id =
    match env.reusable with
    | [] -> // create a new variable
        let newName = sprintf "%04d" env.max
        let env = {env with map = Map.add id newName env.map; max = env.max + 1}
        numberOfUsedIdents <- max numberOfUsedIdents env.max
        env, newName
    | e::l -> // reuse a variable name
        {env with map = Map.add id e env.map; reusable = l}, e

// FIXME: handle 2-letter names
let optimizeContext env id =
    let cid = char (1000 + int id)
    let newName = chooseIdent cid env.reusable
    let l = env.reusable |> List.filter (fun x -> x.[0] <> newName.[0])
    {env with map = Map.add id newName env.map; reusable = l}, newName

let newId env id =
    match renameMode with
    | Unambiguous -> alwaysNewName env id
    | Frequency -> optimizeFrequency env id
    | Context -> optimizeContext env id

let renFunction env nbArgs id =
    if List.exists ((=) id) options.noRenamingList then env, id // don't rename "main"
    else
        // we're looking for a function name, already used before,
        // but not with the same number of arg, and which is not in doNotOverloadList.
        let search (x: KeyValuePair<Ident,Map<int,Ident>>) =
            not (x.Value.ContainsKey nbArgs ||
                 List.exists ((=) x.Key) doNotOverloadList)

        match env.fct |> Seq.tryFind search with
        | Some res when renameMode <> Unambiguous ->
            let newName = res.Key
            let fct = env.fct.Add (res.Key, res.Value.Add(nbArgs, id))
            let env = {env with fct = fct; map = env.map.Add(id, newName)}
            env, newName
        | _ ->
            let env, newName = newId env id
            let env = {env with fct = env.fct.Add (newName, Map.empty.Add(nbArgs, id))}
            env, newName

let renFctName env (f: FunctionType) =
    let ext = options.hlsl && f.semantics <> []
    if (ext && options.preserveExternals) || options.preserveAllGlobals then
        env, f
    else
        let newEnv, newName = renFunction env (List.length f.args) f.fName
        if ext then Formatter.export "F" f.fName newName
        newEnv, {f with fName = newName}

let renList env fct li =
    let mutable env = env
    let res = li |> List.map (fun i ->
        let x = fct env i
        env <- fst x
        snd x)
    env, res

let rec renExpr env =
    let mapper _ = function
        | Var v -> Var (defaultArg (Map.tryFind v env.map) v)
        | e -> e
    mapExpr (mapEnv mapper id)

let renDecl isTopLevel env (ty:Type, vars) : Env * Decl =
    let aux env decl =
        let env, newName =
            let ext =
                match ty.typeQ with
                | Some tyQ -> ["in"; "out"; "attribute"; "varying"; "uniform"]
                             |> List.exists (fun s -> tyQ.Contains(s))
                | None -> false
            if isTopLevel && (ext || options.hlsl || options.preserveAllGlobals) then
                if options.preserveExternals then
                    {env with reusable = List.filter ((<>)decl.name) env.reusable}, decl.name
                else
                    let env, newName = newId env decl.name
                    Formatter.export "" decl.name newName // TODO: first argument seems now useless
                    env, newName
            else
                newId env decl.name

        let init = Option.map (renExpr env) decl.init
        let size = Option.map (renExpr env) decl.size
        env, {decl with name=newName; size=size; init=init}
    let env, res = renList env aux vars
    env, (ty, res)

// "Garbage collection": remove names that are not used in the block
// so that we can reuse them.
let garbage (env: Env) block =
    let d = HashSet()
    let collect mEnv = function
        | Var id as e ->
            if not (mEnv.vars.ContainsKey(id)) then d.Add id |> ignore
            e
        | FunCall(Var id, li) as e ->
            match env.fct.TryFind id with
            | Some m -> if not (m.ContainsKey li.Length) then d.Add id |> ignore
            | None -> d.Add id |> ignore
            e
        | e -> e
    mapStmt (mapEnv collect id) block |> ignore
    let set = HashSet(Seq.choose env.map.TryFind d)
    let map, reusable = Map.partition (fun _ id -> set.Contains id) env.map
    let reusable = reusable |> Seq.filter (fun x -> not (List.exists ((=) x.Value) options.noRenamingList))
    let merge = [for i in reusable -> i.Value] @ env.reusable |> Seq.distinct |> Seq.toList // |> List.sort
    {env with map=map; reusable=merge}

let rec renStmt env =
    let renOpt o = Option.map (renExpr env) o
    function
    | Expr e -> env, Expr (renExpr env e)
    | Decl d ->
        let env, res = renDecl false env d
        env, Decl res
    | Block b ->
        let _, res = renList env renStmt b
        env, Block res
    | If(cond, th, el) ->
        let _, th = renStmt env th
        let el = Option.map (fun x -> snd (renStmt env x)) el
        env, If(renExpr env cond, th, el)
    | ForD(init, cond, inc, body) ->
        let newEnv, init = renDecl false env init
        let _, body = renStmt newEnv body
        let cond = Option.map (renExpr newEnv) cond
        let inc = Option.map (renExpr newEnv) inc
        if options.hlsl then newEnv, ForD(init, renOpt cond, renOpt inc, body)
        else env, ForD(init, renOpt cond, renOpt inc, body)
    | ForE(init, cond, inc, body) ->
        let _, body = renStmt env body
        env, ForE(renOpt init, renOpt cond, renOpt inc, body)
    | While(cond, body) ->
        let _, body = renStmt env body
        env, While(renExpr env cond, body)
    | DoWhile(cond, body) ->
        let _, body = renStmt env body
        env, DoWhile(renExpr env cond, body)
    | Jump(k, e) -> env, Jump(k, renOpt e)
    | Verbatim _ as v -> env, v

let mutable private forbiddenNames = [ "if"; "in"; "do" ]
let addForbiddenName(s: string) = forbiddenNames <- s :: forbiddenNames

let rec renTopLevelName env = function
    | TLDecl d ->
        let env, res = renDecl true env d
        env, TLDecl res
    | Function(fct, body) ->
        let env, res = renFctName env fct
        env, Function(res, body)
    | e -> env, e

let rec renTopLevelBody env = function
    | Function(fct, body) ->
        let env = garbage env body
        let env, args = renList env (renDecl false) fct.args
        let _env, body = renStmt env body
        Function({fct with args=args}, body)
    | e -> e

// Remove the values from the env
// so that the functions are not overloaded
let rec doNotOverload env = function
    | [] -> env
    | name::li ->
        let re = env.reusable |> List.filter (fun x -> x <> name)
        let env = {env with map = Map.add name name env.map; reusable = re}
        doNotOverload env li

let rec renameTopLevel li mode (identTable: string[]) =
    renameMode <- mode
    let idents = identTable |> Array.toList
              |> List.filter (fun x -> x.Length = 1)
              |> List.filter (fun x -> not <| List.exists ((=) x) forbiddenNames)
    // Rename top-level values first
    let env = {map = Map.empty ; max = 0 ; fct = Map.empty ; reusable = idents}
    let env = doNotOverload env doNotOverloadList
    let env, li = renList env renTopLevelName li

    // Then, rename local values
    List.map (renTopLevelBody env) li
