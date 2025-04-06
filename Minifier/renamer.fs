﻿module internal Renamer

open System.Collections.Generic
open Ast

// Environment for renamer
// This object is useful to separate the AST walking from the renaming strategy.
// Maybe we could use a single mutable object, instead of creating envs all the time.
// TODO: create a real class.
[<NoComparison; NoEquality>]
type private Env = {
    // Map from an old variable name to the new one.
    varRenames: Map<string, string>
    // Map from a new function name and function signature to the old name.
    funRenames: Map<string, Map<string, string>>
    // List of names that are still available.
    availableNames: string list

    exportedNames: Ast.ExportedName list ref

    // Whether multiple functions can have the same name (but different signature).
    allowOverloading: bool
    // Function that decides a name (and returns the modified Env).
    newName: Env -> Ident -> Env
    // Function called when we enter a function, optionally updates the Env.
    onEnterFunction: Env -> Stmt -> Env
}
with
    static member Create(availableNames, allowOverloading, newName, onEnterFunction) = {
        varRenames = Map.empty
        funRenames = Map.empty
        availableNames = availableNames
        exportedNames = ref []
        allowOverloading = allowOverloading
        newName = newName
        onEnterFunction = onEnterFunction
    }

    member this.Rename(id: Ident, newName) =
        let prevName = id.Name
        let names = this.availableNames |> List.except [newName]
        id.Rename(newName)
        {this with varRenames = this.varRenames.Add(prevName, newName); availableNames = names}

    member this.Update(varRenames, funRenames, availableNames) = 
        {this with varRenames = varRenames; funRenames = funRenames; availableNames = availableNames}


          (* Contextual renaming *)

type private RenamerImpl(options: Options.Options) =

    let computeContextTable text =
        let contextTable = new Dictionary<(char*char), int>()
        for prev, next in Seq.pairwise text do
            contextTable.[(prev, next)] <- match contextTable.TryGetValue((prev, next)) with _, n -> n + 1
        contextTable

    // /!\ This function is a performance bottleneck.
    let chooseIdent (contextTable: Dictionary<(char*char), int>) ident candidates =
        let allChars = [char 32 .. char 127] // printable chars
        let prevs = allChars |> Seq.choose (fun c ->
            match contextTable.TryGetValue((c, ident)) with
            | true, occ -> Some (c, occ)
            | false, _ -> None
        )
        let nexts = allChars |> Seq.choose (fun c ->
            match contextTable.TryGetValue((ident, c)) with
            | true, occ -> Some (c, occ)
            | false, _-> None
        )

        let mutable best = -10000, ""
        // For performance, consider at most 26 candidates.
        for word: string in candidates |> Seq.take 26 do
            let firstLetter = word.[0]
            let lastLetter = word.[word.Length - 1]
            let mutable score = 0

            for c, _ in prevs do
                match contextTable.TryGetValue((c, firstLetter)) with
                | false, _ -> ()
                | true, occ -> score <- score + occ

            for c, _ in nexts do
                match contextTable.TryGetValue((lastLetter, c)) with
                | false, _ -> ()
                | true, occ -> score <- score + occ

            if word.Length > 1 then
                score <- score - 1000 // avoid long names if there are 1-letter names available
                match contextTable.TryGetValue((firstLetter, lastLetter)) with
                | false, _ -> ()
                | true, occ -> score <- score + occ

            if score > fst best then best <- score, word
            // If the score is equal, consistently pick the same string, to get a more deterministic behavior.
            elif score = fst best && word < snd best then best <- score, word

        let best = snd best
        assert (best.Length > 0)
        let firstLetter = best.[0]
        let lastLetter = best.[best.Length - 1]

        // Update the context table. Due to this side-effect, variables in two identical functions
        // may get different names. Compression tests using Crinkler show that (on average) it's
        // still worth updating the tables. Results might differ with kkrunchy, more testing
        // will be useful.
        for c in allChars do
            match contextTable.TryGetValue((c, ident)), contextTable.TryGetValue((c, firstLetter)) with
            | (false, _), _ -> ()
            | (true, n1), (false, _) -> contextTable.[(c, firstLetter)] <- n1
            | (true, n1), (true, n2) -> contextTable.[(c, firstLetter)] <- n1 + n2
            match contextTable.TryGetValue((ident, c)), contextTable.TryGetValue((lastLetter, c)) with
            | (false, _), _ -> ()
            | (true, n1), (false, _) -> contextTable.[(lastLetter, c)] <- n1
            | (true, n1), (true, n2) -> contextTable.[(lastLetter, c)] <- n1 + n2
          
        best


                                    (* ** Renamer ** *)

    let newUniqueId (numberOfUsedIdents: int ref) (env: Env) (id: Ident) =
        numberOfUsedIdents.Value <- numberOfUsedIdents.Value + 1
        let newName = sprintf "%04d" numberOfUsedIdents.Value
        env.Rename(id, newName)

    // A renaming strategy that considers how a variable is used. It optimizes the frequency of
    // adjacent characters, which can make the output more compression-friendly. This is best
    // suited when minifying a single file.
    let optimizeContext contextTable env (id: Ident) =
        let cid = char (1000 + int id.Name)
        let newName = chooseIdent contextTable cid env.availableNames
        env.Rename(id, newName)

    // A renaming strategy that always picks the first available name. This optimizes the
    // frequency of a few variables. It also ensures that two identical functions will use the
    // same names for local variables, which can be very important in some multifile scenarios.
    let optimizeNameFrequency (env: Env) (id: Ident) =
        let newName = env.availableNames.Head
        env.Rename(id, newName)

    // A renaming strategy that's bijective: if (and only if) two variables had the same old name,
    // they will get the same new name.
    // This leads to a slightly longer output (because lots of names are created, most of them 
    // have two chars). However, this preserves similarities from the input code, so that can be
    // compression-friendly.
    let bijectiveRenaming (allNames: string list) =
        let mutable allNames = allNames
        let d = Dictionary<string, string>()
        fun (env: Env) (id: Ident) ->
            match d.TryGetValue(id.OldName) with
            | true, name ->
                env.Rename(id, name)
            | false, _ ->
                let newName = allNames.Head
                allNames <- allNames.Tail
                d.[id.OldName] <- newName
                env.Rename(id, newName)

    // Renaming safe across multiple files (e.g. uniform/in/out variables are
    // renamed in a consistent way) that tries to optimize based on the context
    // and variable reuse.
    let multiFileRenaming contextTable (exportRenames: IDictionary<string, string>) (env: Env) (id: Ident) =
        match exportRenames.TryGetValue(id.Name) with
            | true, newName -> env.Rename(id, newName)
            | false, _ ->
                let cid = char (1000 + int id.Name)
                let newName = chooseIdent contextTable cid env.availableNames
                env.Rename(id, newName)

    let dontRename (env: Env) (id: Ident) =
        env.Rename(id, id.Name)

    let dontRenameList env names =
        let mutable env = env
        for name in names do env <- dontRename env (Ident name)
        env

    let export env prefix (id: Ident) =
        if not id.IsUniqueId then
            env.exportedNames.Value <- {prefix = prefix; name = id.OldName; newName = id.Name} :: env.exportedNames.Value

    let renFunction env (args: Decl list) (id: Ident) =
        let signature = String.concat "," [for ty, _ in args -> ty.name.ToString()]

        // we're looking for a function name, already used before,
        // but not with the same signature, and which is not in options.noRenamingList.
        let isFunctionNameAvailableForThisSignature(x: KeyValuePair<string, Map<string,string>>) =
            not (x.Value.ContainsKey signature || List.contains x.Key options.noRenamingList)

        match env.funRenames |> Seq.tryFind isFunctionNameAvailableForThisSignature with
        | Some res when env.allowOverloading ->
            // overload an existing function name used with a different signature
            let newName = res.Key
            let funRenames = env.funRenames.Add (res.Key, res.Value.Add(signature, id.Name))
            let env = env.Update(env.varRenames.Add(id.Name, newName), funRenames, env.availableNames)
            id.Rename(newName)
            env
        | _ ->
            // find a new function name
            let prevName = id.Name
            let env = env.newName env id
            let funRenames = env.funRenames.Add (id.Name, Map.empty.Add(signature, prevName))
            let env = env.Update(env.varRenames, funRenames, env.availableNames)
            env

    let renFctName env (f: FunctionType) =
        if (f.isExternal(options) && options.preserveExternals) || options.preserveAllGlobals then
            env
        elif List.contains f.fName.Name options.noRenamingList then
            env
        else
            match env.varRenames.TryFind(f.fName.Name) with
            | Some name ->
                f.fName.Rename(name) // bug, may cause conflicts
                env
            | None ->
                let newEnv = renFunction env f.args f.fName
                if f.isExternal(options) then export env ExportPrefix.HlslFunction f.fName
                newEnv

    let renList env fct li =
        let mutable env = env
        for i in li do
            env <- fct env i
        env

    let rec renExpr (env: Env) expr =
        let mapper _ = function
            | Var v ->
                match env.varRenames.TryFind(v.Name) with
                | Some name -> v.Rename(name); Var v
                | None -> Var v
            | e -> e
        options.visitor(mapper).iterExpr expr

    let renDecl level isFieldOfAnInterfaceBlockWithoutInstanceName env (ty:Type, vars) =
        let aux (env: Env) (decl: Ast.DeclElt) =
            Option.iter (renExpr env) decl.init
            Option.iter (renExpr env) decl.size
            let isExternal = (level = Level.TopLevel && (ty.IsExternal || options.hlsl)) ||
                             isFieldOfAnInterfaceBlockWithoutInstanceName

            if (level = Level.TopLevel && options.preserveAllGlobals) ||
                    List.contains decl.name.Name options.noRenamingList then
                dontRename env decl.name
            elif not isExternal then
                env.newName env decl.name
            elif options.preserveExternals then
                dontRename env decl.name
            else
                match env.varRenames.TryFind(decl.name.Name) with
                | Some name ->
                    decl.name.Rename(name)
                    env
                | None ->
                    let env = env.newName env decl.name
                    export env ExportPrefix.Variable decl.name
                    env

        renList env aux vars

    // "Garbage collection": remove names that are not used in the block
    // so that we can reuse them. In other words, this function allows us
    // to shadow global variables in a function.
    let shadowVariables (env: Env) block =
        // Find all the variables known in varRenames that are used in the block.
        // They should be preserved in the renaming environment.
        let stillUsedSet =
            [for ident in Analyzer.Analyzer(options).varUsesInStmt block -> ident.Name]
                |> Seq.choose env.varRenames.TryFind |> set

        let varRenames, reusable = env.varRenames |> Map.partition (fun _ id -> stillUsedSet.Contains id)
        let reusable = [for i in reusable -> i.Value]
                        |> List.filter (fun x -> not (List.contains x options.noRenamingList))
        let allAvailable = reusable @ env.availableNames |> List.distinct
        env.Update(varRenames, env.funRenames, allAvailable)

    let rec renStmt env =
        let renOpt o = Option.iter (renExpr env) o
        function
        | Expr e -> renExpr env e; env
        | Decl d ->
            renDecl Level.InFunc false env d
        | Block b ->
            renList env renStmt b |> ignore<Env>
            env
        | If(cond, th, el) ->
            renStmt (env.onEnterFunction env th) th |> ignore<Env>
            Option.iter (fun el -> renStmt (env.onEnterFunction env el) el |> ignore<Env>) el
            renExpr env cond
            env
        | ForD(init, cond, inc, body) as stmt ->
            let newEnv = env.onEnterFunction env stmt
            let newEnv = renDecl Level.InFunc false newEnv init
            renStmt newEnv body |> ignore<Env>
            Option.iter (renExpr newEnv) cond
            Option.iter (renExpr newEnv) inc
            if options.hlsl then newEnv
            else env
        | ForE(init, cond, inc, body) as stmt ->
            let newEnv = env.onEnterFunction env stmt
            renOpt init
            renOpt cond
            renOpt inc
            renStmt newEnv body |> ignore<Env>
            env
        | While(cond, body) as stmt ->
            let newEnv = env.onEnterFunction env stmt
            renExpr newEnv cond
            renStmt newEnv body |> ignore<Env>
            env
        | DoWhile(cond, body) as stmt ->
            let newEnv = env.onEnterFunction env stmt
            renExpr newEnv cond
            renStmt newEnv body |> ignore<Env>
            env
        | Jump(_, e) -> renOpt e; env
        | Verbatim _ | Directive _ -> env
        | Switch(e, cl) ->
            let renLabel = function
                | Case e -> renExpr env e
                | Default -> ()
            let renCase env (l, sl) =
                renLabel l
                renList env renStmt sl
            renExpr env e
            renList env renCase cl |> ignore<Env>
            env

    // e.g. struct foo { int a; float b; }
    //   or uniform foo { int a; float b; }
    let renTyBlock (env: Env) = function
        | { StructOrInterfaceBlock.prefix = "struct" } -> env
        | block ->
            // interface block without an instance name: the fields are treated as external global variables
            renList env (renDecl Level.TopLevel true) block.fields

    let rec renTopLevelName env = function
        | TLDecl d -> renDecl Level.TopLevel false env d
        | TypeDecl block -> renTyBlock env block
        | Function(fct, _) -> renFctName env fct
        | _ -> env

    let rec renTopLevelBody (env: Env) = function
        | Function(fct, body) ->
            let env = env.onEnterFunction env body
            let env = renList env (renDecl Level.InFunc false) fct.args
            renStmt env body |> ignore<Env>
        | _ -> ()

    // Compute list of variables names, based on frequency
    let computeListOfNames text =
        let charCounts = Seq.countBy id text |> dict
        let count c = match charCounts.TryGetValue(c) with _, res -> res
        let letters = ['a'..'z'] @ ['A'..'Z'] @ ['_']
        [
            // First, use most frequent letters
            yield! letters |> List.sortBy count |> List.rev |> List.map string

            // Then, generate identifiers with 2 letters
            for c1 in letters do
                for c2 in letters do
                    yield c1.ToString() + c2.ToString()
        ]

    let renameAsts shaders env =
        let mutable env = env
        for shader in shaders do
            // Rename top-level and body at the same time (because the body
            // needs the environment matching the top-level).
            env <- renList env renTopLevelName shader.code
            List.iter (renTopLevelBody env) shader.code

        env.exportedNames.Value

    let assignUniqueIds shaders =
        let numberOfUsedIdents = ref 0
        let mutable env = Env.Create([], false, newUniqueId numberOfUsedIdents, fun env _ -> env)
        renameAsts shaders env

    member _.rename shaders =
        let exportedNames = assignUniqueIds shaders

        // TODO: combine from all shaders
        let forbiddenNames = (Seq.head shaders).forbiddenNames

        // Compute the list of variable names to use
        let text = [for shader in shaders -> Printer.print shader.code] |> String.concat "\0"
        let names = computeListOfNames text
                 |> List.filter (fun x -> not <| List.contains x forbiddenNames)
        let allowOverloading = not options.noOverloading
        let mutable env =
            if Array.length shaders > 1 then
                // Env.Create(names, true, bijectiveRenaming names, shadowVariables)
                let exportsRenames = Seq.zip [for export in exportedNames -> export.name] names |> dict
                let contextTable = computeContextTable text
                Env.Create(names, allowOverloading, multiFileRenaming contextTable exportsRenames, shadowVariables)
            else
                let contextTable = computeContextTable text
                Env.Create(names, allowOverloading, optimizeContext contextTable, shadowVariables)
        env <- dontRenameList env options.noRenamingList
        env.exportedNames.Value <- exportedNames
        renameAsts shaders env

let rename options = RenamerImpl(options).rename
