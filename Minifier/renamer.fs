module internal Renamer

open System.Collections.Generic
open Ast
open Analyzer

type Signature = {
    str: string
}
with
    static member Create(args: Decl list) = { str = String.concat "," [for ty, _ in args -> ty.name.ToString()] }

let private renList env fct li =
    let mutable env = env
    for i in li do
        env <- fct env i
    env

// Environment for renamer
// This object is useful to separate the AST walking from the renaming strategy.
// Maybe we could use a single mutable object, instead of creating envs all the time.
// TODO: create a real class.
[<NoComparison; NoEquality>]
type private Env = {
    // Map from an old identifier name to the new one. Contains names of variables, functions and structs.
    identRenames: Map<string, string>
    // Store function signatures for overloading. (newName, Map(Signature, oldName)). Not accessed as a map, but with linear search.
    funOverloads: Map<string, Map<Signature, string>>
    // List of names that are still available. Variables, functions and structs share the same name space.
    availableNames: string list

    exportedNames: ExportedName list ref

    // Whether multiple functions can have the same name (but different signature).
    allowOverloading: bool
    // Function that decides a name (and returns the modified Env).
    newName: Env -> Ident -> Env
    // Function called when we enter a scope, to support name reuse via shadowing
    onEnterScope: Env -> Stmt -> Env
}
with
    static member Create(availableNames, allowOverloading, newName, onEnterScope) = {
        identRenames = Map.empty
        funOverloads = Map.empty
        availableNames = availableNames
        exportedNames = ref []
        allowOverloading = allowOverloading
        newName = newName
        onEnterScope = onEnterScope
    }

    // Renames the identifier and move its new name from availableNames to identRenames.
    member this.AddRenaming(id: Ident, newName) =
        let prevName = id.Name
        let names = this.availableNames |> List.except [newName]
        id.Rename(newName)
        {this with identRenames = this.identRenames.Add(prevName, newName); availableNames = names}

    // Decide the identifier won't be renamed, and mark its name as used.
    member this.DontRename(id: Ident) = this.AddRenaming(id, id.Name)
    member this.DontRenameList(names: string seq) =
        let mutable env = this
        for name in names do env <- env.DontRename(Ident name)
        env

    member this.Update(identRenames, funOverloads, availableNames) =
        {this with identRenames = identRenames; funOverloads = funOverloads; availableNames = availableNames}

// This visitor has three jobs:
//  * for every identifier declaration, give it a name (stored in Env) by calling Env.newName or DontRename
//  * for every identifier use, assign to it the stored name by calling Ident.Rename
//  * handle function names differently: use function overloading in the output, for better compression
// This happens in two steps:
//  * first pass on all top level declarations
//  * second pass on function bodies, with reuse of unused names via shadowing (with onEnterScope)
type private RenamerVisitor(options: Options.Options, level: Level) =

    let export env prefix (id: Ident) =
        if not id.IsUniqueId then
            env.exportedNames.Value <- {prefix = prefix; name = id.OldName; newName = id.Name} :: env.exportedNames.Value

    let rec renExpr (env: Env) expr =
        let mapper _ = function
            | Var v ->
                match env.identRenames.TryFind(v.Name) with
                | Some name -> v.Rename(name); Var v
                | None -> Var v // don't rename things we didn't see a declaration for. (builtins...)
            | Dot (_, field) as e when not (Builtin.isFieldSwizzle field.Name) ->
                match env.identRenames.TryFind(field.Name) with
                | Some name -> field.Rename(name); e
                | None -> e
            | e -> e
        options.visitor(mapper).iterExpr expr

    let renNamedStruct (env: Env) (structName: Ident) =
        // top level struct declaration, e.g. `struct foo { int a; float b; }`
        if options.noRenamingList |> List.contains structName.Name then
            env.DontRename structName
        else
            let env = env.newName env structName
            env

    let rec renDecl isFieldOfAnInterfaceBlockWithoutInstanceName (envForType: Env option) env (ty:Type, vars) =
        let renDeclElt (env: Env) (decl: DeclElt) =
            // Rename expressions in init and size
            Option.iter (renExpr env) decl.init
            Option.iter (renExpr env) decl.size

            // Rename variable type
            let env: Env =
                match envForType with
                | Some envForType -> // This is when renaming the type of function args or decl in ForD.
                    renType envForType ty |> ignore // use the outer env for the type
                    env // then, keep using the inner env
                | None -> renType env ty

            // Rename declared variable
            let isExternal = (level = Level.TopLevel && (ty.IsExternal || options.hlsl)) ||
                             isFieldOfAnInterfaceBlockWithoutInstanceName

            if (level = Level.TopLevel && options.preserveAllGlobals) ||
                    List.contains decl.name.Name options.noRenamingList then
                env.DontRename decl.name
            elif not isExternal then
                env.newName env decl.name
            elif options.preserveExternals then
                env.DontRename decl.name
            else
                match env.identRenames.TryFind(decl.name.Name) with
                | None -> // first time we see this external: pick a new name, export it
                    let env = env.newName env decl.name
                    export env ExportPrefix.Variable decl.name
                    env
                | Some name -> // external already declared in another file: rename consistently
                    decl.name.Rename(name)
                    env

        renList env renDeclElt vars

    and renType (env: Env) (ty: Type) =
        match ty.name with
        | TypeName t ->
            match env.identRenames.TryFind(t.Name) with
            | Some name -> t.Rename(name) // the type name is a reference to a named struct being renamed
            | _ -> () // e.g. builtin type
            env
        | TypeBlock ({ StructOrInterfaceBlock.blockType = Struct } as stru) ->
            // struct + variable declaration (top level or local, named or unnamed)
            // e.g. `struct Foo { int a; } s;` or `struct { int a; } s;`
            let env = match stru.name with
                      | Some structName -> renNamedStruct env structName
                      | _ -> env
            // This isn't actually recursive with renDecl, because "Embedded struct definitions are not allowed".
            renList env (renDecl false None) stru.fields
        | TypeBlock { StructOrInterfaceBlock.blockType = InterfaceBlock _ } ->
            failwith "Unsupported: interface block declaration not at top level"

    let rec renStmt env =
        let renOpt o = Option.iter (renExpr env) o
        function
        | Expr e -> renExpr env e; env
        | Decl d ->
            renDecl false None env d
        | Block b ->
            renList env renStmt b |> ignore<Env>
            env
        | If(cond, th, el) ->
            renStmt (env.onEnterScope env th) th |> ignore<Env>
            Option.iter (fun el -> renStmt (env.onEnterScope env el) el |> ignore<Env>) el
            renExpr env cond
            env
        | ForD(init, cond, inc, body) as stmt ->
            let innerEnv = env.onEnterScope env stmt // In the for scope, we use an env that allows shadowing unused outer decls.
            let envForType = Some env // Use the inner env to rename variables, but use the outer env to rename the variable type!
            let innerEnv = renDecl false envForType innerEnv init
            renStmt innerEnv body |> ignore<Env>
            Option.iter (renExpr innerEnv) cond
            Option.iter (renExpr innerEnv) inc
            if options.hlsl then innerEnv
            else env
        | ForE(init, cond, inc, body) as stmt ->
            let innerEnv = env.onEnterScope env stmt
            renOpt init
            renOpt cond
            renOpt inc
            renStmt innerEnv body |> ignore<Env>
            env
        | While(cond, body) as stmt ->
            let innerEnv = env.onEnterScope env stmt
            renExpr innerEnv cond
            renStmt innerEnv body |> ignore<Env>
            env
        | DoWhile(cond, body) as stmt ->
            let innerEnv = env.onEnterScope env stmt
            renExpr innerEnv cond
            renStmt innerEnv body |> ignore<Env>
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

    let renFunctionWithOverloading env (signature: Signature) (id: Ident) =
        // we're looking for a function name, already used before,
        // but not with the same signature, and which is not in options.noRenamingList.
        let isFunctionNameAvailableForThisSignature(x: KeyValuePair<string, Map<Signature,string>>) =
            not (x.Value.ContainsKey signature || List.contains x.Key options.noRenamingList)
        match env.funOverloads |> Seq.tryFind isFunctionNameAvailableForThisSignature with
        | Some res when env.allowOverloading ->
            // overload an existing function name used with a different signature
            let newName, overloads = res.Key, res.Value
            let funOverloads = env.funOverloads.Add (newName, overloads.Add(signature, id.Name))
            let env = env.Update(env.identRenames.Add(id.Name, newName), funOverloads, env.availableNames)
            id.Rename(newName)
            env
        | _ ->
            // find a new function name
            let prevName = id.Name
            let env = env.newName env id
            let funOverloads = env.funOverloads.Add (id.Name, Map.empty.Add(signature, prevName))
            let env = env.Update(env.identRenames, funOverloads, env.availableNames)
            env

    let renFunction env (f: FunctionType) =
        if (f.isExternal(options) && options.preserveExternals) || options.preserveAllGlobals then
            env
        elif List.contains f.fName.Name options.noRenamingList then
            env
        else
            match env.identRenames.TryFind(f.fName.Name) with
            | Some name ->
                f.fName.Rename(name) // bug, may cause conflicts
                env
            | None ->
                let newEnv = renFunctionWithOverloading env (Signature.Create f.args) f.fName
                if f.isExternal(options) then export env ExportPrefix.HlslFunction f.fName
                newEnv

    let renInterfaceBlockWithoutInstanceName (env: Env) interfaceBlock =
        // interface block without an instance name: the fields are treated as external global variables
        // e.g. `uniform foo { int a; float b; }`
        renList env (renDecl true None) interfaceBlock.fields

    member _.RenTopLevelName env = function
        | TLDecl d -> renDecl false None env d
        | Function(fct, _) -> renFunction env fct
        | TypeDecl ({ StructOrInterfaceBlock.blockType = InterfaceBlock _ } as interfaceBlock) ->
            renInterfaceBlockWithoutInstanceName env interfaceBlock
        | TypeDecl ({ StructOrInterfaceBlock.blockType = Struct; name = Some structName } as namedStruct) ->
            let env = renNamedStruct env structName
            let env = renList env (renDecl false None) namedStruct.fields
            env
        | TypeDecl { StructOrInterfaceBlock.blockType = Struct; name = None } -> // e.g. `struct {int A;};`
            // TIL Declaring an anonymous struct that doesn't declare a variable is legal and does nothing.
            env
        | _ -> env

    member _.RenTopLevelBody env = function
        | Function(fct, body) ->
            // Use the top level env to rename the return type.
            let env = renType env fct.retType
            // In the function body, we use an env that allows shadowing unused outer decls.
            let envForBody = env.onEnterScope env body
            // Use the function body's env to rename arguments, but use the top level env to rename argument types!
            let envForType = Some env
            let envForBody = renList envForBody (renDecl false envForType) fct.args
            // Use the function body's env to rename in the body.
            renStmt envForBody body |> ignore<Env>
        | _ -> ()


(* Contextual renaming *)
type private RenamerImpl(options: Options.Options) =

    let computeContextTable text =
        let contextTable = Dictionary<(char*char), int>()
        for prev, next in Seq.pairwise text do
            contextTable[(prev, next)] <- match contextTable.TryGetValue((prev, next)) with _, n -> n + 1
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
            let firstLetter = word[0]
            let lastLetter = word[word.Length - 1]
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
        let firstLetter = best[0]
        let lastLetter = best[best.Length - 1]

        // Update the context table. Due to this side-effect, variables in two identical functions
        // may get different names. Compression tests using Crinkler show that (on average) it's
        // still worth updating the tables. Results might differ with kkrunchy, more testing
        // will be useful.
        for c in allChars do
            match contextTable.TryGetValue((c, ident)), contextTable.TryGetValue((c, firstLetter)) with
            | (false, _), _ -> ()
            | (true, n1), (false, _) -> contextTable[(c, firstLetter)] <- n1
            | (true, n1), (true, n2) -> contextTable[(c, firstLetter)] <- n1 + n2
            match contextTable.TryGetValue((ident, c)), contextTable.TryGetValue((lastLetter, c)) with
            | (false, _), _ -> ()
            | (true, n1), (false, _) -> contextTable[(lastLetter, c)] <- n1
            | (true, n1), (true, n2) -> contextTable[(lastLetter, c)] <- n1 + n2

        best


                                    (* ** Renamer ** *)

    let newUniqueId (numberOfUsedIdents: int ref) (env: Env) (id: Ident) =
        numberOfUsedIdents.Value <- numberOfUsedIdents.Value + 1
        let newName = sprintf "%04d" numberOfUsedIdents.Value
        env.AddRenaming(id, newName)

    // A renaming strategy that considers how a variable is used. It optimizes the frequency of
    // adjacent characters, which can make the output more compression-friendly. This is best
    // suited when minifying a single file.
    let optimizeContext contextTable env (id: Ident) =
        let cid = char (1000 + int id.Name)
        let newName = chooseIdent contextTable cid env.availableNames
        env.AddRenaming(id, newName)

    // A renaming strategy that always picks the first available name. This optimizes the
    // frequency of a few variables. It also ensures that two identical functions will use the
    // same names for local variables, which can be very important in some multifile scenarios.
    let optimizeNameFrequency (env: Env) (id: Ident) =
        let newName = env.availableNames.Head
        env.AddRenaming(id, newName)

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
                env.AddRenaming(id, name)
            | false, _ ->
                let newName = allNames.Head
                allNames <- allNames.Tail
                d[id.OldName] <- newName
                env.AddRenaming(id, newName)

    // Renaming safe across multiple files (e.g. uniform/in/out variables are
    // renamed in a consistent way) that tries to optimize based on the context
    // and variable reuse.
    let multiFileRenaming contextTable (exportRenames: IDictionary<string, string>) (env: Env) (id: Ident) =
        match exportRenames.TryGetValue(id.Name) with
            | true, newName -> env.AddRenaming(id, newName)
            | false, _ ->
                let cid = char (1000 + int id.Name)
                let newName = chooseIdent contextTable cid env.availableNames
                env.AddRenaming(id, newName)

    // "Garbage collection": remove names that are not used in the block
    // so that we can reuse them. In other words, this function allows us
    // to shadow global variables in a function.
    let shadowVariables (env: Env) block =
        // Find all the variables known in identRenames that are used in the block.
        // They should be preserved in the renaming environment.
        let stillUsedSet =
            [for ident in Analyzer(options).identUsesInStmt (IdentKind.Var ||| IdentKind.Field) block -> ident.Name]
                |> Seq.choose env.identRenames.TryFind |> set

        let identRenames, reusable = env.identRenames |> Map.partition (fun _ id -> stillUsedSet.Contains id)
        let reusable = [for i in reusable -> i.Value]
                        |> List.filter (fun x -> not (List.contains x options.noRenamingList))
        let allAvailable = reusable @ env.availableNames |> List.distinct
        env.Update(identRenames, env.funOverloads, allAvailable)

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
            env <- renList env (RenamerVisitor(options, Level.TopLevel).RenTopLevelName) shader.code
            List.iter (RenamerVisitor(options, Level.InFunc).RenTopLevelBody env) shader.code

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
        env <- env.DontRenameList options.noRenamingList
        env.exportedNames.Value <- exportedNames
        renameAsts shaders env

let rename options = RenamerImpl(options).rename
