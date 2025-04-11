module Options

open Argu
open System

let version = "1.5.0" // Shader Minifier version
let debugMode = false

type OutputFormat =
    | [<CustomCommandLine("text")>] Text
    | [<CustomCommandLine("indented")>] IndentedText
    | [<CustomCommandLine("c-variables")>] CVariables
    | [<CustomCommandLine("c-array")>] CArray
    | [<CustomCommandLine("js")>] JS
    | [<CustomCommandLine("nasm")>] Nasm
    | [<CustomCommandLine("rust")>] Rust

type FieldSet =
    | RGBA
    | XYZW
    | STPQ

type CliArguments =
    | [<CustomCommandLine("--version")>] Version
    | [<CustomCommandLine("-o")>] OutputName of string
    | [<CustomCommandLine("-v")>] Verbose
    | [<CustomCommandLine("--debug")>] Debug
    | [<CustomCommandLine("--hlsl")>] Hlsl
    | [<CustomCommandLine("--format")>] FormatArg of OutputFormat
    | [<CustomCommandLine("--field-names")>] FieldNames of FieldSet
    | [<CustomCommandLine("--preserve-externals")>] PreserveExternals
    | [<CustomCommandLine("--preserve-all-globals")>] PreserveAllGlobals
    | [<CustomCommandLine("--no-inlining")>] NoInlining
    | [<CustomCommandLine("--aggressive-inlining")>] AggroInlining
    | [<CustomCommandLine("--no-renaming")>] NoRenaming
    | [<CustomCommandLine("--no-renaming-list")>] NoRenamingList of string
    | [<CustomCommandLine("--no-sequence")>] NoSequence
    | [<CustomCommandLine("--no-remove-unused")>] NoRemoveUnused
    | [<CustomCommandLine("--no-overloading")>] NoOverloading
    | [<CustomCommandLine("--move-declarations")>] MoveDeclarations
    | [<CustomCommandLine("--preprocess")>] Preprocess
    | [<CustomCommandLine("--export-kkp-symbol-maps")>] ExportKkpSymbolMaps
    | [<MainCommand>] Filenames of filename:string list
 
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | OutputName _ -> "Set the output filename (default is shader_code.h)"
            | Verbose -> "Verbose, display additional information"
            | Debug -> "Debug, display more additional information"
            | Hlsl -> "Use HLSL (default is GLSL)"
            | FormatArg _ -> "Choose to format the output (use 'text' if you want just the shader)"
            | FieldNames _ -> "Choose the field names for vectors: 'rgba', 'xyzw', or 'stpq'"
            | PreserveExternals -> "Do not rename external values (e.g. uniform)"
            | PreserveAllGlobals -> "Do not rename functions and global variables"
            | NoInlining -> "Do not automatically inline variables and functions"
            | AggroInlining -> "Aggressively inline constants. This can reduce output size due to better constant folding. It can also increase output size due to repeated inlined constants, but this increased redundancy can be beneficial to compression, leading to a smaller final compressed size anyway. Does nothing if inlining is disabled."
            | NoRenaming -> "Do not rename anything"
            | NoRenamingList _ -> "Comma-separated list of functions to preserve"
            | NoSequence -> "Do not use the comma operator trick"
            | NoOverloading -> "When renaming functions, do not introduce new overloads"
            | NoRemoveUnused -> "Do not remove unused code"
            | MoveDeclarations -> "Move declarations to group them"
            | Preprocess -> "Evaluate some of the file preprocessor directives"
            | ExportKkpSymbolMaps -> "Export kkpView symbol maps"
            | Filenames _ -> "List of files to minify"
            | Version -> "Display the version and exit"

type Options = {
    version: bool
    outputName: string
    outputFormat: OutputFormat
    verbose: bool
    debug: bool
    canonicalFieldNames: string
    preserveExternals: bool
    preserveAllGlobals: bool
    hlsl: bool
    noInlining: bool
    noOverloading: bool
    aggroInlining: bool
    noSequence: bool
    noRenaming: bool
    noRenamingList: string list
    noRemoveUnused: bool
    moveDeclarations: bool
    preprocess: bool
    exportKkpSymbolMaps: bool
}
with
    member this.renameField field =
        if Builtin.isFieldSwizzle field then
            field |> String.map (fun c -> this.canonicalFieldNames[Builtin.swizzleIndex c])
        else field
    member this.trace str = if this.debug then printfn "%s" str


let helpTextMessage = sprintf "Shader Minifier %s - https://github.com/laurentlb/Shader_Minifier" version

let private argParser = lazy (
    ArgumentParser.Create<CliArguments>(
        programName = "Shader Minifier",
        helpTextMessage = helpTextMessage))

let private initPrivate argv =
    let args = argParser.Value.Parse(argv)
    let options = {
        version = args.Contains Version
        outputName = args.GetResult(OutputName, defaultValue = "shader_code.h");
        outputFormat = args.GetResult(FormatArg, defaultValue = CVariables);
        verbose = args.Contains(Verbose)
        debug =
            args.Contains(Debug) ||
            Environment.GetEnvironmentVariable("MINIFIER_DEBUG", EnvironmentVariableTarget.User) = "yes"
        canonicalFieldNames = (sprintf "%A" (args.GetResult(FieldNames, defaultValue = XYZW))).ToLower()
        preserveExternals = args.Contains(PreserveExternals) || args.Contains(PreserveAllGlobals)
        preserveAllGlobals = args.Contains(PreserveAllGlobals)
        hlsl = args.Contains(Hlsl)
        noInlining = args.Contains(NoInlining)
        noOverloading = args.Contains(NoOverloading)
        aggroInlining = args.Contains(AggroInlining) && not (args.Contains(NoInlining))
        noSequence = args.Contains(NoSequence)
        noRenaming = args.Contains(NoRenaming)
        noRemoveUnused = args.Contains(NoRemoveUnused)
        moveDeclarations = args.Contains(MoveDeclarations)
        preprocess = args.Contains(Preprocess)
        exportKkpSymbolMaps = args.Contains(ExportKkpSymbolMaps)
        noRenamingList =
            let opt = args.GetResult(NoRenamingList, defaultValue = "main,mainImage")
            [for i in opt.Split([|','|]) -> i.Trim()]
    }

    let filenames = args.GetResult(Filenames, defaultValue=[]) |> List.toArray
    options, filenames

let flagsHelp = lazy (argParser.Value.PrintUsage(message = helpTextMessage))

let init argv =
    let options, filenames = initPrivate argv
    if filenames.Length > 0 then
        failwithf "Unexpected arguments: %A" (String.concat " " filenames)
    options

let initFiles argv =
    let options, filenames = initPrivate argv
    if options.version then
        failwithf "%s" helpTextMessage
    if filenames.Length = 0 then
        failwith (argParser.Value.PrintUsage(message = "Missing parameter: the list of shaders to minify"))
    options, filenames
