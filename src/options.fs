module Options

open System.IO
open Argu
open System

let version = "1.3.6" // Shader Minifier version
let debugMode = false

type OutputFormat =
    | [<CustomCommandLine("text")>] Text
    | [<CustomCommandLine("indented")>] IndentedText
    | [<CustomCommandLine("c-variables")>] CVariables
    | [<CustomCommandLine("c-array")>] CArray
    | [<CustomCommandLine("js")>] JS
    | [<CustomCommandLine("nasm")>] Nasm
    | [<CustomCommandLine("rust")>] Rust

type GlslVersion =
    | [<CustomCommandLine("460")>] GLSL_460
    | [<CustomCommandLine("100es")>] GLSL_100es
    | [<CustomCommandLine("300es")>] GLSL_300es

type FieldSet =
    | RGBA
    | XYZW
    | STPQ

type CliArguments =
    | [<CustomCommandLine("-o")>] OutputName of string
    | [<CustomCommandLine("-v")>] Verbose
    | [<CustomCommandLine("--debug")>] Debug
    | [<CustomCommandLine("--hlsl")>] Hlsl
    | [<CustomCommandLine("--glsl-version")>] GlslVersionArg of GlslVersion
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
            | GlslVersionArg _ -> "Use GLSL version (default is 460)"
            | FormatArg _ -> "Choose to format the output (use 'text' if you want just the shader)"
            | FieldNames _ -> "Choose the field names for vectors: 'rgba', 'xyzw', or 'stpq'"
            | PreserveExternals -> "Do not rename external values (e.g. uniform)"
            | PreserveAllGlobals -> "Do not rename functions and global variables"
            | NoInlining -> "Do not automatically inline variables and functions"
            | AggroInlining -> "Aggressively inline constants. This can reduce output size due to better constant folding. It can also increase output size due to repeated inlined constants, but this increased redundancy can be beneficial to compression, leading to a smaller final compressed size anyway. Does nothing if inlining is disabled."
            | NoRenaming -> "Do not rename anything"
            | NoRenamingList _ -> "Comma-separated list of functions to preserve"
            | NoSequence -> "Do not use the comma operator trick"
            | NoRemoveUnused -> "Do not remove unused code"
            | MoveDeclarations -> "Move declarations to group them"
            | Preprocess -> "Evaluate some of the file preprocessor directives"
            | ExportKkpSymbolMaps -> "Export kkpView symbol maps"
            | Filenames _ -> "List of files to minify"

type Options() =
    member val outputName = "shader_code.h" with get, set
    member val outputFormat = CVariables with get, set
    member val verbose = false with get, set
    member val debug = false with get, set
    member val canonicalFieldNames = "xyzw" with get, set
    member val preserveExternals = false with get, set
    member val preserveAllGlobals = false with get, set
    member val hlsl = false with get, set
    member val glslver = 460 with get, set
    member val esslver = 0 with get, set
    member val noInlining = false with get, set
    member val aggroInlining = false with get, set
    member val noSequence = false with get, set
    member val noRenaming = false with get, set
    member val noRenamingList = ["main"; "mainImage"] with get, set
    member val noRemoveUnused = false with get, set
    member val moveDeclarations = false with get, set
    member val preprocess = false with get, set
    member val exportKkpSymbolMaps = false with get, set
    member val filenames = [||]: string[] with get, set

module Globals =
    let options = Options()

    // like printfn when verbose option is set
    let vprintf fmt = fprintf (if options.verbose then stdout else TextWriter.Null) fmt

    let forceDebug = Environment.GetEnvironmentVariable("MINIFIER_DEBUG", EnvironmentVariableTarget.User) = "yes"
    let debug str = if options.debug || forceDebug then printfn "%s" str

open Globals

let helpTextMessage = sprintf "Shader Minifier %s - https://github.com/laurentlb/Shader_Minifier" version

let private argParser = lazy (
    ArgumentParser.Create<CliArguments>(
        programName = "Shader Minifier",
        helpTextMessage = helpTextMessage))

let private initPrivate argv needFiles =
    let args = argParser.Value.Parse(argv)

    let opt = args.GetResult(NoRenamingList, defaultValue = "main,mainImage")
    let noRenamingList = [for i in opt.Split([|','|]) -> i.Trim()]
    let filenames = args.GetResult(Filenames, defaultValue=[]) |> List.toArray
    let glslVerStr = (sprintf "%A" (args.GetResult(GlslVersionArg, defaultValue = GLSL_460))).Substring(5)
    let essl = glslVerStr.Contains("es")
    let glslVerNumber = System.Int32.Parse(glslVerStr.Substring(0,3))

    if filenames.Length = 0 && needFiles then
        printfn "%s" (argParser.Value.PrintUsage(message = "Missing parameter: the list of shaders to minify"))
        false
    else
        options.outputName <- args.GetResult(OutputName, defaultValue = "shader_code.h")
        options.outputFormat <- args.GetResult(FormatArg, defaultValue = CVariables)
        options.verbose <- args.Contains(Verbose)
        options.debug <- args.Contains(Debug)
        options.canonicalFieldNames <- (sprintf "%A" (args.GetResult(FieldNames, defaultValue = XYZW))).ToLower()
        options.preserveExternals <- args.Contains(PreserveExternals) || args.Contains(PreserveAllGlobals)
        options.preserveAllGlobals <- args.Contains(PreserveAllGlobals)
        options.hlsl <- args.Contains(Hlsl)
        options.glslver <- (if not essl && not options.hlsl then glslVerNumber else 0)
        options.esslver <- (if essl then glslVerNumber else 0)
        options.noInlining <- args.Contains(NoInlining)
        options.aggroInlining <- args.Contains(AggroInlining) && not (args.Contains(NoInlining))
        options.noSequence <- args.Contains(NoSequence)
        options.noRenaming <- args.Contains(NoRenaming)
        options.noRemoveUnused <- args.Contains(NoRemoveUnused)
        options.moveDeclarations <- args.Contains(MoveDeclarations)
        options.preprocess <- args.Contains(Preprocess)
        options.exportKkpSymbolMaps <- args.Contains(ExportKkpSymbolMaps)
        options.noRenamingList <- noRenamingList
        options.filenames <- filenames
        true

let flagsHelp = lazy (argParser.Value.PrintUsage(message = helpTextMessage))

let init argv = initPrivate argv false |> ignore<bool>
let initFiles argv = initPrivate argv true
