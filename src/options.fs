module Options

open System.IO
open Argu

let version = "1.2" // Shader Minifier version
let debugMode = false

type OutputFormat =
    | [<CustomCommandLine("text")>] Text
    | [<CustomCommandLine("indented")>] IndentedText
    | [<CustomCommandLine("c-variables")>] CHeader
    | [<CustomCommandLine("c-array")>] CList
    | [<CustomCommandLine("js")>] JS
    | [<CustomCommandLine("nasm")>] Nasm

let text() = Text

type FieldSet =
    | RGBA
    | XYZW
    | STPQ

type CliArguments =
    | [<CustomCommandLine("-o")>] OutputName of string
    | [<CustomCommandLine("-v")>] Verbose
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
    | [<CustomCommandLine("--smoothstep")>] Smoothstep
    | [<MainCommand>] Filenames of filename:string list
 
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | OutputName _ -> "Set the output filename (default is shader_code.h)"
            | Verbose -> "Verbose, display additional information"
            | Hlsl -> "Use HLSL (default is GLSL)"
            | FormatArg _ -> "Choose to format the output (use 'text' if you want just the shader)"
            | FieldNames _ -> "Choose the field names for vectors: 'rgba', 'xyzw', or 'stpq'"
            | PreserveExternals _ -> "Do not rename external values (e.g. uniform)"
            | PreserveAllGlobals _ -> "Do not rename functions and global variables"
            | NoInlining -> "Do not automatically inline variables"
            | AggroInlining -> "Aggressively inline constants. This can reduce output size due to better constant folding. It can also increase output size due to repeated inlined constants, but this increased redundancy can be beneficial to gzip leading to a smaller final compressed size anyway. Does nothing if inlining is disabled."
            | NoRenaming -> "Do not rename anything"
            | NoRenamingList _ -> "Comma-separated list of functions to preserve"
            | NoSequence -> "Do not use the comma operator trick"
            | Smoothstep -> "Use IQ's smoothstep trick"
            | Filenames _ -> "List of files to minify" 

module Globals =
    let mutable options = {|
        outputName = "shader_code.h"
        outputFormat = CHeader
        verbose = false
        smoothstepTrick = false
        canonicalFieldNames = "xyzw"
        preserveExternals = false
        preserveAllGlobals = false
        reorderDeclarations = ref false
        reorderFunctions = ref false
        hlsl = false
        noInlining = false
        aggroInlining = false
        noSequence = false
        noRenaming = false
        noRenamingList = ["main"]
        filenames = ([||]: string[])
    |}

    // like printfn when verbose option is set
    let vprintf fmt = fprintf (if options.verbose then stdout else TextWriter.Null) fmt

let init(argv) =
    let argParser = ArgumentParser.Create<CliArguments>(
        helpTextMessage =
            sprintf "Shader Minifier %s - https://github.com/laurentlb/Shader_Minifier" version)

    let args = argParser.Parse(argv)

    let opt = args.GetResult(NoRenamingList, defaultValue = "main")
    let noRenamingList = [for i in opt.Split([|','|]) -> i.Trim()]
    let filenames = args.GetResult(Filenames, defaultValue=[]) |> List.toArray

    if filenames.Length = 0 then
        printfn "%s" (argParser.PrintUsage(message = "Missing parameter: the list of shaders to minify"))
        false
    else
        Globals.options <- {|
            outputName = args.GetResult(OutputName, defaultValue = "shader_code.h")
            outputFormat = args.GetResult(FormatArg, defaultValue = CHeader)
            verbose = args.Contains(Verbose)
            smoothstepTrick = args.Contains(Smoothstep)
            canonicalFieldNames = (sprintf "%A" (args.GetResult(FieldNames, defaultValue = XYZW))).ToLower()
            preserveExternals = args.Contains(PreserveExternals) || args.Contains(PreserveAllGlobals)
            preserveAllGlobals = args.Contains(PreserveAllGlobals)
            reorderDeclarations = ref false
            reorderFunctions = ref false
            hlsl = args.Contains(Hlsl)
            noInlining = args.Contains(NoInlining)
            aggroInlining = args.Contains(AggroInlining) && not (args.Contains(NoInlining))
            noSequence = args.Contains(NoSequence)
            noRenaming = args.Contains(NoRenaming)
            noRenamingList = noRenamingList
            filenames = filenames
        |}
        true
