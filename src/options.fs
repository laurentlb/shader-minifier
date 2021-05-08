module Options

open System.Collections.Generic
open System.IO
open Argu

let version = "1.1.6" // Shader Minifier version
let debugMode = false

type TargetOutput =
    | [<CustomCommandLine("text")>] Text
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
    | [<CustomCommandLine("--format")>] FormatArg of TargetOutput
    | [<CustomCommandLine("--field-names")>] FieldNames of FieldSet
    | [<CustomCommandLine("--preserve-externals")>] PreserveExternals
    | [<CustomCommandLine("--preserve-all-globals")>] PreserveAllGlobals
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
            | FormatArg _ -> "Choose to format the output (use none if you want just the shader)"
            | FieldNames _ -> "Choose the field names for vectors: 'rgba', 'xyzw', or 'stpq'"
            | PreserveExternals _ -> "Do not rename external values (e.g. uniform)"
            | PreserveAllGlobals _ -> "Do not rename functions and global variables"
            | NoRenaming -> "Do not rename anything"
            | NoRenamingList _ -> "Comma-separated list of functions to preserve"
            | NoSequence -> "Do not use the comma operator trick"
            | Smoothstep -> "Use IQ's smoothstep trick"
            | Filenames _ -> "List of files to minify" 


let argParser = ArgumentParser.Create<CliArguments>(helpTextMessage =
    sprintf "Shader Minifier %s - https://github.com/laurentlb/Shader_Minifier" version)

type Options() =
    let mutable args: ParseResults<CliArguments> =
        Unchecked.defaultof<ParseResults<CliArguments>>

    member _.outputName =
        args.GetResult(OutputName, defaultValue = "shader_code.h")

    member _.targetOutput =
        args.GetResult(FormatArg, defaultValue = CHeader)

    member _.verbose =
        args.Contains(Verbose)

    member _.smoothstepTrick =
        args.Contains(Smoothstep)

    member _.canonicalFieldNames =
        args.GetResult(FieldNames, defaultValue = XYZW).ToString().ToLower()

    member _.preserveExternals =
        args.Contains(PreserveExternals)
    
    member _.preserveAllGlobals =
        args.Contains(PreserveAllGlobals)
    
    member val reorderDeclarations = false with get, set
    member val reorderFunctions = false with get, set

    member _.hlsl =
        args.Contains(Hlsl)

    member _.noSequence =
        args.Contains(NoSequence)

    member _.noRenaming =
        args.Contains(NoRenaming)

    member _.noRenamingList =
        let opt = args.GetResult(NoRenamingList, defaultValue = "main")
        [for i in opt.Split([|','|]) -> i.Trim()]

    member _.filenames =
        args.GetResult(Filenames, defaultValue=[]) |> List.toArray

    member this.init(argv) =
        args <- argParser.Parse(argv)
        if this.filenames.Length = 0 then
            printfn "%s" (argParser.PrintUsage(message = "Missing parameter: the list of shaders to minify"))
            false
        elif this.filenames.Length > 1 && not this.preserveExternals then
            printfn "When compressing multiple files, you must use the --preserve-externals option."
            false
        else
            true

module Globals =
    let options = Options()

    // like printfn when verbose option is set
    let vprintf fmt = fprintf (if options.verbose then stdout else TextWriter.Null) fmt
