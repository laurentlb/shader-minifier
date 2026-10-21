module Options

open Argu
open System

let version = "1.5.1" // Shader Minifier version
let debugMode = false

exception ParseError of string

type OutputFormat =
    | [<CustomCommandLine("text")>] Text
    | [<CustomCommandLine("indented")>] IndentedText
    | [<CustomCommandLine("c-variables")>] CVariables
    | [<CustomCommandLine("c-array")>] CArray
    | [<CustomCommandLine("js")>] JS
    | [<CustomCommandLine("nasm")>] Nasm
    | [<CustomCommandLine("rust")>] Rust
    | [<CustomCommandLine("json")>] Json

type FieldSet =
    | RGBA
    | XYZW
    | STPQ

// A position in a source file. Ast.Location is an alias of this type.
[<StructuredFormatDisplay("{line}:{col}")>]
type Location = { line: int; col: int }

// The kind of transformation reported in a trace event.
[<RequireQualifiedAccess>]
type TraceKind =
    | Inline // a variable, function, or expression was inlined
    | Remove // some code was removed
    | Merge // statements or declarations were merged
    | Move // a declaration was moved
    | Pragma // a pragma was applied or ignored
    | Info // other information about the minification process

type TraceEvent = {
    kind: TraceKind
    filename: string
    loc: Location option
    message: string
} with
    override this.ToString() =
        match this.loc with
        | Some loc -> $"{this.filename}:{loc.line}:{loc.col}: {this.message}"
        | None -> $"{this.filename}: {this.message}"

// Collects the trace events of one input file.
type TraceLog(filename: string) =
    let events = ResizeArray<TraceEvent>()
    member _.Filename = filename
    member _.Add(event: TraceEvent) = lock events (fun () -> events.Add(event))
    member _.Events = lock events (fun () -> events.ToArray())

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
    | [<CustomCommandLine("--readable-output")>] ReadableOutput of string
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
            | ReadableOutput _ -> "Also write the code before renaming, in indented format, to this file (same as --no-renaming --format indented)"
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
    readableOutput: string
    traceLog: TraceLog // the Minifier uses a separate log for each file
}
with
    member this.renameField field =
        if Builtin.isFieldSwizzle field then
            field |> String.map (fun c -> this.canonicalFieldNames[Builtin.swizzleIndex c])
        else field
    member private this.AddTrace(kind, loc, message) =
        let event = { kind = kind; filename = this.traceLog.Filename; loc = loc; message = message }
        this.traceLog.Add event
        if this.debug then eprintfn "%O" event
    // Record a trace event about the code at this location.
    member this.trace(kind, loc: Location, message) =
        // Identifiers created by the minifier have no location (line = -1).
        this.AddTrace(kind, (if loc.line < 0 then None else Some loc), message)
    // Record a trace event that is not attached to a location.
    member this.trace(kind, message) = this.AddTrace(kind, None, message)


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
        readableOutput = args.GetResult(ReadableOutput, defaultValue = "")
        traceLog = TraceLog("")
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
        printfn "%s" helpTextMessage
    else
        if filenames.Length = 0 then
            failwith (argParser.Value.PrintUsage(message = "Missing parameter: the list of shaders to minify"))
    options, filenames
