namespace ShaderMinifier

open System.IO

[<AutoOpen>]
module MinifierPatterns =
    let (|ParseError|_|) (ex: exn) =
        match ex with
        | :? Options.ParseError as e -> Some e.Data0
        | _ -> None

type Minifier(options, files) =
    let getSize (shaders: Ast.Shader[]) =
        shaders |> Seq.sumBy (fun s -> Printer.print s.code |> String.length)

    let minify (options: Options.Options) (files: (string*string)[]) =
        // like printfn when verbose option is set
        let vprintf fmt = fprintf (if options.verbose then stdout else TextWriter.Null) fmt

        let printSize (shaders: Ast.Shader[]) =
            if options.verbose then
                let length = getSize shaders
                vprintf "Shader size is: %d\n" length

        let names = String.concat "," [for n, c in files -> $"'{n}' ({c.Length}b)"]
        if options.debug then eprintfn "----- minifying %s" names
        vprintf "Input file size is: %d\n" (files |> Array.sumBy (fun (_, s) -> s.Length))

        let parseAndRewrite (filename, content) =
            // Each file gets its own trace log, as files are processed in parallel.
            let options = { options with traceLog = Options.TraceLog(filename) }
            let shader = Parse.runParser options filename content
            let code =
                if shader.reorderFunctions then
                    Rewriter.reorderFunctions options shader.code
                else shader.code
            { shader with code = Rewriter.simplify options code }, options.traceLog.Events

        let shaders, traceEvents = Array.Parallel.map parseAndRewrite files |> Array.unzip
        let traceEvents = Array.concat traceEvents
        vprintf "Rewrite tricks applied. "; printSize shaders

        // The renamer modifies the AST in place, so the readable output must be printed now.
        let readable =
            if options.readableOutput = "" then None
            else
                let readableOptions = { options with outputFormat = Options.IndentedText; exportKkpSymbolMaps = false }
                use out = new StringWriter()
                Formatter.print readableOptions out shaders [||]
                Some (out.ToString())

        if options.noRenaming then
            shaders, [||], readable, traceEvents
        else
            let exportedNames = Renamer.rename options (Seq.toArray shaders) |> List.toArray
            vprintf "Identifiers renamed. "; printSize shaders
            shaders, exportedNames, readable, traceEvents

    let shaders, exportedNames, readable, traceEvents = minify options files

    static member ParseOptions(flags) = Options.init flags
    static member ParseOptionsWithFiles(flags) = Options.initFiles flags

    member _.GetSize = getSize shaders
    member _.GetShaders = shaders
    // What the minifier did (inlining, removals...), in the order of the input files.
    member _.TraceEvents: Options.TraceEvent[] = traceEvents

    member _.Format(writer) = Formatter.print options writer shaders exportedNames
    member _.Format(writer, options) =
        Formatter.print options writer shaders exportedNames
    member _.FormatWithLocations(writer) = Formatter.printWithLocations options writer shaders exportedNames
    // The code before renaming, in indented format. Requires the readableOutput option.
    member _.FormatReadable(writer: TextWriter) =
        match readable with
        | Some str -> writer.Write(str)
        | None -> failwith "the readable output is available only with --readable-output"
