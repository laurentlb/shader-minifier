namespace ShaderMinifier

open System.IO

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
        options.trace $"----- minifying {names}"
        vprintf "Input file size is: %d\n" (files |> Array.sumBy (fun (_, s) -> s.Length))

        let parseAndRewrite (filename, content) =
            let shader = Parse.runParser options filename content
            let code =
                if shader.reorderFunctions then
                    Rewriter.reorderFunctions options shader.code
                else shader.code
            { shader with code = Rewriter.simplify options code }

        let shaders = Array.Parallel.map parseAndRewrite files
        vprintf "Rewrite tricks applied. "; printSize shaders

        if options.noRenaming then
            shaders, [||]
        else
            let exportedNames = Renamer.rename options (Seq.toArray shaders) |> List.toArray
            vprintf "Identifiers renamed. "; printSize shaders
            shaders, exportedNames

    let shaders, exportedNames = minify options files

    static member ParseOptions(flags) = Options.init flags
    static member ParseOptionsWithFiles(flags) = Options.initFiles flags

    member _.GetSize = getSize shaders
    member _.GetShaders = shaders

    member _.Format(writer) = Formatter.print options writer shaders exportedNames
    member _.Format(writer, options) =
        Formatter.print options writer shaders exportedNames
    member _.FormatWithLocations(writer) = Formatter.printWithLocations options writer shaders exportedNames
