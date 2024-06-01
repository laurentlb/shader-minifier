module ShaderMinifier

open System.IO

let getSize (shaders: Ast.Shader[]) =
    shaders |> Array.sumBy (fun s -> Printer.print s.code |> String.length)

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
    let shaders = [|for f, c in files -> Parse.runParser options f c|]
    vprintf "File parsed. "; printSize shaders

    let shaders = [|
        for shader in shaders do
            if shaders.Length > 1 then options.trace $"---- {shader.filename}"
            let code =
                if shader.reorderFunctions then
                    Rewriter.reorderFunctions options shader.code
                else shader.code
            yield { shader with code = Rewriter.simplify options code }
    |]
    vprintf "Rewrite tricks applied. "; printSize shaders

    if options.noRenaming then
        shaders, []
    else
        let exportedNames = Renamer.rename options shaders
        vprintf "Identifiers renamed. "; printSize shaders
        shaders, exportedNames

let format = Formatter.print
let formatWithLocations = Formatter.printWithLocations
let print = Printer.print
