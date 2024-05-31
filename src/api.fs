module ShaderMinifier

open System
open System.IO

let getSize (shaders: Ast.Shader[]) =
    shaders |> Array.map (fun s -> Printer.print s.code)
        |> Array.sumBy (fun s -> s.Length)

let private readFile file =
    let stream =
        if file = "" then new StreamReader(Console.OpenStandardInput())
        else new StreamReader(file)
    stream.ReadToEnd()

let minify (options: Options.Options) (files: (string*string)[]) =
    // like printfn when verbose option is set
    let vprintf fmt = fprintf (if options.verbose then stdout else TextWriter.Null) fmt

    let printSize (shaders: Ast.Shader[]) =
        if options.verbose then
            let length = getSize shaders
            printfn "Shader size is: %d" length

    let names = String.Join(",", files |> Array.map (fun (n,c) -> $"'{n}' ({c.Length}b)")) in options.trace $"----- minifying {names}"
    vprintf "Input file size is: %d\n" (files |> Array.sumBy (fun (_, s) -> s.Length))
    let shaders = files |> Array.map (fun (f, c) -> Parse.runParser options f c)
    vprintf "File parsed. "; printSize shaders

    for shader in shaders do
        if shaders.Length > 1 then options.trace $"---- {shader.filename}"
        if shader.reorderFunctions then
            shader.code <- Rewriter.reorderFunctions options shader.code
        shader.code <- Rewriter.simplify options shader.code
    vprintf "Rewrite tricks applied. "; printSize shaders

    if options.noRenaming then
        shaders, []
    else
        let exportedNames = Renamer.rename options shaders
        vprintf "Identifiers renamed. "; printSize shaders
        shaders, exportedNames

let minifyFiles (options: Options.Options) filenames =
    let files = filenames |> Array.map (fun f ->
        let content = readFile f
        let filename = if f = "" then "stdin" else f
        filename, content)
    minify options files

let format = Formatter.print
let formatWithLocations = Formatter.printWithLocations
let print = Printer.print

let run (options: Options.Options) filenames =
    use out =
        if Options.debugMode || options.outputName = "" || options.outputName = "-" then stdout
        else new StreamWriter(options.outputName) :> TextWriter
    try
        let shaders, exportedNames = minifyFiles options filenames
        Formatter.print options out shaders exportedNames options.outputFormat
        0
    with exn ->
        printfn "%s" (exn.ToString())
        1
