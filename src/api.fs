module ShaderMinifier

open System
open System.IO
open Microsoft.FSharp.Text
open Options.Globals

let getSize (shaders: Ast.Shader[]) =
    shaders |> Array.map (fun s -> Printer.printText s.code)
    |> Array.sumBy (fun s -> s.Length)

let private printSize (shaders: Ast.Shader[]) =
    if options.verbose then
        let length = getSize shaders
        printfn "Shader size is: %d" length

let private readFile file =
    let stream =
        if file = "" then new StreamReader(Console.OpenStandardInput())
        else new StreamReader(file)
    stream.ReadToEnd()

let minify (files: (string*string)[]) =
    vprintf "Input file size is: %d\n" (files |> Array.sumBy (fun (_, s) -> s.Length))
    let shaders = files |> Array.map (fun (f, c) -> Parse.runParser f c)
    vprintf "File parsed. "; printSize shaders

    for shader in shaders do
        if shader.reorderFunctions then
            shader.code <- Rewriter.reorderFunctions shader.code
        shader.code <- Rewriter.simplify shader.code
    vprintf "Rewrite tricks applied. "; printSize shaders

    if options.noRenaming then
        shaders, []
    else
        let exportedNames = Renamer.rename shaders
        vprintf "Identifiers renamed. "; printSize shaders
        shaders, exportedNames

let minifyFiles files =
    let files = files |> Array.map (fun f ->
        let content = readFile f
        let filename = if f = "" then "stdin" else f
        filename, content)
    minify files

let run files =
    use out =
        if Options.debugMode || options.outputName = "" || options.outputName = "-" then stdout
        else new StreamWriter(options.outputName) :> TextWriter
    try
        let shaders, exportedNames = minifyFiles files
        Formatter.print out shaders exportedNames options.outputFormat
        0
    with exn ->
        printfn "%s" (exn.ToString())
        1
