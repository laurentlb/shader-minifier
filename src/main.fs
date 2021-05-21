module Main

open System
open System.IO
open Microsoft.FSharp.Text
open Options.Globals

let printSize code =
    if options.verbose then
        printfn "Shader size is: %d" (Printer.printText code).Length

let readFile file =
    let stream =
        if file = "" then new StreamReader(Console.OpenStandardInput())
        else new StreamReader(file)
    stream.ReadToEnd()

let minify(filename, content: string) =
    vprintf "Input file size is: %d\n" (content.Length)
    let shader = Parse.runParser filename content
    vprintf "File parsed. "; printSize shader.code

    shader.code <- Rewriter.reorder shader.code

    shader.code <- Rewriter.simplify shader.code
    vprintf "Rewrite tricks applied. "; printSize shader.code

    shader.code <-
        if options.noRenaming then shader.code
        else
            let code = Renamer.rename shader
            vprintf "Identifiers renamed. "; printSize code
            code

    vprintf "Minification of '%s' finished.\n" filename
    shader.code

let minifyFile file =
    let content = readFile file
    let filename = if file = "" then "stdin" else file
    minify(filename, content)

let run files =
    let fail (exn:exn) s =
          printfn "%s" s;
          printfn "%s" exn.StackTrace
          1
    use out =
        if Options.debugMode || options.outputName = "" || options.outputName = "-" then stdout
        else new StreamWriter(options.outputName) :> TextWriter
    try
        let codes = Array.map minifyFile files
        Formatter.print out (Array.zip files codes) options.outputFormat
        0
    with
        | Failure s as exn -> fail exn s
        | exn -> fail exn exn.Message

[<EntryPoint>]
let main argv =
    let err =
        try
            if options.init argv then 
                if options.verbose then
                    printfn "Shader Minifier %s - https://github.com/laurentlb/Shader_Minifier" Options.version
                run options.filenames
            else 1
        with
        | :? Argu.ArguParseException as ex ->
            printfn "%s" ex.Message
            1
    if Options.debugMode then System.Console.ReadLine() |> ignore
    exit err
