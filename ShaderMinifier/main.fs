module Main

open ShaderMinifier
open System.IO

let readFile file =
    use stream =
        if file = "" then new StreamReader(System.Console.OpenStandardInput())
        else new StreamReader(file)
    stream.ReadToEnd()

let minifyFiles (options: Options.Options) filenames out =
    let files = [|
        for f in filenames do
            let content = readFile f
            let filename = if f = "" then "stdin" else f
            yield filename, content |]
    let minifier = Minifier(options, files)
    minifier.Format(out)

let rec printError: exn -> unit = function
    | :? System.AggregateException as aggregate ->
        // as minifier can run in parallel, we may have multiple exceptions
        for innerException in aggregate.InnerExceptions do
            printError innerException
    | :? IOException as ex ->
        fprintfn stderr "Error: %s" ex.Message
    | :? Argu.ArguParseException as ex ->
        fprintfn stderr "%s" ex.Message
    | Failure s ->
        fprintfn stderr "%s" s
    | exn ->
        // unexpected exception, print the stack trace for debugging
        fprintfn stderr "%s" (exn.ToString())

let run (options: Options.Options) filenames =
    use out =
        if Options.debugMode || options.outputName = "" || options.outputName = "-" then stdout
        else new StreamWriter(options.outputName) :> TextWriter
    try
        minifyFiles options filenames out
        0
    with
        e -> printError e; 1

[<EntryPoint>]
let main argv =
    let err =
        try
            let options, files = Options.initFiles argv
            if options.verbose then
                printfn "Shader Minifier %s - https://github.com/laurentlb/Shader_Minifier" Options.version
            if options.version then
                0
            else
                run options files
        with
            e -> printError e; 1
    if Options.debugMode then System.Console.ReadLine() |> ignore
    exit err
