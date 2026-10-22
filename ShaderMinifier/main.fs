module Main

open ShaderMinifier
open System.IO

let readFile file =
    use stream =
        if file = "" || file = "-" then new StreamReader(System.Console.OpenStandardInput())
        else new StreamReader(file)
    stream.ReadToEnd()

let writeOutput (options: Options.Options) (content: string) =
    if Options.debugMode || options.outputName = "" || options.outputName = "-" then
        stdout.Write(content)
    else
        File.WriteAllText(options.outputName, content)

let minifyFiles (options: Options.Options) filenames =
    let files = [|
        for f in filenames do
            let content = readFile f
            let filename = if f = "" || f = "-" then "stdin" else f
            yield filename, content |]
    let minifier = Minifier(options, files)
    // Format everything in memory first, so that a failure doesn't leave a truncated output file.
    use out = new StringWriter()
    minifier.Format(out)
    let readable =
        if options.readableOutput = "" then None
        else
            use readableOut = new StringWriter()
            minifier.FormatReadable(readableOut)
            Some (readableOut.ToString())
    writeOutput options (out.ToString())
    readable |> Option.iter (fun str -> File.WriteAllText(options.readableOutput, str))

let rec printError: exn -> unit = function
    | ParseError str ->
        fprintfn stderr "Parse error: %s" str
    | Options.UsageError str ->
        fprintfn stderr "%s" str
    | :? System.AggregateException as aggregate ->
        // as minifier can run in parallel, we may have multiple exceptions
        for innerException in aggregate.InnerExceptions do
            printError innerException
    | :? IOException as ex ->
        fprintfn stderr "Error: %s" ex.Message
    | :? Argu.ArguParseException as ex ->
        fprintfn stderr "%s" ex.Message
    | exn ->
        // unexpected exception, print the stack trace for debugging
        fprintfn stderr "%s" (exn.ToString())

let run (options: Options.Options) filenames =
    try
        minifyFiles options filenames
        0
    with
        e -> printError e; 1

[<EntryPoint>]
let main argv =
    let err =
        try
            let options, files = Options.initFiles argv
            if options.verbose then
                eprintfn "Shader Minifier %s - https://github.com/laurentlb/Shader_Minifier" Options.version
            if options.version then
                0
            else
                run options files
        with
            | :? Argu.ArguParseException as ex when ex.ErrorCode = Argu.ErrorCode.HelpText ->
                printfn "%s" ex.Message
                0
            | e -> printError e; 1
    if Options.debugMode then System.Console.ReadLine() |> ignore
    exit err
