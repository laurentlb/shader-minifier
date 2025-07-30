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

let run (options: Options.Options) filenames =
    use out =
        if Options.debugMode || options.outputName = "" || options.outputName = "-" then stdout
        else new StreamWriter(options.outputName) :> TextWriter
    try
        minifyFiles options filenames out
        0
    with
    | :? IOException as ex ->
        printfn "Error: %s" ex.Message
        1
    | exn ->
        printfn "%s" (exn.ToString())
        1

let splitCommandLine (input: string) : string list =
    // "  a \\\" b  c\"\\d \" c\"\\\d\\\"\"e " -> ["a"; """; "b"; "cd "; "c\d"e"]
    let args = ResizeArray<string>()
    let sb = System.Text.StringBuilder()
    let mutable inQuotes = false
    let mutable escapeNext = false
    for c in input do
        match c with
        | _ when escapeNext ->
            sb.Append(c) |> ignore
            escapeNext <- false
        | '\\' ->
            escapeNext <- true
        | '"' ->
            inQuotes <- not inQuotes
        | ' ' when not inQuotes ->
            if sb.Length > 0 then
                args.Add(sb.ToString())
                sb.Clear() |> ignore
        | _ ->
            sb.Append(c) |> ignore
    if sb.Length > 0 then
        args.Add(sb.ToString())
    args |> List.ofSeq
let x = splitCommandLine "  a \\\" b\"\\c \" z\"\\d\\\"\"e "
in if x <> ["a"; "\""; "bc "; "zd\"e"] then failwithf "unit test failed: %A" x

let runServer () =
    use stdin = new StreamReader(System.Console.OpenStandardInput())
    let mutable exit = false
    while not exit do
        let line = stdin.ReadLine()
        printfn "> %s" line
        match line with
        | null | "#exit" -> exit <- true
        | _ when System.String.IsNullOrWhiteSpace(line) -> ()
        | _ when line.StartsWith "#" -> ()
        | _ ->
            let argv = splitCommandLine line |> Seq.toArray
            printfn "[%s]" (argv |> Array.map (sprintf "\"%s\"") |> String.concat ", ")
            let options, files = Options.initFiles argv
            if files |> Seq.contains "stdin" then
                printfn "Usage of stdin is not supported in server mode."
            else
                run options files |> ignore
                printfn "ok"
    0

[<EntryPoint>]
let main argv =
    let err =
        try
            if argv = [|"--server"|] then
                runServer ()
            else
                let options, files = Options.initFiles argv
                if options.verbose then
                    printfn "Shader Minifier %s - https://github.com/laurentlb/Shader_Minifier" Options.version
                run options files
        with
        | :? Argu.ArguParseException as ex ->
            printfn "%s" ex.Message
            1
        | Failure msg ->
            printfn "%s" msg
            1
    if Options.debugMode then System.Console.ReadLine() |> ignore
    exit err
