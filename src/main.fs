module Main

[<EntryPoint>]
let main argv =
    let err =
        try
            let options, files = Options.initFiles argv
            if options.verbose then
                printfn "Shader Minifier %s - https://github.com/laurentlb/Shader_Minifier" Options.version
            ShaderMinifier.run options files
        with
        | :? Argu.ArguParseException as ex ->
            printfn "%s" ex.Message
            1
        | Failure msg ->
            printfn "%s" msg
            1
    if Options.debugMode then System.Console.ReadLine() |> ignore
    exit err
