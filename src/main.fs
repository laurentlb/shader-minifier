module Main

open Options.Globals

[<EntryPoint>]
let main argv =
    let err =
        try
            if Options.init argv then 
                if options.verbose then
                    printfn "Shader Minifier %s - https://github.com/laurentlb/Shader_Minifier" Options.version
                ShaderMinifier.run options.filenames
            else 1
        with
        | :? Argu.ArguParseException as ex ->
            printfn "%s" ex.Message
            1
    if Options.debugMode then System.Console.ReadLine() |> ignore
    exit err
