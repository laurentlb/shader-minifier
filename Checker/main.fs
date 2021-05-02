open OpenTK.Graphics.OpenGL
open System
open System.IO

let initOpenTK () =
    // OpenTK requires a GameWindow
    let _ = new OpenTK.GameWindow()
    ()

let testCompile content =
    let fragmentShader = GL.CreateShader(ShaderType.FragmentShader)
    GL.ShaderSource(fragmentShader, content)
    GL.CompileShader(fragmentShader)
    let info = GL.GetShaderInfoLog(fragmentShader)
    GL.DeleteShader(fragmentShader)
    if info = "" then
        true
    else
        printfn "compilation failed: %s" info
        false

let doMinify content =
    Options.Globals.options.targetOutput <- Options.text()
    Printer.print(Main.minify("input", content))

let check (file: string) =
    try
        let content =
            use file = new StreamReader(file)
            file.ReadToEnd()
        if not (testCompile content) then
            printfn "Invalid input file '%s'" file
            false
        else
            let minified = doMinify content + "\n"
            if not (testCompile minified) then
                printfn "Minification broke the file '%s'" file
                printfn "%s" minified
                false
            else
                printfn "Success: %s" file
                true
    with :? IO.FileNotFoundException as e ->
        printfn "%A" e
        false

[<EntryPoint>]
let main argv =
    initOpenTK()
    let mutable failures = 0
    let inputs = Directory.GetFiles("tests/unit", "*.frag")
    for f in inputs do
        if not (check f) then
            failures <- failures + 1
    if failures = 0 then
        printfn "All good."
        // System.Console.ReadLine() |> ignore
        0
    else
        printfn "%d failures." failures
        // System.Console.ReadLine() |> ignore
        1
