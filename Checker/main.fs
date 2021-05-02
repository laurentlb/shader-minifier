open OpenTK.Graphics.OpenGL
open System
open System.Diagnostics
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
        let content = System.IO.File.ReadAllText file
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

let performanceCheck files =
    printfn "Running performance tests..."
    let contents = files |> Array.map System.IO.File.ReadAllText
    let stopwatch = Stopwatch.StartNew()
    for str in contents do
        doMinify str |> ignore
    let time = stopwatch.Elapsed
    printfn "%i files minified in %f seconds." files.Length time.TotalSeconds

[<EntryPoint>]
let main argv =
    initOpenTK()
    let mutable failures = 0
    let unitTests = Directory.GetFiles("tests/unit", "*.frag")
    let realTests = Directory.GetFiles("tests/real", "*.frag");
    for f in unitTests do
        if not (check f) then
            failures <- failures + 1
    performanceCheck (Seq.concat [realTests; unitTests] |> Seq.toArray)
    if failures = 0 then
        printfn "All good."
    else
        printfn "%d failures." failures
    
    System.Console.ReadLine() |> ignore
    if failures = 0 then 0 else 1
