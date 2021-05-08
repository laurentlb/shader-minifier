open OpenTK.Graphics.OpenGL
open Options.Globals
open System
open System.Diagnostics
open System.IO

let updateGolden = false

let initOpenTK () =
    // OpenTK requires a GameWindow
    let _ = new OpenTK.GameWindow()
    ()

// Return true if the file can be compiled as a GLSL shader.
let canBeCompiled content =
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
    Printer.printText(Main.minify("input", content))

let testMinifyAndCompile (file: string) =
    try
        let content = File.ReadAllText file
        if not (canBeCompiled content) then
            printfn "Invalid input file '%s'" file
            false
        else
            let minified = doMinify content + "\n"
            if not (canBeCompiled minified) then
                printfn "Minification broke the file '%s'" file
                printfn "%s" minified
                false
            else
                printfn "Success: %s" file
                true
    with :? IO.FileNotFoundException as e ->
        printfn "%A" e
        false

let testPerformance files =
    printfn "Running performance tests..."
    let contents = files |> Array.map File.ReadAllText
    let stopwatch = Stopwatch.StartNew()
    for str in contents do
        doMinify str |> ignore
    let time = stopwatch.Elapsed
    printfn "%i files minified in %f seconds." files.Length time.TotalSeconds

let runCommand argv =
    options.init(argv) |> ignore
    let expected =
        try File.ReadAllText options.outputName
        with _ when updateGolden -> ""
           | _ -> reraise ()
    let result =
        use out = new StringWriter()
        let codes = Array.map Main.minifyFile options.filenames
        CGen.print out (Array.zip options.filenames codes) options.targetOutput
        out.ToString()
    if result = expected then
        printfn "Success: %s" options.outputName
    else
        printfn "Fail: %A" argv
        if updateGolden then
            File.WriteAllText(options.outputName, result)
        else
            printfn "Got: %A" result

let testGolden () =
    let commands = File.ReadAllLines "tests/commands.txt" |> Array.choose (fun line ->
        let line = line.Trim()
        if line.Length = 0 || line.[0] = '#' then 
            None
        else
            Some (line.Split([|' '|]))
    )
    for cmd in commands do
        runCommand cmd

[<EntryPoint>]
let main argv =
    initOpenTK()
    let mutable failures = 0
    testGolden ()
    options.init([|"--format"; "text"; "fake.frag"|]) |> ignore
    let unitTests = Directory.GetFiles("tests/unit", "*.frag")
    let realTests = Directory.GetFiles("tests/real", "*.frag");
    for f in unitTests do
        if not (testMinifyAndCompile f) then
            failures <- failures + 1
    testPerformance (Seq.concat [realTests; unitTests] |> Seq.toArray)
    if failures = 0 then
        printfn "All good."
    else
        printfn "%d failures." failures
    
    System.Console.ReadLine() |> ignore
    if failures = 0 then 0 else 1
