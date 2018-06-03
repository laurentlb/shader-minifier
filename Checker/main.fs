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
    Ast.targetOutput <- Ast.text()
    main.minify("input", content) |> Printer.print

let check (file: string) =
    try
        let content = (new StreamReader(file)).ReadToEnd()
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

let inputs = [
    @"tests\unit\blocks.frag"
    @"tests\unit\hexa.frag"
    @"tests\unit\inline.frag"
    @"tests\unit\keyword_prefix.frag"
    @"tests\unit\commas.frag"
    @"tests\unit\numbers.frag"
    @"tests\unit\array.frag"
]

[<EntryPoint>]
let main argv =
    initOpenTK()
    let mutable failures = 0
    for f in inputs do
        if not (check f) then
            failures <- failures + 1
    if failures = 0 then
        printfn "All good."
        0
    else
        printfn "%d failures." failures
        1