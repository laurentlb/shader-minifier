open OpenTK.Graphics.OpenGL
open Options.Globals
open System
open System.Diagnostics
open System.IO

open Argu
open System.Text.RegularExpressions

type CliArguments =
    | Update_Golden
    | Skip_Glslang_Compile
    | GLSL_Driver_Compile
    | Skip_Performance_Tests

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Update_Golden -> "Update the golden tests"
            | Skip_Glslang_Compile -> "Skip testing compilation of shaders with glslang"
            | GLSL_Driver_Compile -> "Test GLSL compilation of shaders agaist current OpenGL driver"
            | Skip_Performance_Tests -> "Skip the tests of performance"

let cliArgs = ArgumentParser.Create<CliArguments>().ParseCommandLine()
//let cliArgs = ArgumentParser.Create<CliArguments>().ParseCommandLine([|"--update-golden"|])

let initOpenTK () =
    // OpenTK requires a GameWindow
    if cliArgs.Contains(GLSL_Driver_Compile) then
        new OpenTK.GameWindow() |> ignore

// Return true if the file can be compiled as a GLSL shader by the current OpenGL driver
let canBeCompiledByDriver stage (content: string) =
    if not (cliArgs.Contains(GLSL_Driver_Compile)) then
        true
    else
    let fragmentShader = GL.CreateShader(
        match stage with
        | "vert" -> ShaderType.VertexShader
        | "geom" -> ShaderType.GeometryShader
        | "frag" -> ShaderType.FragmentShader
        | "comp" -> ShaderType.ComputeShader
        | _ -> ShaderType.FragmentShader
        )
    GL.ShaderSource(fragmentShader, content)
    GL.CompileShader(fragmentShader)
    let driverinfo = sprintf "%s / %s / %s / %s" (GL.GetString(StringName.Vendor)) (GL.GetString(StringName.Renderer)) (GL.GetString(StringName.Version)) (GL.GetString(StringName.ShadingLanguageVersion))
    let mutable status = 0
    GL.GetShader(fragmentShader, ShaderParameter.CompileStatus, &status)
    let info = GL.GetShaderInfoLog(fragmentShader)
    let info = if info.EndsWith("\n") then info else info + "\n"
    GL.DeleteShader(fragmentShader)
    if status = 1 then
        // getting warnings is not reliable: the shader may be cached but the warnings are not
        //if info.Length > 1 then
        //    printf "%s\nDriver compilation warnings: %s" driverinfo info
        true
    else
        printf "%s\nDriver compilation failed:\n%s" driverinfo info
        false

let canBeCompiledByGlslang stage (content: string) =
    if cliArgs.Contains(Skip_Glslang_Compile) then
        true
    else
    let mutable tgtopt = ""
    let mutable versopt = "-d"
    // glslang bug workarounds:
    // after 330, spir-v targeting must be enabled or else not all OpenGL GLSL features are supported
    // for 1xx languages, spir-v targeting must be disabled
    if Regex.Match(content, @"^\s*#version\s+[34]", RegexOptions.Multiline).Success then
        tgtopt <- "--target-env opengl --amb --aml -o Checker/.out.spv"
    // 300es is not supported, override it to 310es when detected
    if Regex.Match(content, @"^\s*#version\s+300\s+es", RegexOptions.Multiline).Success then
        versopt <- "--glsl-version 310es"
    let si = ProcessStartInfo()
    si.FileName <-
        if Environment.OSVersion.Platform = PlatformID.Win32NT then
            "Checker/glslang.exe"
        else
            // must be installed externally
            "glslang"
    si.Arguments <- sprintf "%s %s --stdin -S %s" versopt tgtopt stage
    si.UseShellExecute <- false
    si.RedirectStandardInput <- true
    si.RedirectStandardOutput <- true
    si.RedirectStandardError <- true
    let proc = Process.Start(si)
    proc.StandardInput.Write(content)
    proc.StandardInput.Close()
    let info = (proc.StandardOutput.ReadToEnd()) + (proc.StandardError.ReadToEnd())
    proc.WaitForExit()
    if proc.ExitCode.Equals(0) then
        true
    else
        printf "glslang compilation failed:\n%s\n" info
        false

let canBeCompiled stage content =
    let mutable fullsrc = content
    // If there is no "void main", add one (at the end, to not disturb any #version line)
    if not (Regex.Match(" " + fullsrc, @"(?s)[^\w]void\s+main\s*(\s*)").Success) then
        fullsrc <- fullsrc + "\n#line 1 2\nvoid main(){}\n"
    canBeCompiledByGlslang stage fullsrc && canBeCompiledByDriver stage fullsrc

let doMinify file content =
    let arr = ShaderMinifier.minify [|file, content|] |> fst |> Array.map (fun s -> s.code)
    Printer.print arr.[0]

let testMinifyAndCompile (file: string) =
    try
        let content = File.ReadAllText file
        let stage = Regex.Match(file, @"[^\.]+$").Value
        let compile = not (Regex.Match(content, @"^//NOCOMPILE").Success)
        if compile && not (canBeCompiled stage content) then
            printfn "Invalid input file '%s'" file
            false
        else
            let minified = doMinify file content + "\n"
            if compile && not (canBeCompiled stage minified) then
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
        doMinify "perf test" str |> ignore<string>
    let time = stopwatch.Elapsed
    printfn "%i files minified in %f seconds." files.Length time.TotalSeconds

// Generated files may contain the Shader Minifier version.
// Ignore version changes in the tests.
let versionRegex = new Regex(@"\bShader Minifier \d(\.\d+)+")

let runCommand argv =
    let cleanString (s: string) =
        let s = s.Replace("\r\n", "\n").Trim()
        versionRegex.Replace(s, "")
    Options.init argv
    let expected =
        try File.ReadAllText options.outputName |> cleanString
        with _ when cliArgs.Contains(Update_Golden) -> ""
           | _ -> reraise ()
    let result =
        use out = new StringWriter()
        let shaders, exportedNames = ShaderMinifier.minifyFiles options.filenames
        Formatter.print out shaders exportedNames options.outputFormat
        out.ToString() |> cleanString
    if result = expected then
        printfn "Success: %s" options.outputName
        0
    else
        printfn "Fail: %A" argv
        if cliArgs.Contains(Update_Golden) then
            File.WriteAllText(options.outputName, result + "\n")
        else
            printfn "Got %d: %A" result.Length result
            printfn "Expected %d: %A" expected.Length expected
        1

let testGolden () =
    let commands = File.ReadAllLines "tests/commands.txt" |> Array.choose (fun line ->
        let line = line.Trim()
        if line.Length = 0 || line.[0] = '#' then
            None
        else
            Some (line.Split([|' '|]))
    )
    commands |> Array.sumBy runCommand

[<EntryPoint>]
let main argv =
    // Manually run compression tests by enabling this line:
    // CompressionTests.run ()

    initOpenTK()
    let mutable failures = testGolden()
    Options.init([|"--format"; "text"; "--no-remove-unused"; "fake.frag"|])
    let srcfilter e = Regex.Match(e, @"\.(vert|geom|frag|comp)$").Success
    let unitTests = Directory.GetFiles("tests/unit", "*") |> Array.filter srcfilter
    let realTests = Directory.GetFiles("tests/real", "*.frag")
    for f in unitTests do
        if not (testMinifyAndCompile f) then
            failures <- failures + 1
    testPerformance (Seq.concat [realTests; unitTests] |> Seq.toArray)
    if failures = 0 then
        printfn "All good."
    else
        printfn "%d failures." failures

    //System.Console.ReadLine() |> ignore
    if failures = 0 then 0 else 1
