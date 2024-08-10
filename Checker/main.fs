open OpenTK.Graphics.OpenGL
open System
open System.Diagnostics
open System.IO

open Argu
open System.Text.RegularExpressions
open ShaderMinifier

type CliArguments =
    | Update_Golden
    | Compile_Golden
    | Skip_Glslang_Compile
    | GLSL_Driver_Compile
    | Skip_Performance_Tests

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Update_Golden -> "Update the golden tests"
            | Compile_Golden -> "Always test compilation of the golden tests"
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

let canBeCompiledByGlslang lang stage (content: string) =
    if cliArgs.Contains(Skip_Glslang_Compile) then
        true
    else
    let mutable tgtopt = ""
    let mutable versopt = "-d"
    if lang = "hlsl" then
        versopt <- "-D --hlsl-dx9-compatible"
        tgtopt <- "--target-env spirv1.6 -V"
    else
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
    if proc.ExitCode = 0 then
        true
    else
        printf "glslang compilation failed:\n%s\n" info
        false

// Note that "100" is ESSL, "#version 100 es" is not valid!
let shaderlangs = set ("110 120 130 140 150 330 400 410 420 430 440 450 460 100 300es hlsl shadertoy".Split ' ')

let canBeCompiled lang stage content =
    if not (shaderlangs.Contains(lang)) then raise (ArgumentException(sprintf "unknown lang %s" lang))
    let mutable lang = lang
    let mutable stage = stage
    let mutable fullsrc = content
    if lang <> "hlsl" then
        let mutable lineadj = 0
        let setline num file =
            // "#line line srcnum" sets line numbering in error messages
            sprintf "\n#line %d %d\n" (num + lineadj) file
        if lang = "shadertoy" then
            let header = File.ReadAllText "tests/shadertoy.h.glsl"
            fullsrc <- header + (setline 1 1) + fullsrc
            lang <- "300es"
            stage <- "frag"
        let mutable verstr = Regex.Replace(lang, @"(\d+)(\w*)", @"$1 $2") // "450" -> "450", "300es" -> "300 es"
        let versmatch = Regex.Match(fullsrc, @"^\s*#version\s+(\d.*)", RegexOptions.Multiline)
        if versmatch.Success then
            verstr <- versmatch.Groups[1].Value
        // #line directive is off-by-one before GLSL 330
        if Regex.Match(verstr, @"1[1-5]").Success then
            lineadj <- -1
        // If there is no "#version" line, add one
        if not versmatch.Success then
            fullsrc <- "#version " + verstr + (setline 1 1) + fullsrc
        // If there is no "void main", add one (at the end, to not disturb any #version line)
        if not (Regex.Match(" " + fullsrc, @"(?s)[^\w]void\s+main\s*(\s*)").Success) then
            fullsrc <- fullsrc + (setline 1 2) + "void main(){}\n"
    canBeCompiledByGlslang lang stage fullsrc && ((lang = "hlsl") || canBeCompiledByDriver stage fullsrc)

let doMinify options file content =
    let minifier = Minifier(options, [|file, content|])
    use tw = new System.IO.StringWriter()
    minifier.Format(tw)
    tw.ToString()

let testMinifyAndCompile options lang (file: string) =
    try
        let content = File.ReadAllText file
        let stage = Regex.Match(file, @"[^\.]+$").Value
        let compile = not (Regex.Match(content, @"^//NOCOMPILE").Success)
        if compile && not (canBeCompiled lang stage content) then
            printfn "Invalid input file '%s'" file
            false
        else
            let minified = doMinify options file content + "\n"
            if compile && not (canBeCompiled lang stage minified) then
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
        let options = Options.init([|"--format"; "text"; "--no-remove-unused"|])
        doMinify options "perf test" str |> ignore<string>
    let time = stopwatch.Elapsed
    printfn "%i files minified in %f seconds." files.Length time.TotalSeconds

// Generated files may contain the Shader Minifier version.
// Ignore version changes in the tests.
let versionRegex = new Regex(@"\bShader Minifier \d(\.\d+)+")

let runCommand argv =
    let cleanString (s: string) =
        let s = s.Replace("\r\n", "\n").Trim()
        versionRegex.Replace(s, "")
    let options, filenames = Minifier.ParseOptionsWithFiles(argv)
    let expected =
        try File.ReadAllText options.outputName |> cleanString
        with _ when cliArgs.Contains(Update_Golden) -> ""
           | _ -> reraise ()
    let files = [|for f in filenames -> f, File.ReadAllText(f)|]
    let minifier = Minifier(options, files)
    let result =
        use out = new StringWriter()
        minifier.Format(out)
        out.ToString() |> cleanString

    let options = { options with outputFormat = Options.OutputFormat.IndentedText; exportKkpSymbolMaps = false}
    if filenames.Length = 1 then
        let shader = minifier.GetShaders[0]
        let resultindented =
            use out = new StringWriter()
            minifier.Format(out, options)
            out.ToString() |> cleanString
        let outdir = "tests/out/" + Regex.Replace(options.outputName, @"^tests/(.*)/[^/]*$", @"$1") + "/"
        let split = Regex.Match(System.IO.Path.GetFileName shader.filename, @"(^.*)\.([^\.]+)$").Groups
        let name = split[1].Value
        let ext = split[2].Value
        Directory.CreateDirectory outdir |> ignore
        File.WriteAllText(outdir + name + ".minind." + ext, resultindented + "\n")

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
    let splitArgs line =
        let mutable args = []
        let mutable insideQuotes = false
        let mutable arg = ""
        for c in line do
            match c with
            | '"' -> insideQuotes <- not insideQuotes
            | ' ' when not insideQuotes ->
                args <- arg :: args
                arg <- ""
            | c -> arg <- $"{arg}{c}"
        if arg <> "" then args <- arg :: args
        args |> List.rev
    let commands = File.ReadAllLines "tests/commands.txt" |> Array.choose (fun line ->
        let line = line.Trim()
        if line.Length = 0 || line.[0] = '#' then
            None
        else
            Some (splitArgs line |> List.toArray)
    )
    commands |> Array.Parallel.map runCommand |> Array.sum

let runCompiler (argv: string array) =
    if (cliArgs.Contains(Skip_Glslang_Compile)) && (not (cliArgs.Contains(GLSL_Driver_Compile))) then
        0
    else
    let lang = argv[0]
    let stage = argv[1]
    let filename = argv[2]
    let content = File.ReadAllText filename
    let info = sprintf "%s (%s %s)" filename lang stage
    if canBeCompiled lang stage content then
        printfn "Compiled: %s" info
        0
    else
        printfn "Compile failure: %s" info
        1

let testCompiled () =
    if not (cliArgs.Contains(Update_Golden) || cliArgs.Contains(Compile_Golden)) then
        0
    else
    let commands = File.ReadAllLines "tests/compile.txt" |> Array.choose (fun line ->
        let line = line.Trim()
        if line.Length = 0 || line.[0] = '#' then
            None
        else
            Some (line.Split([|' '|]))
    )
    commands |> Array.Parallel.map runCompiler |> Array.sum

[<EntryPoint>]
let main argv =
    // Manually run compression tests by enabling this line:
    // CompressionTests.run ()

    initOpenTK()
    let mutable failures = 0
    failures <- failures + testGolden()
    failures <- failures + testCompiled()
    let srcfilter e = Regex.Match(e, @"\.(vert|geom|frag|comp)$").Success
    let unitTests = Directory.GetFiles("tests/unit", "*") |> Array.filter srcfilter
    let realTests = Directory.GetFiles("tests/real", "*.frag")
    for f in unitTests do
        // tests with no #version default to 110
        let options = Options.init([|"--format"; "text"; "--no-remove-unused"|])
        if not (testMinifyAndCompile options "110" f) then
            failures <- failures + 1
    testPerformance (Seq.concat [realTests; unitTests] |> Seq.toArray)
    if failures = 0 then
        printfn "All good."
    else
        printfn "%d failures." failures

    //System.Console.ReadLine() |> ignore
    if failures = 0 then 0 else 1
