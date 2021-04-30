module tests

open System
open System.IO
open System.Diagnostics

let binaryFilename = "shader_minifier.exe"
let outFilename = Path.Combine(Path.GetTempPath(), "shader_code.h")
let testFiles = Directory.GetFiles("tests\\", "*.*", SearchOption.AllDirectories)
let quoteArgs = fun s -> "\"" + s + "\""

let () =
    if not(File.Exists(binaryFilename)) then
        printfn "You need to compile %s first!" binaryFilename
        exit 1
    let stopwatch = Stopwatch.StartNew()
    printfn "Running %i tests..." testFiles.Length
    let mutable failedTestCount = 0
    for filename in testFiles do
        printfn "%s" (binaryFilename + String.concat " " (List.map quoteArgs [filename; "-o"; outFilename]))
        let p = Process.Start(binaryFilename,
                              String.concat " " (List.map quoteArgs [filename; "-o"; outFilename]))
        p.WaitForExit()
        if p.ExitCode <> 0 then
            printfn "*** error while testing %s: program returned %i" filename p.ExitCode
            failedTestCount <- failedTestCount + 1
            printfn "\n"
    printfn "%i tests run in %i seconds, %i failed." testFiles.Length (int(stopwatch.Elapsed.TotalSeconds)) failedTestCount
    ()
