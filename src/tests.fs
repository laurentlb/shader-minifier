module tests

open System
open System.IO
open System.Diagnostics

let outFilename = Path.Combine(Path.GetTempPath(), "shader_code.h")
let testFiles = Directory.GetFiles("tests/", "*.*", SearchOption.AllDirectories)

let () =
  let stopwatch = Stopwatch.StartNew()
  printfn "Running %i tests..." (testFiles.Length)
  let failedTestCount = ref 0
  for filename in testFiles do
    let p = Process.Start("./shader_minifier.exe",
                          String.concat " " [ filename; "-o"; outFilename ])
    p.WaitForExit()
    if p.ExitCode <> 0 then
      printfn "*** error while testing %s: program returned %i" filename p.ExitCode
      failedTestCount := !failedTestCount + 1
      printfn "\n"
  printfn "%i tests run in %i seconds, %i failed." testFiles.Length (int(stopwatch.Elapsed.TotalSeconds)) !failedTestCount
  ()
