module CompressionTests

open System.Runtime.InteropServices
open System.IO
open System.Text
open System.Diagnostics
open System

#nowarn "51" // use of native pointers

module Crinkler =
    [<DllImport(@"Compressor.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void InitCompressor()

    [<DllImport(@"Compressor.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern single ApproximateModels4k(char* data, int datasize)


let testFiles = [
    "audio-flight-v2.frag"
    "buoy.frag"
    "controllable-machinery.frag"
    "ed-209.frag"
    "elevated.hlsl"
    "endeavour.frag"
    "from-the-seas-to-the-stars.frag"
    "frozen-wasteland.frag"
    "kinder_painter.frag"
    "leizex.frag"
    "lunaquatic.frag"
    "mandelbulb.frag"
    "ohanami.frag"
    "orchard.frag"
    "oscars_chair.frag"
    "robin.frag"
    "slisesix.frag"
    "terrarium.frag"
    "the_real_party_is_in_your_pocket.frag"
    "valley_ball.glsl"
    "yx_long_way_from_home.frag"
]

type CompressionResult = {
    minifiedSize: int
    compressedSize: float
    timeSpan: TimeSpan
}

let writer = new StringWriter()

let log fmt =
    let logger str =
        printf "%s" str
        writer.Write(str)

    Printf.ksprintf logger fmt

let compressionTest args files =
    Options.init(args)
    let stopwatch = Stopwatch.StartNew()
    let minified, elapsed =
        use out = new StringWriter()
        let shaders, exportedNames = ShaderMinifier.minifyFiles [|for f in files -> "tests/real/" + f|]
        Formatter.print out shaders exportedNames Options.Text
        out.ToString().ToCharArray(), stopwatch.Elapsed

    let pointer = &&minified.[0]
    log "%-40s " (match files with [f] -> f | f::_ -> f + "..." | [] -> "?")
    log "%5d " minified.Length
    let compressedSize = Crinkler.ApproximateModels4k(pointer, minified.Length)
    log "=> %8.3f\n" compressedSize
    printfn "%2.3fs" (float elapsed.TotalMilliseconds / 1000.0)
    { CompressionResult.minifiedSize = minified.Length; compressedSize = float compressedSize; timeSpan = elapsed }

let compressFile (file: string) =
    let langArg = if file.EndsWith("hlsl") then [|"--hlsl"|] else [||]
    let extraArgs = [|"--format"; "text"|]
    compressionTest (Array.append langArg extraArgs) [file]

let run () =
    Crinkler.InitCompressor() // Platform must be set to x64

    writer.GetStringBuilder().Clear() |> ignore<StringBuilder>

    // Tests for minifying multiple files together.
    // We don't have good examples of files that fit together, but these files use the same uniforms (e.g. time, tex0).
    let multifileInputs = ["clod.frag"; "leizex.frag"; "slisesix.frag"; "motion_blur.frag"; "mandel.frag"; "kaleidoscope.frag"]
    let multifileOutput = compressionTest [|"--format"; "text"|] multifileInputs

    // Tests for individual files.
    let sizes = multifileOutput :: List.map compressFile testFiles
    let minifiedSum = sizes |> List.sumBy (fun r -> r.minifiedSize)
    let compressedSum = sizes |> List.sumBy (fun r -> r.compressedSize)
    let elapsedMilliseconds = sizes |> List.sumBy (fun r -> r.timeSpan.TotalMilliseconds)
    log "Total: %5d => %9.3f\n" minifiedSum compressedSum
    printfn "in %2.3fs" (elapsedMilliseconds / 1000.0)

    File.WriteAllText("tests/compression_results.log", writer.ToString())
