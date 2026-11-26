module CompressionTests

open ShaderMinifier
open System.Runtime.InteropServices
open System.IO
open System.Text.RegularExpressions

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
    "moutard.frag"
]

let logFile = "tests/compression_results.log"

type Result = { name: string; minifiedSize: int; compressedSize: float }
with
    override this.ToString() = sprintf "%-40s %5d => %8.3f" this.name this.minifiedSize this.compressedSize

// Compressed sizes from the log file, by name.
let readPreviousSizes () =
    dict [
        for line in File.ReadAllLines logFile do
            let m = Regex.Match(line, @"^(.*?) +\d+ => +([\d.]+)$")
            if m.Success then
                m.Groups[1].Value, System.Double.Parse(m.Groups[2].Value, System.Globalization.CultureInfo.InvariantCulture)
    ]

let compressionTest args filenames =
    let options = Options.init(args)
    let minified =
        use out = new StringWriter()
        let files = [|for f in filenames -> f, File.ReadAllText("tests/real/" + f)|]
        let minifier = Minifier(options, files)
        minifier.Format(out)
        out.ToString().ToCharArray()

    let pointer = &&minified.[0]
    let compressedSize = Crinkler.ApproximateModels4k(pointer, minified.Length)
    { name = match filenames with [f] -> f | f::_ -> f + " (and others)" | [] -> "?"
      minifiedSize = minified.Length
      compressedSize = float compressedSize }

let compressFile (file: string) =
    let langArg = if file.EndsWith("hlsl") then [|"--hlsl"|] else [||]
    let extraArgs = [|"--format"; "text"|]
    compressionTest (Array.append langArg extraArgs) [file]

let run () =
    Crinkler.InitCompressor() // Platform must be set to x64

    let previousSizes = readPreviousSizes ()
    let report result =
        match previousSizes.TryGetValue result.name with
        | true, previous -> printfn "%O (%+.3f)" result (result.compressedSize - previous)
        | false, _ -> printfn "%O (new)" result
        result

    // Tests for minifying multiple files together.
    // We don't have good examples of files that fit together, but these files use the same uniforms (e.g. time, tex0).
    let multifileInputs = [
        ["clod.frag"; "leizex.frag"; "slisesix.frag"; "motion_blur.frag"; "mandel.frag"; "kaleidoscope.frag"]
        ["mouton/mouton.vert"; "mouton/mouton.frag"; "mouton/fxaa.frag"]
        ["vestige/music.frag"; "vestige/post.frag"; "vestige/render.frag"]
    ]
    let results = [
        for files in multifileInputs do
            report (compressionTest [|"--format"; "text"|] files)
        // Tests for individual files.
        for file in testFiles do
            report (compressFile file)
    ]
    let total = report {
        name = "Total:"
        minifiedSize = results |> List.sumBy _.minifiedSize
        compressedSize = results |> List.sumBy _.compressedSize
    }

    File.WriteAllLines(logFile, results @ [total] |> List.map string)
