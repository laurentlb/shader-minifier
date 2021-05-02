module Main

open System
open System.IO
open Microsoft.FSharp.Text
open Options.Globals

// Compute table of variables names, based on frequency
let computeFrequencyIdentTable li =
    let str = Printer.quickPrint li

    let charCounts = Seq.countBy id str |> dict
    let count c = let ok, res = charCounts.TryGetValue(c) in if ok then res else 0
    let letters = ['a'..'z']@['A'..'Z']

    // First, use most frequent letters
    let oneLetterIdentifiers = letters |> List.sortBy count |> List.rev |> List.map string

    // Then, generate identifiers with 2 letters
    let twoLettersIdentifiers =
        [for c1 in letters do
         for c2 in letters do
         yield c1.ToString() + c2.ToString()]
        |> List.sortByDescending (fun s -> count s.[0] + count s.[1])

    Array.ofList (oneLetterIdentifiers @ twoLettersIdentifiers)

let printSize code =
    if options.verbose then
        printfn "Shader size is: %d" (Printer.quickPrint code).Length

let rename codes =
    let codes = Renamer.renameTopLevel codes Renamer.Unambiguous [||]
    let identTable = computeFrequencyIdentTable codes
    Renamer.computeContextTable codes

    let codes = Renamer.renameTopLevel codes Renamer.Context identTable
    vprintf "%d identifiers renamed. " Renamer.numberOfUsedIdents
    printSize codes
    codes

let readFile file =
    let stream =
        if file = "" then new StreamReader(Console.OpenStandardInput())
        else new StreamReader(file)
    stream.ReadToEnd()

let minify(filename, content: string) =
    vprintf "Input file size is: %d\n" (content.Length)
    let code = Parse.runParser filename content
    vprintf "File parsed. "; printSize code

    let code = Rewriter.reorder code

    let code = Rewriter.apply code
    vprintf "Rewrite tricks applied. "; printSize code

    let code =
        if options.noRenaming then code
        else rename code

    vprintf "Minification of '%s' finished.\n" filename
    code

let minifyFile file =
    let content = readFile file
    let filename = if file = "" then "stdin" else file
    minify(filename, content)

let run files =
    let fail (exn:exn) s =
          printfn "%s" s;
          printfn "%s" exn.StackTrace
          1
    try
        let codes = Array.map minifyFile files
        CGen.print (Array.zip files codes) options.targetOutput
        0
    with
        | Failure s as exn -> fail exn s
        | exn -> fail exn exn.Message

let () =
    let err =
        if Options.parse () then 
            run (options.filenames.ToArray())
        else 1
    if Options.debugMode then System.Console.ReadLine() |> ignore
    exit err
