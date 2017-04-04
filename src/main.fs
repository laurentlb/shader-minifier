module main

open System
open System.IO
open Microsoft.FSharp.Text

// Compute table of variables names, based on frequency
let computeFrequencyIdentTable li =
  let _, str = Printer.quickPrint li

  let stat = Seq.countBy id str |> dict
  let get c = let ok, res = stat.TryGetValue(c) in if ok then res else 0
  let letters = ['a'..'z']@['A'..'Z']

  // First, use most frequent letters
  let table = letters |> List.sortBy get |> List.rev |> List.map string

  // Then, generate identifiers with 2 letters
  let score (s:string) = - (get s.[0] + get s.[1])
  let table2 = [for c1 in letters do for c2 in letters do yield c1.ToString() + c2.ToString()]
            |> List.sortBy score

  Printer.identTable <- Array.ofList (table @ table2)

let nullOut = new StreamWriter(Stream.Null) :> TextWriter

// like printf when verbose option is set
let vprintf fmt =
  let out = if Ast.verbose then stdout else nullOut
  fprintf out fmt

let printSize code =
  if Ast.verbose then
      let n, _ = Printer.quickPrint code
      printfn "Shader size is: %d" n

let rename code =
    Renamer.renameMode <- Renamer.Unambiguous
    Printer.printMode <- Printer.SingleChar
    let code, lastIdent = Renamer.renTopLevel code
    computeFrequencyIdentTable code
    Renamer.computeContextTable code

    Printer.printMode <- Printer.FromTable
    Renamer.renameMode <- Renamer.Context
    let code, lastIdent = Renamer.renTopLevel code
    vprintf "%d identifiers renamed. " Renamer.numberOfUsedIdents
    printSize code
    code

let readFile file =
    let stream =
        if file = "" then new StreamReader(Console.OpenStandardInput())
        else new StreamReader(file)
    stream.ReadToEnd()

let minify file =
  let content = readFile file
  let filename = if file = "" then "stdin" else file
  vprintf "Input file size is: %d\n" (content.Length)
  let code = Parse.runParser filename content
  vprintf "File parsed. "; printSize code

  let code = Rewriter.reorder code

  let code = Rewriter.apply code
  vprintf "Rewrite tricks applied. "; printSize code

  let code =
      if Ast.noRenaming then code
      else rename code

//  vprintf "Identifiers renamed. "; printSize code
//  let code =
//    if !Ast.macroThreshold < 10000 then
//      let code, n = Rewriter.injectMacros lastIdent code
//      vprintfn "%d macros added." n
//      code
//    else code
  vprintf "Minification of '%s' finished.\n" file
  code

let run files =
  try
    let codes = Array.map minify files
    CGen.print (Array.zip files codes)
    0
  with
    | Failure s as exn -> printfn "%s" s; 1 //; printfn "%s" exn.StackTrace
    | exn -> printfn "Error: %s" exn.Message; 1 //; printfn "%s" exn.StackTrace

let printHeader () =
  printfn "Shader Minifier %s (c) Laurent Le Brun 2012" Ast.version
  printfn "http://www.ctrl-alt-test.fr"
  printfn ""

let () =
  let files = ref []
  let setFile s = files := s :: !files

  let setFieldNames s =
    if s = "rgba" || s = "xyzw" || s = "stpq" || s = "" then
      Ast.fieldNames <- s
    else
      printfn "'%s' is not a valid value for field-names" s
      printfn "You must use 'rgba', 'xyzw', or 'stpq'."

  let noRenamingFct (s:string) = Ast.noRenamingList <- [for i in s.Split([|','|]) -> i.Trim()]

  let setFormat = function
      | "c-variables" -> Ast.targetOutput <- Ast.CHeader
      | "js" -> Ast.targetOutput <- Ast.JS
      | "c-array" -> Ast.targetOutput <- Ast.CList
      | "none" -> Ast.targetOutput <- Ast.Text
      | "nasm" -> Ast.targetOutput <- Ast.Nasm
      | s -> printfn "'%s' is not a valid format" s

  let specs =
    ["-o", ArgType.String (fun s -> Ast.outputName <- s), "Set the output filename (default is shader_code.h)"
     "-v", ArgType.Unit (fun() -> Ast.verbose<-true), "Verbose, display additional information"
     "--hlsl", ArgType.Unit (fun() -> Ast.hlsl<-true), "Use HLSL (default is GLSL)"
     "--format", ArgType.String setFormat, "Can be: c-variables (default), c-array, js, nasm, or none"
     "--field-names", ArgType.String setFieldNames, "Choose the field names for vectors: 'rgba', 'xyzw', or 'stpq'"
     "--preserve-externals", ArgType.Unit (fun() -> Ast.preserveExternals<-true), "Do not rename external values (e.g. uniform)"
     "--preserve-all-globals", ArgType.Unit (fun() -> Ast.preserveAllGlobals<-true; Ast.preserveExternals<-true), "Do not rename functions and global variables"
     "--no-renaming", ArgType.Unit (fun() -> Ast.noRenaming<-true), "Do not rename anything"
     "--no-renaming-list", ArgType.String noRenamingFct, "Comma-separated list of functions to preserve"
     "--no-sequence", ArgType.Unit (fun() -> Ast.noSequence<-true), "Do not use the comma operator trick"
     "--smoothstep", ArgType.Unit (fun() -> Ast.smoothstepTrick<-true), "Use IQ's smoothstep trick"
     //"--macro-threshold", ArgType.Int (fun i ->
     //    printfn "Macros are disabled in the release."; Ast.macroThreshold <- i), "[disabled] Use a #define macro if it can save at least <int> bytes"
     //"--make-elevated2", ArgType.Unit (fun () -> printfn "Please buy the commercial version."; exit 1), "Generate the 4k intro 'Elevated 2'"
     "--shader-only", ArgType.Unit (fun() -> Ast.targetOutput<-Ast.Text), "[Deprecated]"
     "--js-output", ArgType.Unit (fun() -> Ast.targetOutput<-Ast.JS), "[Deprecated]"
     "--", ArgType.Rest setFile, "Stop parsing command line"
    ] |> List.map (fun (s, f, d) -> ArgInfo(s, f, d))

  ArgParser.Parse(specs, setFile)
  files := List.rev !files

  let myExit n =
      if Ast.debugMode then System.Console.ReadLine() |> ignore
      exit n

  if !files = [] then
    printHeader()
    ArgParser.Usage(specs, usage="Please give the shader files to compress on the command line.")
    myExit 1
  elif List.length !files > 1 && not Ast.preserveExternals then
    printfn "When compressing multiple files, you must use the --preserve-externals option."
    myExit 1
  else
    if Ast.verbose then printHeader()
    myExit (run (Array.ofList !files))
