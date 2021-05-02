module CGen

open System
open System.IO
open Options.Globals

// Values to export in the C code (uniform and attribute values)
let mutable private exportedValues = ([] : (string * string * string) list)

// 'ty' is a prefix for the type of shader param. Nothing (VAR) for vars, "F" for hlsl functions
let export ty name (newName:string) =
    if newName.[0] <> '0' then
        exportedValues <- exportedValues |> List.map (fun (ty2, name2, newName2 as arg) ->
            if ty = ty2 && name = newName2 then ty, name2, newName
            else arg
        )
    else
        exportedValues <- (ty, name, newName) :: exportedValues

let private output() =
    if Options.debugMode || options.outputName = "" || options.outputName = "-" then stdout
    else new StreamWriter(options.outputName) :> TextWriter

let private printHeader data asAList =
    use out = output()
    let fileName =
        if options.outputName = "" || options.outputName = "-" then "shader_code.h"
        else Path.GetFileName options.outputName
    let macroName = fileName.Replace(".", "_").ToUpper() + "_"

    fprintfn out "/* File generated with Shader Minifier %s" Options.version
    fprintfn out " * http://www.ctrl-alt-test.fr"
    fprintfn out " */"

    if not asAList then
        fprintfn out "#ifndef %s" macroName
        fprintfn out "# define %s" macroName

    for ty, name, newName in List.sort exportedValues do
        // let newName = Printer.identTable.[int newName]
        if ty = "" then
            fprintfn out "# define VAR_%s \"%s\"" (name.ToUpper()) newName
        else
            fprintfn out "# define %c_%s \"%s\"" (System.Char.ToUpper ty.[0]) (name.ToUpper()) newName

    fprintfn out ""
    for file : string, code in data do
        let name = (Path.GetFileName file).Replace(".", "_")
        if asAList then
            fprintfn out "// %s" file
            fprintfn out "\"%s\"," (Printer.print code)
        else
            fprintfn out "const char *%s =%s \"%s\";" name Environment.NewLine (Printer.print code)
        fprintfn out ""

    if not asAList then fprintfn out "#endif // %s" macroName

let private printNoHeader data =
    use out = output()
    let str = [for _, code in data -> Printer.print code] |> String.concat "\n"
    fprintf out "%s" str

let private printJSHeader data =
    use out = output()

    fprintfn out "/* File generated with Shader Minifier %s" Options.version
    fprintfn out " * http://www.ctrl-alt-test.fr"
    fprintfn out " */"

    for ty, name, newName in List.sort exportedValues do
        if ty = "" then
            fprintfn out "var var_%s = \"%s\"" (name.ToUpper()) newName
        else
            fprintfn out "var %c_%s = \"%s\"" (System.Char.ToUpper ty.[0]) (name.ToUpper()) newName

    fprintfn out ""
    for file : string, code in data do
        let name = (Path.GetFileName file).Replace(".", "_")
        fprintfn out "var %s =%s \"%s\"" name Environment.NewLine (Printer.print code)
        fprintfn out ""

let private printNasmHeader data =
    use out = output()

    fprintfn out "; File generated with Shader Minifier %s" Options.version
    fprintfn out "; http://www.ctrl-alt-test.fr"

    for ty, name, newName in List.sort exportedValues do
        if ty = "" then
            fprintfn out "_var_%s: db '%s', 0" (name.ToUpper()) newName
        else
            fprintfn out "_%c_%s: db '%s', 0" (System.Char.ToUpper ty.[0]) (name.ToUpper()) newName

    fprintfn out ""
    for file : string, code in data do
        let name = (Path.GetFileName file).Replace(".", "_")
        fprintfn out "_%s:%s\tdb '%s', 0" name Environment.NewLine (Printer.print code)
        fprintfn out ""

let print data = function
    | Options.Text -> printNoHeader data
    | Options.CHeader -> printHeader data false
    | Options.CList -> printHeader data true
    | Options.JS -> printJSHeader data
    | Options.Nasm -> printNasmHeader data
