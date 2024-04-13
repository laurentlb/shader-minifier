module Formatter

open System
open System.IO
open Options.Globals

let minify shader =
    if options.exportKkpSymbolMaps
    then Printer.printAndWriteSymbols shader
    else Printer.print shader.code

let private formatPrefix = function
    | Ast.ExportPrefix.Variable -> "var"
    | Ast.ExportPrefix.HlslFunction -> "F"

let private splitIndent (str: string) = [
    for line in str.Split([|'\000'|]) do
        // count the number of spaces at the beginning of the string
        let indentSize = line |> Seq.takeWhile (fun c -> c = ' ') |> Seq.length
        yield line.Substring(0, indentSize), line.Substring(indentSize)
    ]

let private escape (str: string) =
    str.Replace("\"", "\\\"").Replace("\n", "\\n")

let private printCVariables out (shaders: Ast.Shader[]) exportedNames =
    let fileName =
        if options.outputName = "" || options.outputName = "-" then "shader_code.h"
        else Path.GetFileName options.outputName
    let macroName = fileName.Replace(".", "_").ToUpper() + "_"

    fprintfn out "// Generated with Shader Minifier %s (https://github.com/laurentlb/Shader_Minifier/)" Options.version

    fprintfn out "#ifndef %s" macroName
    fprintfn out "# define %s" macroName

    for value: Ast.ExportedName in List.sort exportedNames do
        fprintfn out "# define %s_%s \"%s\"" ((formatPrefix value.prefix).ToUpper()) value.name value.newName

    fprintfn out ""
    for shader in shaders do
        let name = (Path.GetFileName shader.filename).Replace(".", "_")
        fprintfn out "const char *%s =" name
        let lines = splitIndent (minify shader)
        let lines = [for indent, line in lines do sprintf "  %s\"%s\"" indent (escape line)]
        fprintfn out "%s;" (String.concat Environment.NewLine lines)

    fprintfn out "#endif // %s" macroName

let private printCArray out (shaders: Ast.Shader[]) exportedNames =
    fprintfn out "// Generated with Shader Minifier %s (https://github.com/laurentlb/Shader_Minifier/)" Options.version

    fprintfn out "#ifndef SHADER_MINIFIER_IMPL"
    fprintfn out "#ifndef SHADER_MINIFIER_HEADER"
    fprintfn out "# define SHADER_MINIFIER_HEADER"

    for value: Ast.ExportedName in List.sort exportedNames do
        fprintfn out "# define %s_%s \"%s\"" ((formatPrefix value.prefix).ToUpper()) value.name value.newName

    fprintfn out "#endif"
    fprintfn out ""
    fprintfn out "#else // if SHADER_MINIFIER_IMPL"

    fprintfn out ""
    for shader in shaders do
        fprintfn out "// %s" shader.filename
        let lines = splitIndent (minify shader)
        let lines = [for indent, line in lines do sprintf "  %s\"%s\"" indent (escape line)]

        for indent, line in splitIndent (minify shader) do
            fprintfn out "  %s\"%s\"" indent (escape line)

        fprintfn out "\"%s\"," (String.concat Environment.NewLine lines)
        fprintfn out ""

    fprintfn out "#endif"

let private printNoHeader out (shaders: Ast.Shader[]) =
    let str = [for shader in shaders -> minify shader] |> String.concat "\n"
    fprintf out "%s" str

let private printIndented out (shaders: Ast.Shader[]) =
    [for shader in shaders do
        if shaders.Length > 1 then
            yield "// " + shader.filename
        yield minify shader]
    |> String.concat "\n\n"
    |> fprintf out "%s"

let private printJSHeader out (shaders: Ast.Shader[]) exportedNames =
    fprintfn out "// Generated with Shader Minifier %s (https://github.com/laurentlb/Shader_Minifier/)" Options.version

    for value: Ast.ExportedName in List.sort exportedNames do
        fprintfn out "var %s_%s = \"%s\"" (formatPrefix value.prefix) (value.name.ToUpper()) value.newName

    fprintfn out ""
    for shader in shaders do
        let name = (Path.GetFileName shader.filename).Replace(".", "_")
        fprintfn out "var %s = `%s`" name (minify shader)
        fprintfn out ""

let private printNasmHeader out (shaders: Ast.Shader[]) exportedNames =
    fprintfn out "; Generated with Shader Minifier %s (https://github.com/laurentlb/Shader_Minifier/)" Options.version

    for value: Ast.ExportedName in List.sort exportedNames do
        fprintfn out "_%s_%s: db '%s', 0" (formatPrefix value.prefix) (value.name.ToUpper()) value.newName

    fprintfn out ""
    for shader in shaders do
        let name = (Path.GetFileName shader.filename).Replace(".", "_")
        fprintfn out "_%s:%s\tdb '%s', 0" name Environment.NewLine (minify shader)
        fprintfn out ""

let private printRustHeader out (shaders: Ast.Shader[]) exportedNames =
    fprintfn out "// Generated with Shader Minifier %s (https://github.com/laurentlb/Shader_Minifier/)" Options.version

    for value: Ast.ExportedName in List.sort exportedNames do
        fprintfn out "pub const %s_%s: &'static [u8] = b\"%s\\0\";" ((formatPrefix value.prefix).ToUpper()) (value.name.ToUpper()) value.newName

    for shader in shaders do
        fprintfn out ""
        let name = (Path.GetFileName shader.filename).Replace(".", "_")    
        fprintfn out "pub const %s: &'static [u8] = b\"\\%s %s\\0\";" (name.ToUpper()) Environment.NewLine (minify shader)

let print out shaders exportedNames = function
    | Options.IndentedText -> printIndented out shaders
    | Options.Text -> printNoHeader out shaders
    | Options.CVariables -> printCVariables out shaders exportedNames
    | Options.CArray -> printCArray out shaders exportedNames
    | Options.JS -> printJSHeader out shaders exportedNames
    | Options.Nasm -> printNasmHeader out shaders exportedNames
    | Options.Rust -> printRustHeader out shaders exportedNames
