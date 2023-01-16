module Formatter

open System
open System.IO
open Options.Globals

let private printHeader out (shaders: Ast.Shader[]) asAList exportedNames =
    let fileName =
        if options.outputName = "" || options.outputName = "-" then "shader_code.h"
        else Path.GetFileName options.outputName
    let macroName = fileName.Replace(".", "_").ToUpper() + "_"

    fprintfn out "// Generated with Shader Minifier %s (https://github.com/laurentlb/Shader_Minifier/)" Options.version

    if not asAList then
        fprintfn out "#ifndef %s" macroName
        fprintfn out "# define %s" macroName

    for value: Ast.ExportedName in List.sort exportedNames do
        // let newName = Printer.identTable.[int newName]
        if value.ty = "" then
            fprintfn out "# define VAR_%s \"%s\"" value.name value.newName
        else
            fprintfn out "# define %s_%s \"%s\"" value.ty value.name value.newName

    fprintfn out ""
    for shader in shaders do
        let name = (Path.GetFileName shader.filename).Replace(".", "_")
        if asAList then
            fprintfn out "// %s" shader.filename
            fprintfn out "\"%s\"," (Printer.print shader.code)
        else
            fprintfn out "const char *%s =%s \"%s\";" name Environment.NewLine (Printer.print shader.code)
        fprintfn out ""

    if not asAList then fprintfn out "#endif // %s" macroName

let private printNoHeader out (shaders: Ast.Shader[]) =
    let str = [for shader in shaders -> Printer.print shader.code] |> String.concat "\n"
    fprintf out "%s" str

let private printIndented out (shaders: Ast.Shader[]) =
    [for shader in shaders do
        if shaders.Length > 1 then
            yield "// " + shader.filename
        yield Printer.print shader.code]
    |> String.concat "\n\n"
    |> fprintf out "%s"

let private printJSHeader out (shaders: Ast.Shader[]) exportedNames =
    fprintfn out "/* File generated with Shader Minifier %s" Options.version
    fprintfn out " * http://www.ctrl-alt-test.fr"
    fprintfn out " */"

    for value: Ast.ExportedName in List.sort exportedNames do
        if value.ty = "" then
            fprintfn out "var var_%s = \"%s\"" (value.name.ToUpper()) value.newName
        else
            fprintfn out "var %s_%s = \"%s\"" value.ty (value.name.ToUpper()) value.newName

    fprintfn out ""
    for shader in shaders do
        let name = (Path.GetFileName shader.filename).Replace(".", "_")
        fprintfn out "var %s = `%s`" name (Printer.print shader.code)
        fprintfn out ""

let private printNasmHeader out (shaders: Ast.Shader[]) exportedNames =
    fprintfn out "; File generated with Shader Minifier %s" Options.version
    fprintfn out "; http://www.ctrl-alt-test.fr"

    for value: Ast.ExportedName in List.sort exportedNames do
        if value.ty = "" then
            fprintfn out "_var_%s: db '%s', 0" (value.name.ToUpper()) value.newName
        else
            fprintfn out "_%s_%s: db '%s', 0" value.ty (value.name.ToUpper()) value.newName

    fprintfn out ""
    for shader in shaders do
        let name = (Path.GetFileName shader.filename).Replace(".", "_")
        fprintfn out "_%s:%s\tdb '%s', 0" name Environment.NewLine (Printer.print shader.code)
        fprintfn out ""

let print out shaders exportedNames = function
    | Options.IndentedText -> printIndented out shaders
    | Options.Text -> printNoHeader out shaders
    | Options.CHeader -> printHeader out shaders false exportedNames
    | Options.CList -> printHeader out shaders true exportedNames
    | Options.JS -> printJSHeader out shaders exportedNames
    | Options.Nasm -> printNasmHeader out shaders exportedNames
