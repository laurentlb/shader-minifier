module Formatter

open System
open System.IO
open Options.Globals

let private printHeader out (shaders: Ast.Shader[]) asAList =
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

    for value in List.sort shaders.[0].exportedNames do
        // let newName = Printer.identTable.[int newName]
        if value.ty = "" then
            fprintfn out "# define VAR_%s \"%s\"" (value.name.name.ToUpper()) value.newName.name
        else
            fprintfn out "# define %s_%s \"%s\"" value.ty (value.name.name.ToUpper()) value.newName.name

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

let private printJSHeader out (shaders: Ast.Shader[]) =
    fprintfn out "/* File generated with Shader Minifier %s" Options.version
    fprintfn out " * http://www.ctrl-alt-test.fr"
    fprintfn out " */"

    for value in List.sort shaders.[0].exportedNames do
        if value.ty = "" then
            fprintfn out "var var_%s = \"%s\"" (value.name.name.ToUpper()) value.newName.name
        else
            fprintfn out "var %s_%s = \"%s\"" value.ty (value.name.name.ToUpper()) value.newName.name

    fprintfn out ""
    for shader in shaders do
        let name = (Path.GetFileName shader.filename).Replace(".", "_")
        fprintfn out "var %s = `%s`" name (Printer.print shader.code)
        fprintfn out ""

let private printNasmHeader out (shaders: Ast.Shader[]) =
    fprintfn out "; File generated with Shader Minifier %s" Options.version
    fprintfn out "; http://www.ctrl-alt-test.fr"

    for value in List.sort shaders.[0].exportedNames do
        if value.ty = "" then
            fprintfn out "_var_%s: db '%s', 0" (value.name.name.ToUpper()) value.newName.name
        else
            fprintfn out "_%s_%s: db '%s', 0" value.ty (value.name.name.ToUpper()) value.newName.name

    fprintfn out ""
    for shader in shaders do
        let name = (Path.GetFileName shader.filename).Replace(".", "_")
        fprintfn out "_%s:%s\tdb '%s', 0" name Environment.NewLine (Printer.print shader.code)
        fprintfn out ""

let print out shaders = function
    | Options.Text -> printNoHeader out shaders
    | Options.CHeader -> printHeader out shaders false
    | Options.CList -> printHeader out shaders true
    | Options.JS -> printJSHeader out shaders
    | Options.Nasm -> printNasmHeader out shaders
