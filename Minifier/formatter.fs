﻿module internal Formatter

open System
open System.IO

type private Impl(options: Options.Options, withLocations) =

    let minify shader =
        if options.exportKkpSymbolMaps then
            if options.outputFormat = Options.IndentedText then
                failwith "exporting symbols is not compatible with indented mode"
            Printer.writeSymbols shader

        match options.outputFormat with
        | Options.Text | Options.JS | Options.Json ->
            if withLocations then
                Printer.printWithLoc shader.code |> Printer.stripIndentation
            else
                Printer.print shader.code
        | Options.IndentedText | Options.CVariables | Options.CArray | Options.Nasm | Options.Rust ->
            if withLocations then
                Printer.printWithLoc shader.code
            else
                Printer.printIndented shader.code

    let formatPrefix = function
        | Ast.ExportPrefix.Variable -> "var"
        | Ast.ExportPrefix.HlslFunction -> "F"

    let getLines shader = [
        let lines = minify shader
        for line in lines.Trim([|'\000'|]).Split([|'\000'|]) do
            // count the number of \t at the beginning of the string
            let indentLevel = line |> Seq.takeWhile (fun c -> c = '\t') |> Seq.length
            yield String(' ', 2 * indentLevel), line.Substring(indentLevel)
        ]

    let escape (str: string) =
        str.Replace("\"", "\\\"").Replace("\n", "\\n")

    let printCVariables out (shaders: Ast.Shader[]) exportedNames =
        let fileName =
            if options.outputName = "" || options.outputName = "-" then "shader_code.h"
            else Path.GetFileName options.outputName
        let macroName = Ast.mangleToAscii(Path.GetFileName fileName).ToUpper() + "_"

        fprintfn out "// Generated with Shader Minifier %s (https://github.com/laurentlb/Shader_Minifier/)" Options.version

        fprintfn out "#ifndef %s" macroName
        fprintfn out "# define %s" macroName

        for value: Ast.ExportedName in Seq.sort exportedNames do
            fprintfn out "# define %s_%s \"%s\"" ((formatPrefix value.prefix).ToUpper()) value.name value.newName

        fprintfn out ""
        for shader in shaders do
            fprintfn out "const char *%s =" (Ast.mangleToAscii shader.mangledFilename)
            let lines = String.concat Environment.NewLine [
                for indent, line in getLines shader do
                    sprintf " %s\"%s\"" indent (escape line)]
            fprintfn out "%s;" lines
            fprintfn out ""

        fprintfn out "#endif // %s" macroName

    let printCArray out (shaders: Ast.Shader[]) exportedNames =
        fprintfn out "// Generated with Shader Minifier %s (https://github.com/laurentlb/Shader_Minifier/)" Options.version

        fprintfn out "#ifndef SHADER_MINIFIER_IMPL"
        fprintfn out "#ifndef SHADER_MINIFIER_HEADER"
        fprintfn out "# define SHADER_MINIFIER_HEADER"

        for value: Ast.ExportedName in Seq.sort exportedNames do
            fprintfn out "# define %s_%s \"%s\"" ((formatPrefix value.prefix).ToUpper()) value.name value.newName

        fprintfn out "#endif"
        fprintfn out ""
        fprintfn out "#else // if SHADER_MINIFIER_IMPL"

        fprintfn out ""
        for shader in shaders do
            fprintfn out "// %s" shader.filename
            let lines = String.concat Environment.NewLine [
                for indent, line in getLines shader do
                    sprintf " %s\"%s\"" indent (escape line)]
            fprintfn out "%s," lines
            fprintfn out ""

        fprintfn out "#endif"

    let printNoHeader out (shaders: Ast.Shader[]) =
        let str = [for shader in shaders -> minify shader] |> String.concat "\n"
        fprintf out "%s" str

    let printIndented out (shaders: Ast.Shader[]) =
        [for shader in shaders do
            if shaders.Length > 1 then
                yield "// " + shader.filename
            for indent, line in getLines shader do
                yield indent + line + Environment.NewLine
            yield Environment.NewLine
        ]
        |> String.concat ""
        |> fprintf out "%s"

    let printJSHeader out (shaders: Ast.Shader[]) exportedNames =
        fprintfn out "// Generated with Shader Minifier %s (https://github.com/laurentlb/Shader_Minifier/)" Options.version

        for value: Ast.ExportedName in Seq.sort exportedNames do
            fprintfn out "var %s_%s = \"%s\"" (formatPrefix value.prefix) (value.name.ToUpper()) value.newName

        fprintfn out ""
        for shader in shaders do
            fprintfn out "var %s = `%s`" shader.mangledFilename (minify shader)
            fprintfn out ""

    let printNasmHeader out (shaders: Ast.Shader[]) exportedNames =
        let escape (str: string) =
            str.Replace("\"", "\\\"").Replace("\n", "', 10, '")

        fprintfn out "; Generated with Shader Minifier %s (https://github.com/laurentlb/Shader_Minifier/)" Options.version

        for value: Ast.ExportedName in Seq.sort exportedNames do
            fprintfn out "_%s_%s: db '%s', 0" (formatPrefix value.prefix) (value.name.ToUpper()) value.newName

        fprintfn out ""
        for shader in shaders do
            // fprintfn out "_%s:%s\tdb '%s', 0" shader.mangledFilename Environment.NewLine (minify shader)

            fprintfn out "_%s:" (Ast.mangleToAscii shader.mangledFilename) // \tdb '%s', 0" shader.mangledFilename Environment.NewLine (minify shader)
            let lines = String.concat Environment.NewLine [
                for indent, line in getLines shader do
                    sprintf "\tdb %s'%s'" indent (escape line)]
            fprintfn out "%s, 0" lines

            fprintfn out ""

    let printRustHeader out (shaders: Ast.Shader[]) exportedNames =
        fprintfn out "// Generated with Shader Minifier %s (https://github.com/laurentlb/Shader_Minifier/)" Options.version

        for value: Ast.ExportedName in Seq.sort exportedNames do
            fprintfn out "pub const %s_%s: &'static [u8] = b\"%s\\0\";" ((formatPrefix value.prefix).ToUpper()) (value.name.ToUpper()) value.newName

        for shader in shaders do
            fprintfn out ""
            fprintfn out "pub const %s: &'static [u8] = b\"\\" (shader.mangledFilename.ToUpper())
            let lines = String.concat Environment.NewLine [
                for indent, line in getLines shader do
                    sprintf " %s%s\\" indent (escape line)]
            fprintfn out "%s0\";" lines

    let printJsonHeader out (shaders: Ast.Shader[]) (exportedNames: Ast.ExportedName[]) =
        let mappingsMap =
            exportedNames
            |> Seq.sort
            |> Seq.map (fun name -> name.name, name.newName)
            |> Map.ofSeq
        let shadersMap =
            shaders
            |> Seq.map (fun shader -> shader.filename, minify shader)
            |> Map.ofSeq
        let obj =  {| mappings = mappingsMap; shaders = shadersMap |}

        fprintfn out "%s" (Text.Json.JsonSerializer.Serialize obj)

    member this.Format out shaders exportedNames =
        match options.outputFormat with
        | Options.IndentedText -> printIndented out shaders
        | Options.Text -> printNoHeader out shaders
        | Options.CVariables -> printCVariables out shaders exportedNames
        | Options.CArray -> printCArray out shaders exportedNames
        | Options.JS -> printJSHeader out shaders exportedNames
        | Options.Nasm -> printNasmHeader out shaders exportedNames
        | Options.Rust -> printRustHeader out shaders exportedNames
        | Options.Json -> printJsonHeader out shaders exportedNames

let print options =
    Impl(options, false).Format

let printWithLocations options =
    Impl(options, true).Format
