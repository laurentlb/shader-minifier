module Printer

open System
open Ast
open Options.Globals
open System.Text.RegularExpressions

type PrinterImpl(outputFormat0) =

    let mutable outputFormat = outputFormat0

    let out a = sprintf a

    let precedenceList = [
        [","]
        ["="; "+="; "-="; "*="; "/="; "%="; "<<="; ">>="; "&="; "^="; "|="] // precedence = 1
        ["?:"]
        ["||"]
        ["^^"]
        ["&&"]
        ["|"]
        ["^"]
        ["&"]
        ["=="; "!="]
        ["<"; ">"; "<="; ">="]
        ["<<"; ">>"]
        ["+"; "-"]
        ["*"; "/"; "%"]
        // _++ is prefix and $++ is postfix
        ["_++"; "_--"; "_+"; "_-"; "_~"; "_!"; "$++"; "$--"]
        ["."]
    ]

    let precedence =
        precedenceList
        |> List.mapi (fun k li -> List.map (fun op -> op, k) li)
        |> List.concat
        |> dict

    let idToS (id: Ident) =
        // In mode Unambiguous, ids contain numbers. We print a single unicode char instead.
        if id.IsUniqueId then
            string (char (1000 + int id.Name))
        else id.Name

    let commaListToS toS li =
        List.map toS li |> String.concat ","

    let floatToS f =
        let a = abs (float f)
        let str1 = a.ToString("#.################", System.Globalization.CultureInfo.InvariantCulture)
        let str1 = if str1 = "" then "0."
                   elif Regex.Match(str1, "^\\d+$").Success then str1 + "."
                   else str1
        let str2 = a.ToString("0.################e0", System.Globalization.CultureInfo.InvariantCulture)
        let str = [str1; str2] |> List.minBy(fun x -> x.Length)
        
        let sign = if f < 0.M then "-" else ""
        sign + str

    let mutable ignoreFirstNewLine = true
    let nl indent = // newline and optionally indent
        if ignoreFirstNewLine then
            ignoreFirstNewLine <- false
            ""
        else
            let spaces = new string(' ', indent * 2 + 1)
            match outputFormat with
            | Options.IndentedText -> Environment.NewLine + new string(' ', indent * 2)
            | Options.Text | Options.JS -> ""
            | Options.CHeader | Options.CList -> out "\"%s%s\"" Environment.NewLine spaces
            | Options.Nasm -> out "'%s\tdb%s'" Environment.NewLine spaces
            | Options.Rust -> out "\\%s%s" Environment.NewLine spaces

    let rec exprToS indent exp = exprToSLevel indent 0 exp

    // Convert Expr option to string, with default value.
    and exprToSOpt indent def exp =
        defaultArg (Option.map (exprToS indent) exp) def

    and exprToSLevel indent level = function
        | Int (i, suf) -> (out "%d%s" i suf)
        | Float (f, suf) -> out "%s%s" (floatToS f) suf
        | Var s -> idToS s
        | Op s -> s
        | FunCall(f, args) ->
            match f, args with
            | Op "?:", [a1; a2; a3] ->
                let prec = precedence.["?:"]
                let res = out "%s?%s%s:%s%s" (exprToSLevel indent prec a1)
                            (nl (indent+1)) (exprToSLevel (indent+1) prec a2)
                            (nl (indent+1)) (exprToSLevel (indent+1) prec a3)
                if prec < level then out "(%s)" res else res
            // Function calls.
            | Var op, _ ->
                // We set level to 1 in case in case a comma operator is used in the argument list.
                out "%s(%s)" (idToS op) (commaListToS (exprToSLevel indent 1) args)
            
            // Unary operators. _++ is prefix and $++ is postfix
            | Op op, [a1] when op.[0] = '$' -> out "%s%s" (exprToSLevel indent precedence.[op] a1) op.[1..]
            | Op op, [a1] -> out "%s%s" op (exprToSLevel indent precedence.["_" + op] a1)

            // Binary operators.
            | Op op, [a1; a2] ->
                let prec = precedence.[op]
                let res =
                    if prec = 1 then // "=", "+=", or other operator with right-associativity
                        out "%s%s%s" (exprToSLevel indent (prec+1) a1) op (exprToSLevel indent prec a2)
                    else
                        out "%s%s%s" (exprToSLevel indent prec a1) op (exprToSLevel indent (prec+1) a2)
                if prec < level then out "(%s)" res
                else res
            | _ -> out "%s(%s)" (exprToS indent f) (commaListToS (exprToS indent) args)
        | Subscript(arr, ind) ->
            out "%s[%s]" (exprToS indent arr) (exprToSOpt indent "" ind)
        | Cast(id, e) ->
            // Cast seems to have the same precedence as unary minus
            out "(%s)%s" id.Name (exprToSLevel indent precedence.["_-"] e)
        | VectorExp(li) ->
            out "{%s}" (commaListToS (exprToS indent) li)
        | Dot(e, field) ->
            out "%s.%s" (exprToSLevel indent precedence.["."] e) field
        | VerbatimExp s -> s

    // Add a space if needed
    let sp (s: string) =
        if s.Length > 0 && System.Char.IsLetterOrDigit (s.[0]) then " " + s
        else s

    let sp2 (s: string) (s2: string) =
        if s.Length > 0 && System.Char.IsLetterOrDigit(s.[s.Length-1]) &&
            s2.Length > 0 && System.Char.IsLetterOrDigit(s2.[0]) then s + " " + s2
        else s + s2

    let backslashN() =
        match outputFormat with
        | Options.Text | Options.JS | Options.IndentedText -> "\n"
        | Options.Nasm -> "', 10, '"
        | Options.CHeader | Options.CList | Options.Rust ->  "\\n"

    // Print HLSL semantics
    let semToS sem =
        let res = sem |> List.map (exprToS 0) |> String.concat ":"
        if res = "" then res else ":" + res

    let rec structToS prefix id decls =
        let name = match id with None -> "" | Some (s: Ident) -> " " + s.Name
        let d = decls |> List.map (fun s -> declToS 0 s + ";") |> String.concat ""
        out "%s{%s}" (sp2 prefix name) d

    and typeSpecToS = function
        | TypeName s -> s
        | TypeStruct(prefix, id, decls) -> structToS prefix id decls

    and typeToS (ty: Type) =
        let get = function
            | [] -> ""
            | li -> (String.concat " " li) + " "
        let typeSpec = typeSpecToS ty.name
        out "%s%s" (get ty.typeQ) typeSpec

    and declToS indent (ty, vars) =
        let out1 decl =
            let size =
                match decl.size with
                | None -> ""
                | Some (Int (0, _)) -> "[]"
                | Some n -> out "[%s]" (exprToS indent n)

            let init =
                match decl.init with
                | None -> ""
                | Some i -> out "=%s" (exprToS indent i)
            out "%s%s%s%s" (idToS decl.name) size (semToS decl.semantics) init

        if vars.IsEmpty then ""
        else out "%s %s" (typeToS ty) (vars |> commaListToS out1)

    let escape (s: string) =
        match outputFormat with
        | Options.IndentedText -> s
        | Options.Text -> s
        | Options.JS -> s
        | Options.CHeader | Options.CList | Options.JS | Options.Rust -> s.Replace("\"", "\\\"").Replace("\n", "\\n")
        | Options.Nasm -> s.Replace("'", "\'").Replace("\n", "', 10, '")

    /// Detect if the current statement might accept a dangling else.
    /// Note that the function needs to be recursive to detect things like:
    ///   if(a) for(;;) if(b) {} else {}
    /// https://github.com/laurentlb/Shader_Minifier/issues/143
    let rec hasDanglingElseProblem = function
        | If(_, _, None) ->
            true
        | If(_, _, Some body)
        | While(_, body)
        | DoWhile(_, body)
        | ForD(_, _, _, body)
        | ForE(_, _, _, body) ->
            hasDanglingElseProblem body
        | _ ->
            false

    let rec stmtToS' indent = function
        | Block [] -> ";"
        | Block b ->
            let body = List.map (stmtToS (indent+1)) b |> String.concat ""
            out "{%s%s}" body (nl indent)
        | Decl (_, []) -> ""
        | Decl d -> out "%s;" (declToS indent d)
        | Expr e -> out "%s;" (exprToS indent e)
        | If(cond, th, el) ->
            let th = if el <> None && hasDanglingElseProblem th then Block [th] else th
            let el = match el with
                     | None -> ""
                     | Some el -> out "%selse%s%s" (nl indent) (nl (indent+1)) (stmtToS' (indent+1) el |> sp)
            out "if(%s)%s%s" (exprToS indent cond) (stmtToSInd indent th) el
        | ForD(init, cond, inc, body) ->
            let cond = exprToSOpt indent "" cond
            let inc = exprToSOpt indent "" inc
            out "%s(%s;%s;%s)%s" "for" (declToS indent init) cond inc (stmtToSInd indent body)
        | ForE(init, cond, inc, body) ->
            let cond = exprToSOpt indent "" cond
            let inc = exprToSOpt indent "" inc
            let init = exprToSOpt indent "" init
            out "%s(%s;%s;%s)%s" "for" init cond inc (stmtToSInd indent body)
        | While(cond, body) ->
            out "%s(%s)%s" "while" (exprToS indent cond) (stmtToSInd indent body)
        | DoWhile(cond, body) ->
            out "%s%s%s(%s)" "do" "while" (exprToS indent cond |> sp) (stmtToS indent body)
        | Jump(k, None) -> out "%s;" (jumpKeywordToString k)
        | Jump(k, Some exp) -> out "%s%s;" (jumpKeywordToString k) (exprToS indent exp |> sp)
        | Verbatim s ->
            // add a space at end when it seems to be needed
            let s = if s.Length > 0 && System.Char.IsLetterOrDigit s.[s.Length - 1] then s + " " else s
            if s <> "" && s.[0] = '#' then out "%s%s" (backslashN()) (escape s)
            else escape s
        | Switch(e, cl) ->
            let labelToS = function
                | Case e -> out "case %s:" (exprToS indent e)
                | Default -> out "default:"
            let caseToS (l, sl) =
                let stmts = List.map (stmtToS (indent+2)) sl |> String.concat ""
                out "%s%s%s" (nl (indent+1)) (labelToS l) stmts
            let body = List.map caseToS cl |> String.concat ""
            out "%s(%s){%s%s}" "switch" (exprToS indent e) body (nl indent)

    and stmtToS indent i =
        out "%s%s" (nl indent) (stmtToS' indent i)

    /// print indented statement
    and stmtToSInd indent i = stmtToS (indent+1) i

    let funToS (f: FunctionType) =
        out "%s %s(%s)%s" (typeToS f.retType) (idToS f.fName) (commaListToS (declToS 0) f.args) (semToS f.semantics)

    let topLevelToS = function
        | TLVerbatim s ->
            // add a space at end when it seems to be needed
            let trailing = if s.Length > 0 && System.Char.IsLetterOrDigit s.[s.Length - 1] then " " else ""
            out "%s%s%s" (nl 0) (escape s) trailing
        | Function (fct, Block []) -> out "%s%s%s{}" (nl 0) (funToS fct) (nl 0)
        | Function (fct, (Block _ as body)) -> out "%s%s%s" (nl 0) (funToS fct) (stmtToS 0 body)
        | Function (fct, body) -> out "%s%s%s{%s%s}" (nl 0) (funToS fct) (nl 0) (stmtToS 1 body) (nl 0)
        | Precision ty -> out "precision %s;" (typeToS ty);
        | TLDecl (_, []) -> ""
        | TLDecl decl -> out "%s%s;" (nl 0) (declToS 0 decl)
        | TypeDecl t -> out "%s;" (typeSpecToS t)

    member _.Print tl =
        let mutable wasMacro = true
        // handle the required \n before a macro
        ignoreFirstNewLine <- true
        let f x =
            let isMacro = match x with TLVerbatim s -> s <> "" && s.[0] = '#' | _ -> false
            let needEndLine = isMacro && not wasMacro
            wasMacro <- isMacro
            if needEndLine then out "%s%s" (backslashN()) (topLevelToS x)
            else topLevelToS x

        tl |> List.map f |> String.concat ""
    member _.ExprToS = exprToS
    member _.TypeToS = typeToS

let print tl = (new PrinterImpl(options.outputFormat)).Print(tl)
let printText tl = (new PrinterImpl(Options.Text)).Print(tl)
let exprToS x = (new PrinterImpl(Options.Text)).ExprToS 0 x
let typeToS ty = (new PrinterImpl(Options.Text)).TypeToS ty
