module Printer

open Ast

let mutable identTable: string[] = [||]
let out a = sprintf a

// how to print variable names
type printMode = FromTable | SingleChar | Nothing
let mutable printMode = Nothing

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

let idToS (id: string) =
  if id.[0] = '0' then
    match printMode with
    | FromTable -> id
    | Nothing -> ""
    | SingleChar -> string (char (1000 + int id))
  else id

let listToS toS sep li =
  List.map toS li |> String.concat sep

let floatToS f =
    let si = if f < 0. then "-" else ""
    let test = out "%g" (abs f)
    // display "3." instead of "3"
    if fst (System.Int32.TryParse test) then (out "%g." f)
    // display ".5" instead of "0.5"
    else if test.[0] = '0' then si + (test.[1..])
    else si + test

let rec exprToS exp = exprToSLevel 0 exp

// Convert Expr option to string, with default value.
and exprToSOpt def exp =
    defaultArg (Option.map exprToS exp) def

and exprToSLevel level = function
  | Int (i, suf) -> (out "%d%s" i suf)
  | Float (f, suf) -> out "%s%s" (floatToS f) suf
  | Var s -> idToS s
  | FunCall(f, args) ->
        match f, args with
        | Var "?:", [a1; a2; a3] ->
           let prec = precedence.["?:"]
           let res = out "%s?%s:%s" (exprToSLevel prec a1) (exprToSLevel prec a2) (exprToSLevel prec a3)
           if prec < level then out "(%s)" res else res
        | Var op, _ when System.Char.IsLetter op.[0] -> out "%s(%s)" (idToS op) (listToS exprToS "," args)
        | Var op, _ when op.[0] = '0' -> out "%s(%s)" (idToS op) (listToS exprToS "," args)
        | Var op, [a1] when op.[0] = '$' -> out "%s%s" (exprToSLevel precedence.[op] a1) op.[1..]
        | Var op, [a1] -> out "%s%s" op (exprToSLevel precedence.["_" + op] a1)
        | Var op, [a1; a2] ->
           let prec = precedence.[op]
           let res =
              if prec = 1 then // "=", "+=", or other operative with right-associativity
                  out "%s%s%s" (exprToSLevel (prec+1) a1) op (exprToSLevel prec a2)
              else
                  out "%s%s%s" (exprToSLevel prec a1) op (exprToSLevel (prec+1) a2)
           if prec < level then out "(%s)" res
           else res
        | _ -> out "%s(%s)" (exprToS f) (listToS exprToS "," args)
  | Subscript(arr, ind) ->
      out "%s[%s]" (exprToS arr) (exprToSOpt "" ind)
  | Cast(id, e) ->
      // Cast seems to have the same precedence as unary minus
      out "(%s)%s" id (exprToSLevel precedence.["_-"] e)
  | VectorExp(li) ->
      out "{%s}" (listToS exprToS "," li)
  | Dot(e, field) ->
      out "%s.%s" (exprToSLevel precedence.["."] e) field

// Add a space if needed
let sp (s: string) =
  if s.Length > 0 && System.Char.IsLetterOrDigit (s.[0]) then " " + s
  else s

let sp2 (s: string) (s2: string) =
  if s.Length > 0 && System.Char.IsLetterOrDigit(s.[s.Length-1]) &&
     s2.Length > 0 && System.Char.IsLetterOrDigit(s2.[0]) then s + " " + s2
  else s + s2

let backslashN() =
    match targetOutput with
    | Text -> "\n"
    | Nasm -> "', 10, '"
    | _ ->  "\\n"

// Print HLSL semantics
let semToS sem =
  let res = sem |> List.map exprToS |> String.concat ":"
  if res = "" then res else ":" + res

let rec structToS prefix id decls =
  let name = match id with None -> "" | Some s -> " " + s
  let d = decls |> List.map declToS |> List.map (fun s -> s + ";") |> String.concat ""
  out "%s{%s}" (sp2 prefix name) d

and typeSpecToS = function
  | TypeName s -> s
  | TypeStruct(prefix, id, decls) -> structToS prefix id decls

and typeToS (ty: Type) =
  let get = Option.fold (fun _ s -> s + " ") ""
  let typeSpec = typeSpecToS ty.name
  out "%s%s" (get ty.typeQ) typeSpec

and declToS (ty, vars) =
  let out1 decl =
    let size =
      match decl.size with
      | None -> ""
      | Some (Int (0, _)) -> "[]"
      | Some n -> out "[%s]" (exprToS n)

    let init =
      match decl.init with
      | None -> ""
      | Some i -> out "=%s" (exprToS i)
    out "%s%s%s%s" (idToS decl.name) size (semToS decl.semantics) init

  if vars = [] then ""
  else out "%s %s" (typeToS ty) (vars |> List.map out1 |> String.concat ",")

let ignoreFirstNewLine = ref true
let nl =
  let wh = String.replicate 80 " "
  fun n ->
    if !ignoreFirstNewLine then
      ignoreFirstNewLine := false
      ""
    else
      match targetOutput with
      | Text -> ""
      | CHeader | CList -> out "\"\r\n%s\"" wh.[0 .. 2 * n]
      | JS -> out "\" +\r\n%s\"" wh.[0 .. 2 * n]
      | Nasm -> out "'\r\n\tdb%s'" wh.[0 .. 2 * n]

let escape (s: string) =
    match targetOutput with
    | Text -> s
    | CHeader | CList | JS -> s.Replace("\"", "\\\"").Replace("\n", "\\n")
    | Nasm -> s.Replace("'", "\'").Replace("\n", "', 10, '")

let rec instrToS' indent = function
  | Block [] -> ";"
  | Block b ->
     let body = List.map (instrToS (indent+1)) b |> String.concat ""
     out "{%s%s}" body (nl indent)
  | Decl (_, []) -> ""
  | Decl d -> out "%s;" (declToS d)
  | Expr e -> out "%s;" (exprToS e)
  | If(cond, th, el) ->
     let el = match el with
               | None -> ""
               | Some el -> out "%s%s%s%s" (nl indent) "else" (nl (indent+1)) (instrToS' (indent+1) el |> sp)
     out "if(%s)%s%s" (exprToS cond) (instrToSInd indent th) el
  | ForD(init, cond, inc, body) ->
     let cond = exprToSOpt "" cond
     let inc = exprToSOpt "" inc
     out "%s(%s;%s;%s)%s" "for" (declToS init) cond inc (instrToSInd indent body)
  | ForE(init, cond, inc, body) ->
     let cond = exprToSOpt "" cond
     let inc = exprToSOpt "" inc
     let init = exprToSOpt "" init
     out "%s(%s;%s;%s)%s" "for" init cond inc (instrToSInd indent body)
  | While(cond, body) ->
     out "%s(%s)%s" "while" (exprToS cond) (instrToSInd indent body)  
  | DoWhile(cond, body) ->
     out "%s%s%s(%s)" "do" "while" (exprToS cond |> sp) (instrToS indent body)
  | Keyword(k, None) -> out "%s;" k
  | Keyword(k, Some exp) -> out "%s%s;" k (exprToS exp |> sp)
  | Verbatim s ->
      // add a space at end when it seems to be needed
      let s = if System.Char.IsLetterOrDigit s.[s.Length - 1] then s + " " else s
      if s <> "" && s.[0] = '#' then out "%s%s" (backslashN()) (escape s)
      else escape s

and instrToS indent i =
  out "%s%s" (nl indent) (instrToS' indent i)

// print indented instruction
and instrToSInd indent i = instrToS (indent+1) i

let funToS (f: FunctionType) =
  out "%s %s(%s)%s" (typeToS f.retType) (idToS f.fName) (listToS declToS "," f.args) (semToS f.semantics)

let topLevelToS = function
  | TLVerbatim s ->
      // add a space at end when it seems to be needed
      let s = if System.Char.IsLetterOrDigit s.[s.Length - 1] then s + " " else s
      out "%s%s" (nl 0) (escape s)
  | Function (fct, Block []) -> out "%s%s%s{}" (nl 0) (funToS fct) (nl 0)
  | Function (fct, (Block _ as body)) -> out "%s%s%s" (nl 0) (funToS fct) (instrToS 0 body)
  | Function (fct, body) -> out "%s%s%s{%s%s}" (nl 0) (funToS fct) (nl 0) (instrToS 1 body) (nl 0)
  | TLDecl (_, []) -> ""
  | TLDecl decl -> out "%s%s;" (nl 0) (declToS decl)
  | TypeDecl t -> out "%s;" (typeSpecToS t)

let print tl =
    let wasMacro = ref true
    // handle the required \n before a macro
    ignoreFirstNewLine := true
    let f x =
        let isMacro = match x with TLVerbatim s -> s <> "" && s.[0] = '#' | _ -> false
        let needEndline = isMacro && not !wasMacro
        wasMacro := isMacro
        if needEndline then out "%s%s" (backslashN()) (topLevelToS x)
        else topLevelToS x

    tl |> List.map f |> String.concat ""

let quickPrint tl =
  let out = Ast.targetOutput
  Ast.targetOutput <- Text
  let str = print tl
  Ast.targetOutput <- out
  str.Length, str
