module Parse

open Options.Globals

module private ParseImpl =

    // TODO: true, false

    open FParsec.Primitives
    open FParsec.CharParsers
    open FParsec

    let private commentLine = parse {
        do! skipString "//" // .>> noneOf "[")) // (pchar '[')) // <?> "comment, not verbatim code"
        do! notFollowedBy (anyOf "[]") <?> "not a verbatim code"
        do! skipManyTill anyChar (followedBy newline) } |> attempt

    let private commentBlock = parse {
        do! skipString "/*"
        do! skipManyTill anyChar (skipString "*/") }

    let ws = (many (choice [spaces1; commentLine; commentBlock] <?> "") |>> ignore)

    let skipComment = skipMany (commentLine <|> commentBlock)

    let verbatim = parse {
        do! skipString "//["
        do! skipComment
        let! content = manyCharsTill (anyChar .>> skipComment) (pstring "//]")
        do! ws
        return content }

    let ch c = skipChar c >>. ws
    let str s = pstring s .>> ws

    let ident =
        let nonDigit = asciiLetter <|> pchar '_'
        let p = pipe2 nonDigit (manyChars (nonDigit <|> digit <?> "")) (fun c s -> Ast.Ident (c.ToString() + s))
        (p .>> ws) <?> "identifier"

    let opp = new OperatorPrecedenceParser<_,_,_>()
    let exprNoCommaNoVerbatim = opp.ExpressionParser
    let exprNoComma = choice [exprNoCommaNoVerbatim; verbatim |>> Ast.VerbatimExp]
    let expr = sepBy1 exprNoComma (ch ',') |>> (List.reduce (fun acc e -> Ast.FunCall(Ast.Op ",", [acc;e])))

    let parenExp = between (ch '(') (ch ')') expr

    // Primitives
    let octal =
        let r = @"0[0-7]+"
        let conv s = System.Convert.ToInt32(s, 8) |> (fun x -> Ast.Int(x, ""))
        let body = regex r |>> conv
        body .>> ws

    let hexa =
        let prefix = pstring "0x" <|> pstring "0X"
        let r = @"([0-9a-fA-F])+"
        let conv s = System.Convert.ToInt32(s, 16) |> (fun x -> Ast.Int(x, ""))
        let body = regex r |>> conv
        prefix >>. body .>> ws

    let number =
        let r = @"(\d+\.?\d*|\.\d+)([eE][-+]?[0-9]+)?"
        let conv s =
            let ok, res = System.Int32.TryParse(s : string)
            if ok then Ast.Int (res, "")
            else Ast.Float (try decimal s, "" with _ -> failwith ("invalid number: " + s))
        regex r .>> ws |>> conv

    let anyNumber =
        let n = (hexa <|> octal <|> number) <?> "number"
        // number suffixes: float, long float, unsigned, long, half. FIXME: test the last two
        let suffix = ["f"; "F"; "LF"; "lf"; "u"; "U"; "l"; "L"; "h"; "H"]
                     |> List.map str |> choice
        let suffix = suffix <?> "suffix"
        let addSuffix = function
            | Some su, Ast.Int (i, _) -> Ast.Int (i, su)
            | Some su, Ast.Float (f,_) -> Ast.Float (f, su)
            | _, n -> n
        pipe2 n (opt suffix) (fun nb su -> addSuffix (su, nb))

    let vectorExp =
        let inner = sepBy exprNoComma (ch ',')
        between (ch '{') (ch '}') inner |>> Ast.VectorExp

    let prim' = choice [vectorExp; parenExp; ident |>> Ast.Var; anyNumber]
               <?> "expression"

    let cast =
        let op = between (ch '(') (ch ')') ident
        pipe2 op prim' (fun id e -> Ast.Cast(id, e)) <?> "cast"

    let prim = attempt cast <|> prim'

    // Very high priority (parenthesis, function call, field access)
    let argList = sepBy exprNoComma (ch ',')
    let fcall = between (ch '(') (ch ')') argList |>>
                (fun args fct -> Ast.FunCall(fct, args))
    let subscript = between (ch '[') (ch ']') (opt expr) |>>
                    (fun ind arr -> Ast.Subscript(arr, ind))
    let dot = ch '.' >>. ident |>> (fun field r -> Ast.Dot(r, field.Name))
    let post = (dot <|> subscript <|> fcall) <?> ""

    let simpleExpr = pipe2 prim (many post)
                      (fun prim posts -> List.fold (fun acc elt -> elt acc) prim posts)
    opp.TermParser <- simpleExpr

    // Operators

    let precedence1 = [
        ["*"; "/"; "%"], Associativity.Left
        ["+"; "-"], Associativity.Left
        ["<<"; ">>"], Associativity.Left
        ["<"; ">"; "<="; ">="], Associativity.Left
        ["=="; "!="], Associativity.Left
        ["&"], Associativity.Left
        ["^"], Associativity.Left
        ["|"], Associativity.Left
        ["&&"], Associativity.Left
        ["^^"], Associativity.Left
        ["||"], Associativity.Left
    ]
    // precedence of ?: is between precedence1 and precedence2
    let precedence2 = [
        ["="; "+="; "-="; "*="; "/="; "%="; "<<="; ">>="; "&="; "^="; "|="], Associativity.Right
    ]

    // Add all the operators in the OperatorParser
    let () =
        // we start with operators with highest priority, then we decrement the counter.
        let mutable precCounter = 20 //(we have at most 20 different priorities)
        let addInfix li =
            for ops, assoc in li do
                precCounter <- precCounter - 1
                for op in ops do
                    opp.AddOperator(InfixOperator(op, ws, precCounter, assoc, fun x y -> Ast.FunCall(Ast.Op op, [x; y])))

        let addPrefix() =
            precCounter <- precCounter - 1
            for op in ["++"; "--"; "+"; "-"; "~"; "!"] do
                opp.AddOperator(PrefixOperator(op, ws, precCounter, true, fun x -> Ast.FunCall(Ast.Op op, [x])))

        let addPostfix() =
            precCounter <- precCounter - 1
            for op in ["++"; "--"] do
                opp.AddOperator(PostfixOperator(op, ws, precCounter, true, fun x -> Ast.FunCall(Ast.Op ("$"+op), [x])))

        addPostfix()
        addPrefix()
        addInfix precedence1
        precCounter <- precCounter - 1
        opp.AddOperator(TernaryOperator("?", ws, ":", ws, precCounter, Associativity.Right, fun x y z -> Ast.FunCall(Ast.Op "?:", [x; y; z])))
        addInfix precedence2

    let simpleStatement = opt expr |>> (function Some exp -> Ast.Expr exp | None -> Ast.Block [])
    let statement, stmtRef = createParserForwardedToRef()
    let declaration, declRef = createParserForwardedToRef()

    let keyword s = attempt (pstring s .>> notFollowedBy letter .>> notFollowedBy digit .>> notFollowedBy (ch '_')) .>> ws

    // A type block, like struct or interface blocks
    let blockSpecifier prefix =

        // Restriction on field names
        let check ((_,l) as arg : Ast.Decl) =
            for decl in l do
                if decl.name.Name <> Rewriter.renameField decl.name.Name then
                    failwithf "Record field name '%s' is not allowed by Shader Minifier,\nbecause it looks like a vec4 field name." decl.name.Name
            arg

        let decls = many (declaration .>> ch ';' |>> check)
        let name = opt ident
        pipe2 name (between (ch '{') (ch '}') decls)
            (fun n d -> Ast.TypeStruct(prefix, n, d))

    let structSpecifier = parse {
        let! str = keyword "struct"
        let! res = blockSpecifier str
        return res
    }

    let structDecl =
        let semi = if options.hlsl then opt (ch ';') |>> ignore else ch ';'
        (structSpecifier .>> semi) |>> Ast.TypeDecl

    // eg. "const out int", "uniform float"
    let specifiedTypeGLSL =
        let storage = ["const"; "inout"; "in"; "out"; "centroid"
                       "patch"; "sample"; "uniform"; "buffer"; "shared"; "coherent"
                       "volatile"; "restrict"; "readonly"; "writeonly"; "subroutine"
                       "attribute"; "varying"
                       "highp"; "mediump"; "lowp"
                       "invariant"; "precise"
                       "smooth"; "flat"; "noperspective"
                      ]
                      |> List.map keyword |> choice <?> "Type qualifier"
        let layout = keyword "layout" >>. ch '(' >>. manyCharsTill anyChar (ch ')')
                     |>> (function s -> "layout(" + s + ")")
        let qualifier = many (storage <|> layout)
                        |>> (function [] -> None | li -> Some (String.concat " " li))
        let typeSpec = structSpecifier <|> (ident |>> (fun id -> Ast.TypeName id.Name))
        pipe2 qualifier typeSpec (fun tyQ name -> Ast.makeType name tyQ)

    let specifiedTypeHLSL =
        let storage = ["extern"; "nointerpolation"; "precise"; "shared"; "groupshared"
                       "static"; "uniform"; "volatile"; "const"; "row_major"; "column_major"
                       "inline"; "target"
                       "out"; "in"; "inout"
                       "linear"; "centroid"; "nointerpolation"; "noperspective"; "sample"
                       "cbuffer"; "tbuffer"
                       // https://docs.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-geometry-shader
                       "point"; "line"; "triangle"; "lineadj"; "triangleadj"
                      ]
                      |> List.map keyword |> choice <?> "Type qualifier"
        let qualifier = many storage |>> (function [] -> None | li -> Some (String.concat " " li))
        let generic = ch '<' >>. manyCharsTill anyChar (ch '>')
                   |> opt
                   |>> (function Some s -> "<" + s + ">" | None -> "")
        let typeName = pipe2 (ident |>> (fun id -> id.Name)) generic (+)
        let typeSpec = structSpecifier <|> (typeName |>> Ast.TypeName)
        pipe3 qualifier typeSpec generic (fun tyQ name _ -> Ast.makeType name tyQ)

    let specifiedType = parse {
        let! ret = if options.hlsl then specifiedTypeHLSL else specifiedTypeGLSL
        return ret
    }

    // For HLSL, e.g. ": color"
    let semantics =
        many (ch ':' >>. simpleExpr)

    // eg. "int foo[] = exp, bar = 3"
    declRef := (
        let bracket = between (ch '[') (ch ']') (opt expr) |>> (fun size -> defaultArg size (Ast.Int (0, "")))
        let init = ch '=' >>. exprNoComma
        let var = pipe4 ident (opt bracket) semantics (opt init) Ast.makeDecl
        let list = sepBy1 var (ch ',')
        tuple2 specifiedType list
    )

    // eg. int foo[]   used for function arguments
    let singleDeclaration =
        let bracket = between (ch '[') (ch ']') (opt expr) |>> (fun size -> defaultArg size (Ast.Int (0, "")))
        pipe4 specifiedType ident (opt bracket) semantics
            (fun ty id brack sem -> (ty, [Ast.makeDecl id brack sem None]))

    // GLSL, eg. "uniform Transform { ... };"
    let interfaceBlock = parse {
        let! ty = specifiedType
        let! sem = semantics
        let s = sem |> List.map (fun s -> ":" + Printer.exprToS s) |> String.concat ""
        let! ret = blockSpecifier (Printer.typeToS ty + s)
                      |>> Ast.TypeDecl
        // semi-colon seems to be optional in hlsl
        do! if options.hlsl then opt (ch ';') |>> ignore else ch ';'
        return ret
    }

    let forLoop =
        let init1 = declaration |>> (fun decl e2 e3 body -> Ast.ForD(decl, e2, e3, body))
        let init2 = opt expr |>> (fun e1 e2 e3 body -> Ast.ForE(e1, e2, e3, body))
        let init = attempt init1 <|> init2 .>> ch ';'
        let cond = opt expr .>> ch ';'
        let inc = opt expr .>> ch ')'
        pipe4 (keyword "for" >>. ch '(' >>. init) cond inc statement
            (fun f e2 e3 body -> f e2 e3 body)

    let whileLoop =
        pipe2 (keyword "while" >>. parenExp) statement
            (fun cond stmt -> Ast.While(cond, stmt))
    let doWhileLoop =
        pipe2 (keyword "do" >>. statement) (str "while" >>. parenExp)
            (fun stmt cond -> Ast.DoWhile(cond, stmt))
    let ifStatement =
        pipe3 (keyword "if" >>. parenExp) statement (opt (str "else" >>. statement))
            (fun cond stmt1 stmt2 -> Ast.If(cond, stmt1, stmt2))

    let block =
        let list = many statement |>> Ast.Block
        between (ch '{') (ch '}') list

    let caseLabel =
        let keywCase = keyword "case" >>. expr |>> (fun e -> Ast.Case e)
        let keywDefault = keyword "default" |>> (fun _ -> Ast.Default)
        attempt keywCase <|> keywDefault .>> ch ':'

    let case =
        pipe2 caseLabel (many (attempt statement)) (fun l sl -> (l, sl))

    let switch =
      let body = between (ch '{') (ch '}') (many case)
      pipe2 (keyword "switch" >>. parenExp) body (fun e cl -> Ast.Switch(e, cl))

    let mutable private forbiddenNames = []

    let macro =
        let nl = skipComment >>. skipMany (pchar '\\' >>. newline)
        let line = manyCharsTill (anyChar .>> nl) newline
        // an ident, without eating trailing spaces
        let ident = manyChars (pchar '_' <|> asciiLetter <|> digit)
        // parse the #define macros to get the macro name
        let define = pipe2 (keyword "define" >>. ident) line
                       (fun id line -> forbiddenNames <- id :: forbiddenNames; "define " + id + line)
        pchar '#' >>. (define <|> line) .>> ws |>> (fun s -> "#" + s)

    // HLSL attribute, eg. [maxvertexcount(12)]
    let attribute =
        if options.hlsl then
            ch '[' >>. manyCharsTill anyChar (ch ']')
                |>> (function s -> "[" + s + "]")
        else
            pzero

    let jump =
        let key =
            choice [keyword "break"; keyword "continue"; keyword "discard"]
              |>> (fun k -> Ast.Jump(Ast.stringToJumpKeyword k, None))

        let ret = pipe2 (keyword "return") (opt expr) (fun k e -> Ast.Jump(Ast.stringToJumpKeyword k, e))
        (key <|> ret) .>> ch ';'

    // A statement
    stmtRef := choice [
        block
        jump
        forLoop
        ifStatement
        whileLoop
        doWhileLoop
        switch
        verbatim |>> Ast.Verbatim
        macro |>> Ast.Verbatim
        attribute |>> Ast.Verbatim
        attempt ((declaration .>> ch ';') |>> Ast.Decl)
        simpleStatement .>> ch ';'] <?> "instruction"

    // e.g. "int foo(float a[], out int b) : color"
    let functionHeader =
        let void_ = keyword "void" |>> (fun _ -> [])
        let argList = void_ <|> (sepBy singleDeclaration (ch ','))
        let argList = between (ch '(') (ch ')') argList
        pipe4 specifiedType ident argList semantics Ast.makeFunctionType

    let pfunction =
        pipe2 functionHeader block (fun head body -> Ast.Function(head, body))

    let toplevel =
        let decl = declaration .>> ch ';'
        let item = choice [
                    macro |>> Ast.TLVerbatim
                    verbatim |>> Ast.TLVerbatim
                    attribute |>> Ast.TLVerbatim
                    attempt decl |>> Ast.TLDecl
                    structDecl
                    attempt interfaceBlock
                    pfunction
        ]
        let forwardDecl = functionHeader .>> ch ';' |>> (fun _ -> options.reorderFunctions <- true)
        many ((attempt forwardDecl|>>fun _ -> None) <|> (item|>>Some)) |>> List.choose id // FIXME: use skip?

    let parse = ws >>. toplevel .>> eof

    let runParser streamName content : Ast.Shader =
        forbiddenNames <- [ "if"; "in"; "do" ]
        let res = runParserOnString parse () streamName content
        match res with
        | Success(r,_,_) -> { Ast.Shader.filename = streamName; code = r; forbiddenNames = forbiddenNames }
        | Failure(str, _, _) -> failwithf "Parse error: %s" str


let runParser = ParseImpl.runParser
