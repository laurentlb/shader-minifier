module internal Parse

open FParsec.Primitives
open FParsec.CharParsers
open FParsec

type private ParseImpl(options: Options.Options) =

    let mutable forbiddenNames = []
    let mutable reorderFunctions = false

    let commentLine = parse {
        do! skipString "//" // .>> noneOf "[")) // (pchar '[')) // <?> "comment, not verbatim code"
        do! notFollowedBy (anyOf "[]") <?> "not a verbatim code"
        do! skipManyTill anyChar (followedBy newline) } |> attempt

    let commentBlock = parse {
        do! skipString "/*"
        do! skipManyTill anyChar (skipString "*/") }

    let ws = (many (choice [spaces1; commentLine; commentBlock] <?> "") |>> ignore<unit list>)

    let skipComment = skipMany (commentLine <|> commentBlock)

    let verbatim = parse {
        do! skipString "//["
        do! many spaces1 |>> ignore<unit list>
        let! content = manyCharsTill anyChar (pstring "//]")
        do! ws
        return content }

    let ch c = skipChar c >>. ws
    let str s = pstring s .>> ws

    let ident =
        let nonDigit = asciiLetter <|> pchar '_'
        let p = pipe3 getPosition nonDigit (manyChars (nonDigit <|> digit <?> "")) (
            fun pos c s -> Ast.Ident(c.ToString() + s, int pos.Line, int pos.Column))
        let p = p >>= (fun s -> if Builtin.keywords.Contains(s.Name) then fail "ident is a keyword" else preturn s)
        (p .>> ws) <?> "identifier"

    let opp = OperatorPrecedenceParser<_,_,_>()
    let exprNoCommaNoVerbatim = opp.ExpressionParser
    let exprNoComma = choice [exprNoCommaNoVerbatim; verbatim |>> Ast.VerbatimExp]
    let expr = sepBy1 exprNoComma (ch ',') |>> (List.reduce (fun acc e -> Ast.FunCall(Ast.Op ",", [acc;e])))

    let parenExp = between (ch '(') (ch ')') expr

    // Primitives
    let octal =
        let r = @"0[0-7]+"
        let conv s = System.Convert.ToInt64(s, 8) |> (fun x -> Ast.Int(x, ""))
        let body = regex r |>> conv
        body .>> ws

    let hexa =
        let prefix = pstring "0x" <|> pstring "0X"
        let r = @"([0-9a-fA-F])+"
        let conv s = System.Convert.ToInt64(s, 16) |> (fun x -> Ast.Int(x, ""))
        let body = regex r |>> conv
        prefix >>. body .>> ws

    let number =
        let r = @"(\d+\.?\d*|\.\d+)([eE][-+]?[0-9]+)?"
        let conv s =
            let ok, res = System.Int64.TryParse(s : string)
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
    do opp.TermParser <- simpleExpr

    // Operators

    let precedence = [
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
        ["="; "+="; "-="; "*="; "/="; "%="; "<<="; ">>="; "&="; "^="; "|="], Associativity.Right
    ]

    // Add all the operators in the OperatorParser
    let () =
        // we start with operators with the highest priority, then we decrement the counter.
        let mutable precCounter = 20 // we have at most 20 different precedence levels
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
        addInfix precedence
        // same precedence as =
        opp.AddOperator(TernaryOperator("?", ws, ":", ws, precCounter, Associativity.Right, fun x y z -> Ast.FunCall(Ast.Op "?:", [x; y; z])))

    let simpleStatement = opt expr |>> (function Some exp -> Ast.Expr exp | None -> Ast.Block [])
    let statement, stmtRef = createParserForwardedToRef()
    let declaration, declRef = createParserForwardedToRef()

    let keyword s = attempt (pstring s .>> notFollowedBy letter .>> notFollowedBy digit .>> notFollowedBy (ch '_')) .>> ws

    // A type block, like struct or interface blocks
    let blockSpecifier prefix =

        // Restriction on field names
        let check ((_,l) as arg : Ast.Decl) =
            for decl in l do
                if decl.name.Name <> options.renameField decl.name.Name then
                    failwithf "Record field name '%s' is not allowed by Shader Minifier,\nbecause it looks like a vec4 field name." decl.name.Name
            arg

        let decls = many (declaration .>> ch ';' |>> check)
        let name = opt ident

        let generic = ch '<' >>. manyCharsTill anyChar (ch '>')
                   |> opt
                   |>> (function Some s -> $"<{s}>" | None -> "")
        let baseClassName = pipe2 (ch ':' >>. ident |>> (fun id -> id.Name)) generic (+)
                            |> opt
        pipe4 name (opt generic) baseClassName (between (ch '{') (ch '}') decls)
            (fun n t c d ->
                Option.iter (fun (i:Ast.Ident) -> forbiddenNames <- i.Name::forbiddenNames) n
                { prefix = prefix; name = n; template = t; baseClass = c; fields = d }: Ast.StructOrInterfaceBlock)

    let structSpecifier = parse {
        let! str = keyword "struct"
        let! res = blockSpecifier str
        return res
    }

    let structDecl =
        let semi = if options.hlsl then opt (ch ';') |>> ignore<unit option> else ch ';'
        (structSpecifier .>> semi) |>> Ast.TypeDecl

    // eg. "const out int", "uniform float", "int[2][3]"
    let glslStorage = ["const"; "inout"; "in"; "out"; "centroid"
                       "patch"; "sample"; "uniform"; "buffer"; "shared"; "coherent"
                       "volatile"; "restrict"; "readonly"; "writeonly"; "subroutine"
                       "attribute"; "varying"
                       "highp"; "mediump"; "lowp"
                       "invariant"; "precise"
                       "smooth"; "flat"; "noperspective"
                      ]
                      |> List.map keyword |> choice <?> "Type qualifier"
    let glslLayout = keyword "layout" >>. ch '(' >>. manyCharsTill anyChar (ch ')')
                      |>> (function s -> "layout(" + s + ")")
    let glslQualifier = many (glslStorage <|> glslLayout)
    let specifiedTypeGLSL =
        let typeSpec = (structSpecifier |>> Ast.TypeBlock) <|> (ident |>> (fun id -> Ast.TypeName id.Name))
        let arraySizes = many (between (ch '[') (ch ']') expr)
        pipe3 glslQualifier typeSpec arraySizes (fun tyQ name sizes -> Ast.makeType name tyQ sizes)

    let hlslStorage = ["extern"; "nointerpolation"; "precise"; "shared"; "groupshared"
                       "static"; "uniform"; "volatile"; "const"; "row_major"; "column_major"
                       "inline"; "target"
                       "out"; "in"; "inout"
                       "linear"; "centroid"; "nointerpolation"; "noperspective"; "sample"
                       "cbuffer"; "tbuffer"
                       // https://docs.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-geometry-shader
                       "point"; "line"; "triangle"; "lineadj"; "triangleadj"
                      ]
                      |> List.map keyword |> choice <?> "Type qualifier"
    let hlslQualifier = many hlslStorage
    let specifiedTypeHLSL =
        let generic = ch '<' >>. manyCharsTill anyChar (ch '>')
                   |> opt
                   |>> (function Some s -> "<" + s + ">" | None -> "")
        let typeName = pipe2 (ident |>> (fun id -> id.Name)) generic (+)
        let typeSpec = (structSpecifier |>> Ast.TypeBlock) <|> (typeName |>> Ast.TypeName)
        let arraySizes = many (between (ch '[') (ch ']') expr)
        pipe4 hlslQualifier typeSpec generic arraySizes (fun tyQ name _ sizes -> Ast.makeType name tyQ sizes)

    let qualifier = parse {
        let! ret = if options.hlsl then hlslQualifier else glslQualifier
        return ret
    }
    let specifiedType = parse {
        let! ret = if options.hlsl then specifiedTypeHLSL else specifiedTypeGLSL
        return ret
    }

    // For HLSL, e.g. ": color"
    let semantics =
        many (ch ':' >>. simpleExpr)

    // eg. "int foo[] = exp, bar = 3"
    do declRef.Value <- (
        let bracket = between (ch '[') (ch ']') (opt expr) |>> (fun size -> defaultArg size (Ast.Int (0, "")))
        let init = ch '=' >>. exprNoComma
        let var = pipe4 ident (opt bracket) semantics (opt init) Ast.makeDecl
        let list = sepBy1 var (ch ',')
        tuple2 specifiedType list
    )

    // e.g. int foo[]   used for function arguments
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
        // semicolon seems to be optional in hlsl
        do! if options.hlsl then opt (ch ';') |>> ignore<unit option> else ch ';'
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

    let macro =
        let nl = skipComment >>. skipMany (pchar '\\' >>. newline)
        let line = manyCharsTill (anyChar .>> nl) newline
        // an ident, without eating trailing spaces
        let ident = manyChars (pchar '_' <|> asciiLetter <|> digit)
        // parse the #define macros to get the macro name
        let define = pipe2 (keyword "define" >>. ident) line
                       (fun id line -> forbiddenNames <- id :: forbiddenNames; ["#define"; id; line])
        let otherDirective = line |>> (fun s -> ["#" + s])
        pchar '#' >>. (define <|> otherDirective) .>> ws

    // HLSL attribute, eg. [maxvertexcount(12)]
    let attribute =
        if options.hlsl then
            ch '[' >>. manyCharsTill anyChar (ch ']')
                |>> (function s -> "[" + s + "]")
        else
            pzero

    // HLSL template, e.g. template<typename T>
    let template =
        if options.hlsl then
            str "template" >>. ch '<' >>. manyCharsTill anyChar (ch '>')
                |>> (function s -> $"template<{s}>")
        else
            pzero

    let jump =
        let key =
            choice [keyword "break"; keyword "continue"; keyword "discard"]
              |>> (fun k -> Ast.Jump(Ast.JumpKeyword.fromString k, None))

        let ret = pipe2 (keyword "return") (opt expr) (fun k e -> Ast.Jump(Ast.JumpKeyword.fromString k, e))
        (key <|> ret) .>> ch ';'

    // A statement
    do stmtRef.Value <- choice [
        template |>> Ast.Verbatim
        block
        jump
        forLoop
        ifStatement
        whileLoop
        doWhileLoop
        switch
        verbatim |>> Ast.Verbatim
        macro |>> Ast.Directive
        attribute |>> Ast.Verbatim
        attempt ((declaration .>> ch ';') |>> Ast.Decl)
        simpleStatement .>> ch ';'] <?> "statement"

    // e.g. "int foo(float a[], out int b) : color"
    let functionHeader =
        let void_ = keyword "void" |>> (fun _ -> [])
        let argList = void_ <|> (sepBy singleDeclaration (ch ','))
        let argList = between (ch '(') (ch ')') argList
        pipe4 specifiedType ident argList semantics Ast.makeFunctionType

    let pfunction =
        pipe2 functionHeader block (fun head body -> Ast.Function(head, body))

    let precision =
         keyword "precision" >>. (specifiedType |>> Ast.Precision) .>> ch ';'

    let loneLayoutQualifier =
        qualifier .>> ch ';' |>> (fun list -> String.concat " " list + ";")

    let toplevel =
        let decl = declaration .>> ch ';'
        let item = choice [
                    pipe2 macro getPosition (fun ss pos -> Ast.TLDirective (ss, {line = int pos.Line; col = int pos.Column}))
                    template |>> Ast.TLVerbatim
                    verbatim |>> Ast.TLVerbatim
                    attribute |>> Ast.TLVerbatim
                    attempt decl |>> Ast.TLDecl
                    structDecl
                    attempt interfaceBlock
                    attempt loneLayoutQualifier |>> Ast.TLVerbatim
                    precision
                    pfunction
        ]
        let forwardDecl = functionHeader .>> ch ';' |>> (fun _ -> reorderFunctions <- true)
        many ((attempt forwardDecl|>>fun _ -> None) <|> (item|>>Some)) |>> List.choose id // FIXME: use skip?

    let parse = ws >>. toplevel .>> eof

    member _.runParser streamName content : Ast.Shader =
        forbiddenNames <- [ "if"; "in"; "do" ]
        reorderFunctions <- false
        let content =
            if options.preprocess then Preprocessor.preprocess streamName content else content
        let res = runParserOnString parse () streamName content
        match res with
        | Success(r,_,_) -> {
            Ast.Shader.filename = streamName
            code = r
            forbiddenNames = forbiddenNames
            reorderFunctions = reorderFunctions
          }
        | Failure(str, _, _) -> failwithf "Parse error: %s" str

let runParser options = ParseImpl(options).runParser
