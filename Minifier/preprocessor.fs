module internal Preprocessor

// A partial preprocessor: it expands the macros, evaluates the conditions (#if, #ifdef...)
// and removes the dead code. When a condition cannot be evaluated (e.g. "#ifdef GL_ES"),
// the directive is kept in the output and both branches are preprocessed.

open System
open System.Collections.Generic
open System.Text
open System.Text.RegularExpressions

let private error msg = raise (Options.ParseError msg)

let private isDigit c = c >= '0' && c <= '9'
let private isIdentStart c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let private isIdentChar c = isIdentStart c || isDigit c

// Names defined by the implementation (e.g. GL_ES, __VERSION__) have an unknown value.
let private isBuiltinName (name: string) = name.StartsWith "GL_" || name.StartsWith "__"

// Split "foo bar" into ("foo", " bar"). Return "" if the string doesn't start with an identifier.
let private splitIdent (s: string) =
    if s.Length = 0 || not (isIdentStart s[0]) then "", s
    else
        let mutable i = 1
        while i < s.Length && isIdentChar s[i] do i <- i + 1
        s.Substring(0, i), s.Substring(i)

let private normalizeSpaces (s: string) = Regex.Replace(s.Trim(), @"\s+", " ")

let private countNewlines (s: string) = s |> Seq.filter (fun c -> c = '\n') |> Seq.length

// Tokens

type private TokenKind = Ident | Number | Punct | Space | Comment

type private Token = {
    kind: TokenKind
    text: string
    // Macros that must not be expanded in this token (to stop recursive expansions).
    hide: Set<string>
}

let private mkToken kind text = { kind = kind; text = text; hide = Set.empty }

let private tokenize (s: string) =
    let tokens = ResizeArray<Token>()
    let mutable i = 0
    let at k c = k < s.Length && s[k] = c
    while i < s.Length do
        let start = i
        let c = s[i]
        let kind =
            if c = '/' && at (i + 1) '/' then
                while i < s.Length && s[i] <> '\n' do i <- i + 1
                Comment
            elif c = '/' && at (i + 1) '*' then
                let close = s.IndexOf("*/", i + 2)
                i <- if close < 0 then s.Length else close + 2
                Comment
            elif Char.IsWhiteSpace c then
                while i < s.Length && Char.IsWhiteSpace s[i] do i <- i + 1
                Space
            elif isIdentStart c then
                while i < s.Length && isIdentChar s[i] do i <- i + 1
                Ident
            elif isDigit c || (c = '.' && i + 1 < s.Length && isDigit s[i + 1]) then
                // A "preprocessing number", e.g. 1.5e-3
                i <- i + 1
                let mutable go = true
                while go && i < s.Length do
                    let c = s[i]
                    if (c = '+' || c = '-') && "eEpP".Contains(s[i - 1]) then i <- i + 1
                    elif isIdentChar c || c = '.' then i <- i + 1
                    else go <- false
                Number
            else
                i <- i + 1
                Punct
        tokens.Add(mkToken kind (s.Substring(start, i - start)))
    List.ofSeq tokens

let private toText (tokens: Token list) = tokens |> List.map (fun t -> t.text) |> String.concat ""

let private isBlank t = t.kind = Space || t.kind = Comment

let private skipBlanks tokens = List.skipWhile isBlank tokens

let private trimBlanks tokens = tokens |> skipBlanks |> List.rev |> skipBlanks |> List.rev

// Surround tokens with spaces, to avoid accidentally merging them with their neighbors (e.g. "-" and "-1").
let private spaced tokens =
    let space = mkToken Space " "
    space :: tokens @ [space]

// Parse the arguments of a macro call, e.g. "(a, f(b, c))". Return the list of arguments
// and the remaining tokens, or None if there is no call.
let private parseArgs (tokens: Token list) =
    match skipBlanks tokens with
    | { kind = Punct; text = "(" } :: rest ->
        let args = ResizeArray<Token list>()
        let current = ResizeArray<Token>()
        let endArg () =
            args.Add(trimBlanks (List.ofSeq current))
            current.Clear()
        let rec loop depth tokens =
            match tokens with
            | [] -> None
            | (t: Token) :: rest ->
                match t.kind, t.text with
                | Punct, ")" when depth = 0 ->
                    endArg ()
                    Some (List.ofSeq args, rest)
                | Punct, "," when depth = 0 ->
                    endArg ()
                    loop depth rest
                | Punct, "(" -> current.Add t; loop (depth + 1) rest
                | Punct, ")" -> current.Add t; loop (depth - 1) rest
                | _ -> current.Add t; loop depth rest
        loop 0 rest
    | _ -> None

// Evaluation of #if expressions. The value is None when it's unknown.

exception private EvalError

let private exprTokens (s: string) =
    let twoCharOps = set ["||"; "&&"; "=="; "!="; "<="; ">="; "<<"; ">>"]
    let res = ResizeArray<string>()
    let mutable i = 0
    while i < s.Length do
        let c = s[i]
        if Char.IsWhiteSpace c then
            i <- i + 1
        elif isIdentChar c || c = '.' then
            let start = i
            while i < s.Length && (isIdentChar s[i] || s[i] = '.') do i <- i + 1
            res.Add(s.Substring(start, i - start))
        elif i + 1 < s.Length && twoCharOps.Contains(s.Substring(i, 2)) then
            res.Add(s.Substring(i, 2))
            i <- i + 2
        else
            res.Add(string c)
            i <- i + 1
    res.ToArray()

let private parseNumber (t: string) =
    let t = t.TrimEnd('u', 'U', 'l', 'L')
    try
        if t.StartsWith "0x" || t.StartsWith "0X" then Convert.ToInt64(t.Substring 2, 16)
        elif t.Length > 1 && t[0] = '0' then Convert.ToInt64(t.Substring 1, 8)
        else Int64.Parse(t, Globalization.NumberStyles.None, Globalization.CultureInfo.InvariantCulture)
    with _ -> raise EvalError

type private Evaluator(tokens: string[], identValue: string -> int64 option) =
    let mutable pos = 0
    let peek () = if pos < tokens.Length then tokens[pos] else ""
    let advance () = pos <- pos + 1
    let expect s = if peek () = s then advance () else raise EvalError

    let ofBool b = if b then 1L else 0L
    let arith f a b = match a, b with Some a, Some b -> Some (f a b) | _ -> None
    let divide f a b = match a, b with Some _, Some 0L -> None | _ -> arith f a b

    let binary op a b =
        match op with
        // Some values are known even if one side is unknown, e.g. "x || 1".
        | "||" ->
            match a, b with
            | Some x, _ when x <> 0L -> Some 1L
            | _, Some y when y <> 0L -> Some 1L
            | Some _, Some _ -> Some 0L
            | _ -> None
        | "&&" ->
            match a, b with
            | Some 0L, _ | _, Some 0L -> Some 0L
            | Some _, Some _ -> Some 1L
            | _ -> None
        | "|" -> arith (|||) a b
        | "^" -> arith (^^^) a b
        | "&" -> arith (&&&) a b
        | "==" -> arith (fun a b -> ofBool (a = b)) a b
        | "!=" -> arith (fun a b -> ofBool (a <> b)) a b
        | "<" -> arith (fun a b -> ofBool (a < b)) a b
        | ">" -> arith (fun a b -> ofBool (a > b)) a b
        | "<=" -> arith (fun a b -> ofBool (a <= b)) a b
        | ">=" -> arith (fun a b -> ofBool (a >= b)) a b
        | "<<" -> arith (fun a b -> a <<< int b) a b
        | ">>" -> arith (fun a b -> a >>> int b) a b
        | "+" -> arith (+) a b
        | "-" -> arith (-) a b
        | "*" -> arith (*) a b
        | "/" -> divide (/) a b
        | "%" -> divide (%) a b
        | _ -> raise EvalError

    // Binary operators, from the lowest to the highest precedence.
    let levels = [|
        ["||"]; ["&&"]; ["|"]; ["^"]; ["&"]; ["=="; "!="]; ["<"; ">"; "<="; ">="]
        ["<<"; ">>"]; ["+"; "-"]; ["*"; "/"; "%"] |]

    let rec ternary () =
        let cond = binaryLevel 0
        if peek () = "?" then
            advance ()
            let a = ternary ()
            expect ":"
            let b = ternary ()
            match cond with
            | Some 0L -> b
            | Some _ -> a
            | None -> if a = b then a else None
        else cond

    and binaryLevel n =
        if n = levels.Length then unary ()
        else
            let mutable left = binaryLevel (n + 1)
            while List.contains (peek ()) levels[n] do
                let op = peek ()
                advance ()
                left <- binary op left (binaryLevel (n + 1))
            left

    and unary () =
        match peek () with
        | "!" -> advance (); unary () |> Option.map (fun x -> ofBool (x = 0L))
        | "-" -> advance (); unary () |> Option.map (~-)
        | "+" -> advance (); unary ()
        | "~" -> advance (); unary () |> Option.map (~~~)
        | _ -> primary ()

    and primary () =
        let t = peek ()
        advance ()
        if t = "(" then
            let v = ternary ()
            expect ")"
            v
        elif t = "defined" then
            // The known "defined" were replaced before evaluation, this one is unknown.
            if peek () = "(" then
                advance ()
                advance ()
                expect ")"
            else advance ()
            None
        elif t.Length > 0 && isDigit t[0] then Some (parseNumber t)
        elif t.Length > 0 && isIdentStart t[0] then identValue t
        else raise EvalError

    member _.Evaluate() =
        let v = ternary ()
        if pos <> tokens.Length then raise EvalError
        v

let private evaluate identValue (s: string) =
    try Evaluator(exprTokens s, identValue).Evaluate()
    with EvalError -> None

// Splitting the input into directives and text

type private Item =
    | Directive of text: string * lines: int // text after the '#', without comments and line continuations
    | Text of string

let private splitItems (content: string) =
    let items = ResizeArray<Item>()
    let text = StringBuilder()
    let flushText () =
        if text.Length > 0 then
            items.Add(Text (text.ToString()))
            text.Clear() |> ignore
    let at k (s: string) = String.CompareOrdinal(content, k, s, 0, s.Length) = 0
    let mutable inComment = false
    let mutable i = 0
    while i < content.Length do
        // We are at the beginning of a line.
        let mutable k = i
        while k < content.Length && (content[k] = ' ' || content[k] = '\t') do k <- k + 1
        if not inComment && at k "#" then
            flushText ()
            let directive = StringBuilder()
            let mutable lines = 0
            k <- k + 1
            let mutable go = true
            while go && k < content.Length do
                if content[k] = '\n' then
                    lines <- lines + 1
                    k <- k + 1
                    go <- false
                elif at k "\\\n" || at k "\\\r\n" then
                    lines <- lines + 1
                    k <- content.IndexOf('\n', k) + 1
                    directive.Append(' ') |> ignore
                elif at k "//" then
                    while k < content.Length && content[k] <> '\n' do k <- k + 1
                elif at k "/*" then
                    let close = content.IndexOf("*/", k + 2)
                    let close = if close < 0 then content.Length else close + 2
                    lines <- lines + countNewlines (content.Substring(k, close - k))
                    k <- close
                    directive.Append(' ') |> ignore
                else
                    directive.Append(content[k]) |> ignore
                    k <- k + 1
            items.Add(Directive (directive.ToString(), lines))
        else
            // A line of code. Track the block comments, as they may contain a '#'.
            k <- i
            while k < content.Length && content[k] <> '\n' do
                if inComment then
                    if at k "*/" then
                        inComment <- false
                        k <- k + 2
                    else k <- k + 1
                elif at k "//" then
                    while k < content.Length && content[k] <> '\n' do k <- k + 1
                elif at k "/*" then
                    inComment <- true
                    k <- k + 2
                else k <- k + 1
            if k < content.Length then k <- k + 1
            text.Append(content, i, k - i) |> ignore
        i <- k
    flushText ()
    items

// Preprocessing

type private Macro = {
    parameters: string list option // None for object-like macros
    body: Token list
}

type private MacroState =
    | Defined of Macro
    | Unknown // e.g. the macro was defined in a region that may or may not be compiled

// Whether some code is compiled: Maybe when it depends on an unknown condition.
type private Status = Yes | No | Maybe

// Information about an #if/#elif/#else chain.
type private Frame = {
    parent: Status // status of the enclosing region
    mutable taken: Status // whether a previous branch of the chain is compiled
    mutable current: Status // whether the current branch is compiled
    mutable emitted: bool // whether a directive was printed for this chain
    mutable seenElse: bool
}

type private Impl(cmdDefines: (string * string) list, noExpand: Set<string>) =

    let defines = Dictionary<string, MacroState>()
    do for name, value in cmdDefines do
        defines[name] <- Defined { parameters = None; body = trimBlanks (tokenize value) }

    // Macros that were modified in a region that may or may not be compiled.
    let unstable = HashSet<string>()

    let stack = Stack<Frame>()
    let out = StringBuilder()
    let mutable line = 1

    let fail msg = error (sprintf "line %d: %s" line msg)

    let status () =
        if stack.Count = 0 then Yes
        else
            let frame = stack.Peek()
            match frame.parent, frame.current with
            | No, _ | _, No -> No
            | Yes, Yes -> Yes
            | _ -> Maybe

    let canOutput name = not (noExpand.Contains name)

    let isDefined name =
        match defines.TryGetValue name with
        | true, Defined _ -> Yes
        | true, Unknown -> Maybe
        | _ -> if isBuiltinName name then Maybe else No

    // The value of an identifier that remains after macro expansion.
    let identValue name =
        match defines.TryGetValue name with
        | true, Unknown -> None
        | true, Defined _ -> Some 0L // e.g. a recursive macro
        | _ -> if isBuiltinName name then None else Some 0L

    // Expand the macros for which canExpand returns true.
    let rec expand canExpand (tokens: Token list) =
        let res = ResizeArray<Token>()
        let mutable input = tokens
        while not input.IsEmpty do
            let tok = input.Head
            input <- input.Tail
            let macro =
                if tok.kind = Ident && not (tok.hide.Contains tok.text) && canExpand tok.text then
                    match defines.TryGetValue tok.text with
                    | true, Defined m -> Some m
                    | _ -> None
                else None
            // The result of an expansion is put back in the input, to be scanned again.
            let pushBack tokens rest =
                let hide = tok.hide.Add tok.text
                input <- spaced (tokens |> List.map (fun t -> { t with hide = Set.union t.hide hide })) @ rest
            match macro with
            | None -> res.Add tok
            | Some { parameters = None; body = body } -> pushBack body input
            | Some { parameters = Some ps; body = body } ->
                match parseArgs input with
                | None -> res.Add tok // the macro is not called
                | Some (args, rest) ->
                    let args = if ps.IsEmpty && args = [[]] then [] else args
                    if args.Length <> ps.Length then
                        fail (sprintf "macro '%s' expects %d arguments, got %d" tok.text ps.Length args.Length)
                    let args = Map (List.zip ps (args |> List.map (expand canExpand)))
                    let body = body |> List.collect (fun t ->
                        match t.kind, args.TryFind t.text with
                        | Ident, Some arg -> spaced arg
                        | _ -> [t])
                    pushBack body rest
        List.ofSeq res

    // Replace "defined X" and "defined(X)" when the value is known.
    let rec replaceDefined (tokens: Token list) =
        match tokens with
        | [] -> []
        | { kind = Ident; text = "defined" } :: rest ->
            let name, rest =
                match skipBlanks rest with
                | { kind = Punct; text = "(" } :: rest ->
                    match skipBlanks rest with
                    | { kind = Ident; text = name } :: rest ->
                        match skipBlanks rest with
                        | { kind = Punct; text = ")" } :: rest -> name, rest
                        | _ -> fail "missing ')' after 'defined'"
                    | _ -> fail "expected an identifier after 'defined'"
                | { kind = Ident; text = name } :: rest -> name, rest
                | _ -> fail "expected an identifier after 'defined'"
            let replacement =
                match isDefined name with
                | Yes -> [mkToken Number "1"]
                | No -> [mkToken Number "0"]
                | Maybe -> tokenize $"defined({name})" |> List.map (fun t -> { t with hide = Set.singleton name })
            spaced replacement @ replaceDefined rest
        | t :: rest -> t :: replaceDefined rest

    // Return the status of the condition, and a function that returns the condition to print.
    let evalCondition (text: string) =
        let tokens = replaceDefined (tokenize text)
        let value = expand (fun _ -> true) tokens |> toText |> evaluate identValue
        let status = match value with Some 0L -> No | Some _ -> Yes | None -> Maybe
        status, (fun () -> expand canOutput tokens |> toText |> normalizeSpaces)

    let emit (directive: string) = out.Append('#').Append(directive) |> ignore

    let newFrame () =
        let frame = { parent = status (); taken = No; current = No; emitted = false; seenElse = false }
        stack.Push frame
        frame

    let topFrame name =
        if stack.Count = 0 then fail $"#{name} without #if"
        stack.Peek()

    // Process a branch of an #if chain. eval returns the status of the branch, and a function
    // that returns the directive to print if the branch opens the chain, or if it continues it.
    let branch (frame: Frame) (eval: unit -> Status * (unit -> string * string)) =
        if frame.parent = No || frame.taken = Yes then
            frame.current <- No
        else
            let cond, directive = eval ()
            match frame.taken, cond with
            | _, No -> frame.current <- No
            | No, Yes ->
                frame.current <- Yes
                frame.taken <- Yes
            | No, Maybe ->
                emit (fst (directive ()))
                frame.emitted <- true
                frame.current <- Maybe
                frame.taken <- Maybe
            | _, Yes ->
                emit "else"
                frame.current <- Maybe
                frame.taken <- Yes
            | _, Maybe ->
                emit (snd (directive ()))
                frame.current <- Maybe

    let condition (text: string) () =
        let status, cond = evalCondition text
        status, (fun () -> let c = cond () in "if " + c, "elif " + c)

    let define (text: string) =
        let name, after = splitIdent (text.TrimStart())
        if name = "" then fail "invalid #define"
        let parameters, body =
            // A '(' immediately after the name starts a function-like macro.
            if after.StartsWith "(" then
                let close = after.IndexOf ')'
                if close < 0 then fail "missing ')' in macro parameters"
                let ps = after.Substring(1, close - 1).Split(',') |> Array.map (fun s -> s.Trim()) |> List.ofArray
                let ps = if ps = [""] then [] else ps
                for p in ps do
                    if p = "" || fst (splitIdent p) <> p then fail $"invalid macro parameter '{p}'"
                Some ps, after.Substring(close + 1)
            else None, after
        let macro = { parameters = parameters; body = trimBlanks (tokenize body) }
        let status = status ()
        if status = Maybe then
            defines[name] <- Unknown
            unstable.Add name |> ignore
        else
            defines[name] <- Defined macro
        if status = Maybe || noExpand.Contains name then
            let ps = defaultArg parameters []
            let body = expand (fun n -> canOutput n && not (List.contains n ps)) macro.body |> toText
            let ps = match parameters with Some ps -> "(" + String.concat "," ps + ")" | None -> ""
            emit (normalizeSpaces $"define {name}{ps} {body}")

    let undef (text: string) =
        let name, _ = splitIdent (text.TrimStart())
        if name = "" then fail "invalid #undef"
        let status = status ()
        if status = Maybe then
            defines[name] <- Unknown
            unstable.Add name |> ignore
        else
            defines.Remove name |> ignore
        if status = Maybe || noExpand.Contains name then
            emit ("undef " + name)

    let directive (text: string) =
        let name, rest = splitIdent (text.TrimStart())
        let live = status () <> No
        match name with
        | "define" -> if live then define rest
        | "undef" -> if live then undef rest
        | "if" -> branch (newFrame ()) (condition rest)
        | "ifdef" | "ifndef" ->
            branch (newFrame ()) (fun () ->
                let ident, _ = splitIdent (rest.TrimStart())
                if ident = "" then fail $"expected an identifier after #{name}"
                let status =
                    match isDefined ident, name with
                    | Yes, "ifndef" -> No
                    | No, "ifndef" -> Yes
                    | status, _ -> status
                status, (fun () -> let d = $"{name} {ident}" in d, d))
        | "elif" ->
            let frame = topFrame name
            if frame.seenElse then fail "#elif after #else"
            branch frame (condition rest)
        | "else" ->
            let frame = topFrame name
            if frame.seenElse then fail "#else after #else"
            frame.seenElse <- true
            branch frame (fun () -> Yes, (fun () -> "else", "else"))
        | "endif" ->
            let frame = topFrame name
            stack.Pop() |> ignore
            if frame.parent <> No && frame.emitted then emit "endif"
        | "line" -> ()
        | "extension" when fst (splitIdent (rest.TrimStart())) = "GL_GOOGLE_include_directive" -> ()
        | "" -> () // It is valid to have '#' alone on a line. This is a no op.
        | _ -> if live then emit (text.Trim()) // Unknown directive, keep it.

    let textChunk (text: string) =
        let lines = countNewlines text
        if status () = No then
            out.Append('\n', lines) |> ignore
        else
            let result = expand canOutput (tokenize text) |> toText
            out.Append(result) |> ignore
            // Keep the line numbers when a macro call spans multiple lines.
            let missing = lines - countNewlines result
            if missing > 0 then out.Append('\n', missing) |> ignore
        line <- line + lines

    member _.Run(content: string) =
        for item in splitItems content do
            match item with
            | Text text -> textChunk text
            | Directive (text, lines) ->
                directive text
                out.Append('\n', lines) |> ignore
                line <- line + lines
        if stack.Count > 0 then fail "missing #endif"
        out.ToString()

    member _.Unstable = Set.ofSeq unstable

let preprocess (options: Options.Options) streamName content =
    let run noExpand =
        let impl = Impl(options.preprocessDefines, noExpand)
        let result = impl.Run content
        result, impl.Unstable
    try
        let noExpand = set options.preprocessNoExpand
        let result, unstable = run noExpand
        // Macros modified in a region that may or may not be compiled cannot be expanded.
        // Run again, keeping all their definitions.
        if Set.isSubset unstable noExpand then result
        else fst (run (Set.union noExpand unstable))
    with Options.ParseError msg -> raise (Options.ParseError $"{streamName}: {msg}")
