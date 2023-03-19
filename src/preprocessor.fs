module Preprocessor

open FParsec.Primitives
open FParsec.CharParsers
open FParsec

open System.Collections.Generic

type Impl() =
    // Dict of macro name to value
    let defines = new Dictionary<string, string>()

    // The stack contains information for each nested #if/#ifdef block
    //   Some true: the condition was true, keep the text
    //   Some false: the condition was false, delete the text
    //   None: the condition was not evaluated, keep the text and the #end
    let stack = new Stack<bool option>()

    let currentScope () =
        if stack.Count = 0 then None
        else stack.Peek()

    let enterScope evaluated =
        let current = currentScope ()
        if current = Some false then
            stack.Push current
        else
            stack.Push evaluated

    let keyword s = attempt (pstring s .>> notFollowedBy letter .>> notFollowedBy digit .>> notFollowedBy (pchar '_')) .>> spaces

    let parseIdent =
        manyChars (choice [letter; digit])

    let parseArgs = parse {
        let! _ = pchar '('
        let! _ = pchar ')'
        return "(args)"
    }

    let parseEndLine = manyCharsTill anyChar (followedBy newline) .>> newline

    let parseDefine = parse {
        let! _ = keyword "define"
        let! name = parseIdent
        let! args = opt parseArgs // TODO
        do! spaces
        let! line = parseEndLine
        defines.Add(name, line)
        return sprintf "#define %s %s" name line
    }

    let parseIfdef = parse {
        let! word = keyword "ifdef" <|> keyword "ifndef"
        let! ident = parseIdent
        do if defines.ContainsKey ident = (word = "ifdef") then
             enterScope (Some true)
           else
             enterScope (Some false)
        return ""
    }

    let parseEndif = parse {
        let! _ = keyword "endif"
        let state = stack.Pop()
        return if state = None then
                "#endif"
            else
                ""
    }

    let parseText = parse {
        let! line = parseEndLine
        return
            if currentScope() = Some false then ""
            else line
    }

    let parseOther = parseEndLine |>> (fun s -> "#" + s)

    let directive = pchar '#' >>. spaces >>. choice [parseDefine; parseIfdef; parseEndif; parseOther]

    member _.Parse = many (directive <|> parseText)

let preprocess streamName content =
    let impl = new Impl()
    let res = runParserOnString impl.Parse () streamName content
    match res with
        | Success(s,_,_) ->
            // printfn "%s\n-------------------------" (String.concat "\n" s)
            String.concat "\n" s
        | Failure(str, _, _) -> failwithf "Parse error: %s" str
