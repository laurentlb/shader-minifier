module internal Preprocessor

open FParsec.Primitives
open FParsec.CharParsers
open FParsec

open System.Collections.Generic

// The stack contains information for each nested #if/#ifdef block
//   Active: condition was true, lines are printed (e.g. #if 1)
//   Inactive: condition was false, delete the text (e.g. #if 0)
//   Unknown: condition could not be evaluated, keep both the directive and the text
type private Status = Active | Inactive | Unknown

type private Impl() =

    // Dict of macro name to value
    let defines = Dictionary<string, string>()

    let stack = Stack<Status>()

    let currentStatus () =
        if stack.Count = 0 then Active
        else stack.Peek()

    let enterScope evaluated =
        let current = currentStatus ()
        if current = Inactive then
            stack.Push Inactive
        else
            stack.Push evaluated

    let spaces = skipManySatisfy (function ' ' | '\t' -> true | _ -> false)

    let keyword s = attempt (pstring s .>> notFollowedBy letter .>> notFollowedBy digit .>> notFollowedBy (pchar '_')) .>> spaces

    let parseIdent =
        manyChars (choice [letter; digit; pchar '_'])

    let parseEndLine = manyCharsTill anyChar (followedBy newline) .>> newline

    // TODO: ignore the defines when status is Inactive
    // TODO: mark defines as unknown when status is Unknown

    let parseLine = parse {
        let! _ = keyword "line"
        let! _ = parseEndLine
        return ""
    }

    let parseExtension = parse {
        let! _ = keyword "extension"
        let! name = parseIdent
        let! behavior = parseEndLine
        return
            match name with
            | "GL_GOOGLE_include_directive" -> ""
            | _ -> sprintf "#extension %s%s" name behavior
    }

    let parseDefine = parse {
        let! _ = keyword "define"
        let! name = parseIdent
        let! line = parseEndLine
        defines.Add(name, line)
        return sprintf "#define %s%s" name line
    }

    let parseUndef = parse {
        let! _ = keyword "undef"
        let! name = parseIdent
        defines.Remove(name) |> ignore<bool>
        return sprintf "#undef %s" name
    }

    let parseIfdef = parse {
        let! word = keyword "ifdef" <|> keyword "ifndef"
        let! ident = parseIdent
        do if defines.ContainsKey ident = (word = "ifdef") then
             enterScope Active
           else
             enterScope Inactive
        return ""
    }

    let evalCond (str: string) =
        match str.Trim() with
        | "0" -> Inactive
        | "1" -> Active
        | _ -> Unknown

    let parseIf = parse {
        let! _ = keyword "if"
        let! arg = parseEndLine
        let status = evalCond arg
        do enterScope status
        return if status = Unknown then "#if " + arg else ""
    }

    let parseElse = parse {
        let! _ = keyword "else"
        return
            match stack.Pop() with
            | Active -> stack.Push(Inactive); ""
            | Inactive -> stack.Push(Active); ""
            | Unknown -> stack.Push(Unknown); "#else"
    }

    let parseEndif = parse {
        let! _ = keyword "endif"
        let state = stack.Pop()
        return if state = Unknown then
                "#endif"
            else
                ""
    }

    let parseElif = parse {
        let! _ = keyword "elif"
        let! arg = parseEndLine
        let oldStatus = stack.Pop()
        let cond = evalCond arg
        let newStatus =
            match oldStatus, cond with
            | Unknown, _ | _, Unknown -> Unknown
            | _, Inactive -> Inactive
            | Active, _ -> Inactive
            | Inactive, Active -> Active
        do stack.Push newStatus
        return if newStatus = Unknown then "#elif " + arg else ""
    }

    // It is valid to have '#' alone on a line. This is a no op.
    let parseNope = newline |>> (fun _ -> "\n")

    let parseText = parse {
        let! line = parseEndLine
        return
            if currentStatus() = Inactive then ""
            else line
    }

    // Unknown directive, keep it.
    let parseOther = parseEndLine |>> (fun s -> if currentStatus() = Inactive then "" else "#" + s)

    let directive = pchar '#' >>. spaces >>. choice [
        parseLine
        parseExtension
        parseDefine
        parseElif
        parseElse
        parseEndif
        parseIf
        parseIfdef
        parseNope
        parseUndef

        parseOther
    ]

    member _.Parse = many (directive <|> parseText)

let preprocess streamName content =
    let impl = Impl()
    let res = runParserOnString impl.Parse () streamName content
    match res with
        | Success(s,_,_) ->
            // printfn "%s\n-------------------------" (String.concat "\n" s)
            String.concat "\n" s
        | Failure(str, _, _) -> failwithf "Parse error: %s" str
