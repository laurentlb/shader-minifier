module internal NameModel

open System
open System.Collections.Generic

// Number of previous characters used to predict a character.
let private contextSize = 3

// A model of the minified text, used to choose names that a compressor can predict.
// In the source, each identifier is represented by a unique placeholder character. The model
// counts the n-grams of the text, ignoring those with placeholders. When a name is assigned to
// an identifier, the placeholders are replaced and the new n-grams are counted.
type Model(source: string, isPlaceholder: char -> bool) =
    let text = source.ToCharArray()
    let counts = Dictionary<uint64, int>() // of each n-gram
    let contextCounts = Dictionary<uint64, int>() // of each n-gram that is followed by a non-placeholder
    let positions = // of each placeholder, in increasing order
        dict [for c, li in source |> Seq.indexed |> Seq.groupBy snd do
                if isPlaceholder c then c, [|for pos, _ in li -> pos|]]

    let positionsOf placeholder =
        match positions.TryGetValue placeholder with
        | true, li -> li
        | false, _ -> [||]

    // Key for the text between two positions (included). It's empty if last < first.
    let ngram first last =
        let mutable key = 1UL // so that the key depends on the length
        for i in first .. last do
            key <- (key <<< 8) ||| (uint64 text[i] &&& 0xFFUL)
        key

    let count (counts: Dictionary<uint64, int>) ngram =
        match counts.TryGetValue ngram with
        | true, n -> n
        | false, _ -> 0

    // Number of usable characters before this position.
    let contextLength pos =
        let mutable length = 0
        while length < contextSize && pos - length > 0 && not (isPlaceholder text[pos - length - 1]) do
            length <- length + 1
        length

    // Count the n-grams that end at `last`, and that start at `maxFirst` or before.
    let addNgrams last maxFirst =
        if not (isPlaceholder text[last]) then
            for first in last - contextLength last .. maxFirst do
                let context, key = ngram first (last - 1), ngram first last
                contextCounts[context] <- count contextCounts context + 1
                counts[key] <- count counts key + 1

    // Probability of the character at this position. The estimation starts with the uniform
    // distribution and it's refined with longer and longer contexts.
    let probability pos =
        let mutable p = 1. / 96. // number of printable characters
        for length in 0 .. contextLength pos do
            let context = ngram (pos - length) (pos - 1)
            let contextAndChar = ngram (pos - length) pos
            p <- (float (count counts contextAndChar) + p) / (float (count contextCounts context) + 1.)
        p

    // Last position with a context that includes `pos`.
    let lastAffected pos = min (pos + contextSize) (text.Length - 1)

    do
        for pos in 0 .. text.Length - 1 do
            addNgrams pos pos

    // Estimates how many bits are needed to encode the uses of the identifier, and the characters that follow them.
    let cost placeholder name =
        let positions = positionsOf placeholder
        for pos in positions do
            text[pos] <- Seq.last name
        let mutable bits = 0.
        for i in 0 .. positions.Length - 1 do
            let nextUse = if i + 1 < positions.Length then positions[i + 1] else text.Length
            for pos in positions[i] .. min (lastAffected positions[i]) (nextUse - 1) do
                if not (isPlaceholder text[pos]) then
                    bits <- bits - Math.Log2(probability pos)
        for pos in positions do
            text[pos] <- placeholder
        bits

    let assign placeholder name =
        for pos in positionsOf placeholder do
            text[pos] <- Seq.last name
            for last in pos .. lastAffected pos do
                addNgrams last pos

    // Choose the best name for the identifier, and update the model.
    // For simplicity, only the last character of long names is considered. Long names are only used as a
    // last resort, so this doesn't matter much.
    // Due to the update, variables in two identical functions may get different names. Compression
    // tests show that (on average) it's worth it.
    member _.ChooseName(placeholder: char, candidates: string list) =
        // If the costs are equal, pick the same name consistently.
        let name = candidates |> List.minBy (fun name -> cost placeholder name, name)
        assign placeholder name
        name
