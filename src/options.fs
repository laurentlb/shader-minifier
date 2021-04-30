module Options

open System.Collections.Generic
open System.IO

let version = "1.1.6" // Shader Minifer version
let debugMode = false

type TargetOutput = Text | CHeader | CList | JS | Nasm
let text() = Text

type Options() =
    let mutable _canonicalFieldNames = "xyzw"
    member val outputName = "shader_code.h" with get, set
    member val targetOutput = CHeader with get, set
    member val verbose = false with get, set
    member val smoothstepTrick = false with get, set
    member this.canonicalFieldNames with get () = _canonicalFieldNames
    member this.trySetCanonicalFieldNames(s: string) =
        if not (s = "rgba" || s = "xyzw" || s = "stpq" || s = "") then
            false
        else
            _canonicalFieldNames <- s
            true
    member val preserveExternals = false with get, set
    member val preserveAllGlobals = false with get, set 
    member val reorderDeclarations = false with get, set
    member val reorderFunctions = false with get, set
    member val hlsl = false with get, set
    member val noSequence = false with get, set
    member val noRenaming = false with get, set
    member val noRenamingList = ["main"] with get, set
    member val filenames = List<string>() with get, set

module Globals =
    let options = Options()
