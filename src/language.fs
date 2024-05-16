module Language

type Language() =
    member val hlsl = false with get, set
    member val dynamicloop = true with get, set
    member val implicitconv = false with get, set

module Globals =
    let mutable language = Language()
    let LanguageHLSL _ =
        Language (hlsl = true)
    let LanguageGLSL ver =
        Language (implicitconv = true)
    let LanguageESSL ver =
        Language (dynamicloop = (ver>=300))
