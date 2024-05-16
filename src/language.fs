module Language

type Language() =
    member val hlsl = false with get, set
    member val glslver = 0 with get, set
    member val esslver = 0 with get, set
    member val dynamicloop = false with get, set
    member val implicitconv = false with get, set

module Globals =
    let mutable language = Language()
    let LanguageHLSL ver =
      Language (hlsl = true, dynamicloop = true)
    let LanguageGLSL ver =
      Language (glslver = ver, dynamicloop = true, implicitconv = true)
    let LanguageESSL ver =
      Language (esslver = ver, dynamicloop = (ver>=300))
