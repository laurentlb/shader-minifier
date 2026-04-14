module internal Builtin

// source: https://registry.khronos.org/OpenGL/specs/gl/GLSLangSpec.4.60.pdf

let keywords = System.Collections.Generic.HashSet<_>([
  "if"; "else"; "break"; "continue"; "do"; "for"; "while"; "switch"; "case"; "default";
  "in"; "out"; "inout"; "discard"; "return"; "lowp"; "mediump"; "highp"; "precision";
  "struct"; "layout"; "centroid"; "flat"; "smooth"; "noperspective"; "patch"; "sample"; "invariant";
  "precise"; "subroutine"; "coherent"; "volatile"; "restrict"; "readonly"; "writeonly";
  "const"; "uniform"; "buffer"; "shared"; "attribute"; "varying"
  "template"
])

let builtinScalarTypes = set [
    "bool"; "int"; "uint"; "float"; "double"
]
let builtinVectorTypes = set([
    for p in [""; "d"; "b"; "i"; "u"] do
        for n in ["2"; "3"; "4"] do
            yield p+"vec"+n
])
let builtinMatrixTypes = set([
    for p in [""; "d"] do
        for n in ["2"; "3"; "4"] do
            yield p+"mat"+n
        for c in ["2"; "3"; "4"] do
            for r in ["2"; "3"; "4"] do
                yield p+"mat"+c+"x"+r
])

let isSamplerType (name: string) = name.Contains("sampler")

// MSL scalar types: float/half/char/short/int/uint/bool + N for vectors, MxN for matrices.
let mslScalarTypes = set [
    "half"; "char"; "uchar"; "short"; "ushort"; "long"; "ulong"; "size_t"; "ptrdiff_t"
]
let mslVectorTypes = set([
    for p in ["float"; "half"; "char"; "uchar"; "short"; "ushort"; "int"; "uint"; "long"; "ulong"; "bool"] do
        for n in ["2"; "3"; "4"] do
            yield p+n
])
let mslMatrixTypes = set([
    for p in ["float"; "half"] do
        for c in ["2"; "3"; "4"] do
            for r in ["2"; "3"; "4"] do
                yield p+c+"x"+r
])
// MSL templated types (parser accepts <...> suffix, so we just list the heads).
let mslTemplatedTypes = set [
    "texture1d"; "texture1d_array"; "texture2d"; "texture2d_array"; "texture2d_ms";
    "texture2d_ms_array"; "texture3d"; "texturecube"; "texturecube_array";
    "depth2d"; "depth2d_array"; "depth2d_ms"; "depth2d_ms_array"; "depthcube"; "depthcube_array";
    "array"; "vec"; "matrix"; "atomic"
]

let builtinTypes = set [ "void" ] + builtinScalarTypes + builtinVectorTypes + builtinMatrixTypes
                   + mslScalarTypes + mslVectorTypes + mslMatrixTypes + mslTemplatedTypes;

let implicitConversions = // (from, to)
    [
        (["int"], "uint")
        (["int"; "uint"], "float")
        (["int"; "uint"; "float"], "double")
        (["ivec2"], "uvec2")
        (["ivec3"], "uvec3")
        (["ivec4"], "uvec4")
        (["ivec2"; "uvec2"], "vec2")
        (["ivec3"; "uvec3"], "vec3")
        (["ivec4"; "uvec4"], "vec4")
        (["ivec2"; "uvec2"; "vec2"], "dvec")
        (["ivec3"; "uvec3"; "vec3"], "dvec3")
        (["ivec4"; "uvec4"; "vec4"], "dvec4")
        (["mat2"], "dmat2")
        (["mat3"], "dmat3")
        (["mat4"], "dmat4")
        (["mat2x3"], "dmat2x3")
        (["mat2x4"], "dmat2x4")
        (["mat3x2"], "dmat3x2")
        (["mat3x4"], "dmat3x4")
        (["mat4x2"], "dmat4x2")
        (["mat4x3"], "dmat4x3")
    ]

let assignOps = set [
    "="; "+="; "-="; "*="; "/="; "%="
    "<<="; ">>="; "&="; "^="; "|="
    "++"; "--"; "$++"; "$--"
]
let nonAssignOps = set [
    "*"; "/"; "%"
    "+"; "-"
    "<<"; ">>"
    "<"; ">"; "<="; ">="
    "=="; "!="
    "&"; "^"; "|"
    "&&"; "^^"; "||"
]
let augmentableOperators = set ["+"; "-"; "*"; "/"; "%"; "<<"; ">>"; "&"; "^"; "|"]

let castFunctions = builtinTypes - set ["void"]
let trigonometryFunctions = set([
    "acos"; "acosh"; "asin"; "asinh"; "atan"; "atanh"; "cos"; "cosh"; "degrees";
    "radians"; "sin"; "sinh"; "tan"; "tanh"])
let mathsFunctions = set([
    "abs"; "ceil"; "clamp"; "dFdx"; "dFdy"; "exp"; "exp2"; "floor"; "floor"; "fma";
    "fract"; "fwidth"; "inversesqrt"; "isinf"; "isnan"; "log"; "log2"; "max"; "min";
    "mix"; "mod"; "modf"; "noise"; "pow"; "round"; "roundEven"; "sign"; "smoothstep";
    "sqrt"; "step"; "trunc"])
let vectorFunctions = set([
    "cross"; "distance"; "dot"; "equal"; "faceforward"; "length"; "normalize";
    "notEqual"; "reflect"; "refract"])
let textureFunctions = set(["texture"; "textureLod"; "texture2D"; "texelFetch"; "textureLodOffset"])

// MSL-specific built-in functions (subset used in typical shaders).
let mslBuiltinFunctions = set [
    "saturate"; "rsqrt"; "fma"; "fmod"; "select"; "popcount"; "clz"; "ctz";
    "as_type"; "atomic_load_explicit"; "atomic_store_explicit"; "atomic_fetch_add_explicit"
]

let pureBuiltinFunctions =
    trigonometryFunctions +
    mathsFunctions +
    vectorFunctions +
    castFunctions +
    textureFunctions +
    mslBuiltinFunctions

let impureBuiltinFunctions = set ["atomicCounterIncrement"]

let builtinFunctions = pureBuiltinFunctions + impureBuiltinFunctions

// Type qualifiers telling that a global variables is an 'external' name
// (it may be referenced from other files).
let externalQualifiers = set ["in"; "out"; "attribute"; "varying"; "uniform"]

let isFieldSwizzle s =
    Seq.forall (fun c -> Seq.contains c "rgba") s ||
    Seq.forall (fun c -> Seq.contains c "xyzw") s ||
    Seq.forall (fun c -> Seq.contains c "stpq") s

let swizzleIndex = function
    | 'r' | 'x' | 's' -> 0
    | 'g' | 'y' | 't' -> 1
    | 'b' | 'z' | 'p' -> 2
    | 'a' | 'w' | 'q' -> 3
    | c -> failwithf "not a swizzle (%c) " c
