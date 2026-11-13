module internal Typer

// Best-effort typing of expressions, computed on demand.
// The typer never fails: when it can't tell, it returns Unknown (e.g. macros, unresolved
// names, unsupported constructs). Callers must treat Unknown conservatively.
//
// Variables are typed through Ident.Declaration, which is set by Analyzer.resolve.
// Idents created during a rewrite pass are not resolved yet, so they are Unknown until the next pass.

open Ast

[<RequireQualifiedAccess>]
type Prim = Bool | Int | UInt | Float | Double

[<RequireQualifiedAccess>]
type Ty =
    | Unknown
    | Scalar of Prim
    | Vector of Prim * size: int
    | Struct of StructOrInterfaceBlock // also an interface block with an instance name
    // TODO: Matrix, Array, and opaque types (samplers...).

    member this.isStruct =
        match this with
        | Struct _ -> true
        | _ -> false
    member this.isScalar =
        match this with
        | Scalar _ -> true
        | _ -> false
    member this.isScalarOrVector =
        match this with
        | Scalar _ | Vector _ -> true
        | _ -> false

// Parse a builtin type name, e.g. "float" or "ivec3".
// TODO: matrix types, HLSL type names (float3, half...).
let private ofTypeName = function
    | "bool" -> Ty.Scalar Prim.Bool
    | "int" -> Ty.Scalar Prim.Int
    | "uint" -> Ty.Scalar Prim.UInt
    | "float" -> Ty.Scalar Prim.Float
    | "double" -> Ty.Scalar Prim.Double
    | name when Builtin.builtinVectorTypes.Contains name ->
        // e.g. "ivec3" is "i" + "vec" + "3"
        let size = int name[name.Length - 1] - int '0'
        match name.Substring(0, name.Length - 4) with
        | "" -> Ty.Vector (Prim.Float, size)
        | "b" -> Ty.Vector (Prim.Bool, size)
        | "i" -> Ty.Vector (Prim.Int, size)
        | "u" -> Ty.Vector (Prim.UInt, size)
        | "d" -> Ty.Vector (Prim.Double, size)
        | _ -> Ty.Unknown
    | _ -> Ty.Unknown

let private ofType (ty: Type) =
    match ty.name with
    | _ when not ty.arraySizes.IsEmpty -> Ty.Unknown
    | TypeName name ->
        match name.Declaration with
        | Declaration.StructType block -> Ty.Struct block
        | _ -> ofTypeName name.Name
    | TypeBlock block -> Ty.Struct block
    | TypeTemplate _ -> Ty.Unknown

// Type of a declared variable or struct member, e.g. `x` in `float x[2]`.
let private ofDeclElt (ty: Type) (elt: DeclElt) =
    if elt.sizes.IsEmpty then ofType ty else Ty.Unknown

// Matches a float or double type, scalar or vector.
// Builtin function calls are typed only when their arguments match this pattern:
// with an int argument, the result can be a float, e.g. `sin(1)`.
let private (|FloatingPoint|_|) = function
    | Ty.Scalar (Prim.Float | Prim.Double) | Ty.Vector ((Prim.Float | Prim.Double), _) as ty -> Some ty
    | _ -> None

// Type of a call to a builtin function or constructor, given the types of its arguments.
// TODO: matrix functions, texture functions, functions returning bool vectors.
let private ofBuiltinCall name (argTypes: Ty list) =
    match argTypes with
    | _ when Builtin.castFunctions.Contains name -> ofTypeName name // constructor, e.g. vec3(...)
    | FloatingPoint ty :: _ when Builtin.sameTypeAsFirstArgFunctions.Contains name -> ty
    | _ when Builtin.sameTypeAsLastArgFunctions.Contains name ->
        match List.tryLast argTypes with
        | Some (FloatingPoint ty) -> ty
        | _ -> Ty.Unknown
    | FloatingPoint (Ty.Scalar prim | Ty.Vector (prim, _)) :: _ when Builtin.scalarResultFunctions.Contains name -> Ty.Scalar prim
    | _ -> Ty.Unknown

// Type of `e.field`, where e has type ty.
let private ofField (ty: Ty) (field: string) =
    match ty with
    | Ty.Vector (prim, size) when Builtin.isFieldSwizzle field && field.Length <= 4
                                  && Seq.forall (fun c -> Builtin.swizzleIndex c < size) field ->
        if field.Length = 1 then Ty.Scalar prim else Ty.Vector (prim, field.Length)
    | Ty.Struct block ->
        block.members |> List.tryPick (function
            | MemberVariable (ty, elts) ->
                elts |> List.tryFind (fun elt -> elt.name.Name = field) |> Option.map (ofDeclElt ty)
            | Method _ -> None)
        |> Option.defaultValue Ty.Unknown
    | _ -> Ty.Unknown // TODO: swizzles on scalars

// Type of `e1 op e2`, for an arithmetic operator.
let private ofArithmetic t1 t2 =
    match t1, t2 with
    | Ty.Scalar p1, Ty.Scalar p2 when p1 = p2 -> t1
    | Ty.Scalar p1, Ty.Vector (p2, _) when p1 = p2 -> t2
    | Ty.Vector (p1, _), Ty.Scalar p2 when p1 = p2 -> t1
    | Ty.Vector _, Ty.Vector _ when t1 = t2 -> t1
    | _ -> Ty.Unknown // TODO: implicit conversions, matrices

let rec typeOf = function
    | Int (_, "") -> Ty.Scalar Prim.Int
    | Int (_, ("u" | "U")) -> Ty.Scalar Prim.UInt
    | Float (_, ("" | "f" | "F")) -> Ty.Scalar Prim.Float
    | Float (_, ("lf" | "LF")) -> Ty.Scalar Prim.Double
    | ResolvedVariableUse (_, vd) -> ofDeclElt vd.ty vd.decl
    | Dot (e, field) -> ofField (typeOf e) field.Name
    | FunCall (Op ("-" | "+"), [e]) -> typeOf e
    | FunCall (Op ("+" | "-" | "*" | "/"), [e1; e2]) -> ofArithmetic (typeOf e1) (typeOf e2)
    | FunCall (Var f, args) ->
        match f.Declaration with
        // An overload of a builtin function may not be the function that's called.
        | Declaration.UserFunction fd when not (Builtin.builtinFunctions.Contains f.Name) -> ofType fd.funcType.retType
        | Declaration.BuiltinFunction -> ofBuiltinCall f.Name (List.map typeOf args)
        | Declaration.StructType block -> Ty.Struct block // constructor
        | _ -> Ty.Unknown // e.g. overloaded functions
    // TODO: other operators, subscripts, bool literals, half and long suffixes.
    | _ -> Ty.Unknown

// Whether `e.field` can be a swizzle. When the type of e is unknown, only the field name is checked.
let isSwizzle e (field: Ident) =
    Builtin.isFieldSwizzle field.Name && not (typeOf e).isStruct

// Whether `t1 * t2` is equal to `t2 * t1`.
// The multiplication is not commutative with matrices, unless the other operand is a scalar.
let isCommutativeMul (t1: Ty) (t2: Ty) =
    t1.isScalar || t2.isScalar || (t1.isScalarOrVector && t2.isScalarOrVector)
