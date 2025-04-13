#version 460
// struct declaration in TypeDecl
struct MarchData { // newName
    float dist;
    vec3 col;
    float spec;
};
// struct in TLDecl
MarchData m;
uniform MyInterfaceBlock
{
    // struct in interface block field
    MarchData march;
    /* Not allowed, at least on shadertoy.
    // anonymous struct declaration in interface block field
    struct {
        // struct identifier use in field in interface block
        MarchData abc;
    } struc;
    */
};
// struct identifier use in FunctionType.retType
MarchData struct_as_ret()
{
    return m;
}
// struct identifier use in FunctionType.args
int struct_as_arg(MarchData m)
{
    return 1;
}
int struct_uses() {
    // struct identifier use in Stmt.Decl
    MarchData m;
    // struct field access
    m.dist = 1.;
    m.spec = m.dist / 2.;
    // struct identifier use in constructor
    m = MarchData(1., vec3(1), m.spec);
    // anonymous struct declaration in Stmt.Decl
    struct {
        // struct identifier use in field in local
        MarchData abc;
    } s;
    // struct identifier use in Stmt.Decl
    for (MarchData m2; true; ) break;
    // anonymous struct declaration in Stmt.ForD
    for (struct {
                    // struct identifier use in field in local
                    MarchData abc;
                } s; true; ) break;
    return int(m.col.x + s.abc.dist + march.spec);
}

// anonymous struct declaration at top level
struct {
    // struct identifier use in field in global struct
    MarchData abc;
} s2;

// bug test
float v(vec4 w)
{
  float MarchData = w.x-4.0; // reuse type name as var
  return w.w + MarchData*MarchData; // don't rename field `.w`
}
