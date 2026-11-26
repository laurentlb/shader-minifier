#version 450

uniform float u;
out vec4 o;

void main() {
    /* Note: Verbatim subtrees do not get parsed and will still get inlined! :/ */
    //[
    float a = 3.;
    //]
    a = (u, //[
        a * 2.
    //]
    );
    float b = true ? u, //[
        a * 2.
    //]
    : a;
    float c = false ? a : (u, //[
        a * 2.
    //]
    );
    o = vec4(a, b, c, 0);
}
