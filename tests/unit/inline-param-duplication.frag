#version 330
// Inlining a function whose parameter is used several times.
uniform float u;
float g;
out vec4 fragColor;

float sq(float a) { return a * a; }
float sqGlobal(float a) { return a * a; }
float sqExpr(float a) { return a * a; }

void main() {
  float x = sin(u);
  g = cos(u);
  fragColor = vec4(
    sq(x) + x,        // a local variable can be duplicated: inlined
    sqGlobal(g),      // a global can't be duplicated: not inlined
    sqExpr(x * 3.),   // a non-trivial expression can't be duplicated: not inlined
    1.);
}
