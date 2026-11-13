#version 330
// Unused assignments are only removed when the code has no branch or loop
// (the rewriter calls this "single-flow", see isSingleFlow).
uniform float u;
float g;
out vec4 fragColor;
void h() { g++; }

float doWhileLoop(float x) {
  float a = sin(x); // unused, but not removed: the code after it has a loop
  h();
  a = cos(x);
  do { x += a; } while (x < 10.);
  return x;
}

float switchStmt(float x) {
  float a = sin(x); // unused, but not removed: the code after it has a switch
  h();
  a = cos(x);
  switch (int(x)) { case 0: x = a; break; default: x = 1.; }
  return x;
}

float noBranch(float x) {
  float a = sin(x);
  g = a;
  h();
  a = cos(x);  // unused: removed
  return x + g;
}

void main() {
  fragColor = vec4(doWhileLoop(u), switchStmt(u), noBranch(u), 1.);
}
