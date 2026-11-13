#version 330
// Tests for --move-declarations (grouping declarations of the same type).
uniform float u;
out vec4 fragColor;

float merge(float x) {
  float a = x * 2.;
  a += sin(a);
  float b = a * 3.;  // moved to the first float declaration, init becomes an assignment
  b += cos(x);
  vec2 v = vec2(a);  // a different type: not merged
  float c;           // no initializer: no assignment left behind
  c = b + a;
  float d = u, e = x; // several declarators in one declaration
  return c + b + v.x + d * e;
}

float withConst(float x) {
  float a = x * x;
  const float k = 2.;  // const declarations must keep their initializer and are not moved
  float b = a + k;
  const float k2 = 3.;
  return a * b * k2;
}

float shadowing(float x) {
  float y = x * 2.;
  {
    float z = y + 1.;
    x += z;
    float y = y * 3.;  // the initializer reads the outer y, so the declaration is not moved (#458)
    x += y * z;
  }
  return x + y;
}

void main() {
  fragColor = vec4(merge(u), withConst(u), shadowing(u), 1.);
}
