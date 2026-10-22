#version 330
// With --preserve-all-globals, a local variable must not get the name of a function it calls.
// Many short function names and many live locals, so that a collision is likely without the fix.
uniform vec3 c0;
out vec4 O;
float oa(vec3 q) { return length(q) - 1.; }
float ob(vec3 q) { return length(q) - 2.; }
float o(vec3 q)
{
  return min(oa(q), ob(q));
}
float a(float x) { return x * x + c0.x; }
float b(float x) { return sin(x) + c0.y; }
float c(float x) { return cos(x) + c0.z; }
float e(float x) { return fract(x) + c0.x; }
float f(float x) { return abs(x) + c0.y; }
void main() {
  vec3 p = c0;
  float d = o(p);
  p += d * o(p);
  d = o(p);
  p += d * o(p);
  float s = a(p.x), t = b(p.y), u = c(p.z);
  float v = e(s) + f(t), w = a(u) * b(v), x = c(w) - e(s);
  float y = f(x) + a(t), z = b(y) * c(u) + e(w) - f(v);
  float m = a(s + t), n = b(t + u), r = c(u + v);
  O = vec4(p, d) + vec4(s, t, u, v) * w + vec4(x, y, z, m) * n + r;
}
