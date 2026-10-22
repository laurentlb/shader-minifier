// With --preserve-all-globals, a local variable must not get the name of a function it calls.
uniform vec3 c0;
out vec4 O;
float oa(vec3 q) { return length(q) - 1.; }
float ob(vec3 q) { return length(q) - 2.; }
float o(vec3 q)
{
  return min(oa(q), ob(q));
}
void main() {
  vec3 c = c0;
  float e = o(c);
  c += e * o(c);
  e = o(c);
  c += e * o(c);
  O = vec4(c, e);
}
