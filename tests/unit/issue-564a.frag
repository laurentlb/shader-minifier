#version 300 es
precision highp float;
uniform float t;
out vec4 outColor;
float foo(float x) {
  return x + x;
}
vec2 bar(float x) {
  return vec2(x + x, x);
}
void main() {
  outColor = vec4(foo(t) + foo(2.0 * t), bar(3.0 * t), 0.0);
}
