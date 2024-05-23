#version 130

in vec3 foo;

float f(in float a) {
  return a;
}

float g(const in float a) {
  return a;
}

float h(inout float a) {
  return a;
}
