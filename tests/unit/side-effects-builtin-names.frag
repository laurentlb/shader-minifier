#version 450
uniform float u;
out vec4 color;
float seed;

// A user function with the name of a pure builtin is not pure.
float noise() { return fract(sin(seed++) * 43758.5); }

vec2 userNoise() {
  vec2 v = vec3(1, 2, noise()).xy; // noise() must be kept
  float unused = noise();          // noise() must be kept
  return v;
}

// modf writes its second argument.
float modfOut(float x) {
  float ip;
  float unused = modf(x, ip);      // modf must be kept: it sets ip
  vec2 v = vec3(1, 2, modf(x, ip)).xy;
  return ip + v.x;
}

// Pure builtins are still optimized.
vec2 pureBuiltin(float x) {
  float unused = sin(x);           // removed
  return vec3(1, 2, sin(x)).xy;    // vec2(1,2)
}

void main() {
  seed = u;
  color = vec4(userNoise(), modfOut(u), pureBuiltin(u).x);
}
