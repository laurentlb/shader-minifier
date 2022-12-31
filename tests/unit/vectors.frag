float swizzles() {
  vec2 v1 = vec2(1., 1.);
  vec3 v2 = vec3(v1.x, v1.y, v1.x);
  vec3 v3 = vec3(v1.x, v2.x, v2.r);
  vec4 v4 = vec4(v1.xx, v1.y, v1.x);
  vec4 v5 = vec4(1., v2.z, v2.g, 2.);
  vec4 v6 = vec4(v1.x, v1.z, v2.r, v2.t);
  return v1.x + v2.x + v3.x + v4.x + v5.x + v6.x;
}

void main() { swizzles(); }
