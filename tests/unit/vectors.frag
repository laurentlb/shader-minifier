float swizzles() {
  vec2 v1 = vec2(1., 1.);
  vec3 v2 = vec3(v1.x, v1.y, v1.x);
  vec3 v3 = vec3(v1.x, v2.x, v2.r);
  vec4 v4 = vec4(v1.xx, v1.y, v1.x);
  vec4 v5 = vec4(1., v2.z, v2.g, 2.);
  vec4 v6 = vec4(v1.x, v1.y, v2.r, v2.t);
  return v1.x + v2.x + v3.x + v4.x + v5.x + v6.x;
}

vec4 constructor() {
	return vec4(1., 1e2, 2e3, 3e4);
}

float constructors() {
    vec2 v2 = vec2(1e10, 1e10);
    vec3 v3 = vec3(v2, v2);
    vec4 v4 = vec4(v3, v2);
    return v2.x + v3.x + v4.x;
}

float withExtraComponents() {
    vec2 v2 = vec2(1);
    vec3 v3 = vec3(1, 2, v2.x);
    vec4 v4 = vec4(v2, v3.xy);
    vec3 v5 = vec3(1, v3.rg);
    return v2.x + v3.x + v4.x + v5.x;
}

float withExtraComponentsUnsafe(in vec3 a) {
    vec3 v1 = vec3(a.x);
    vec2 v3 = vec2(1.0, a.y);
    vec3 v4 = vec3(1.0, 2.0, a.y);
    return v1.x + v3.x + v4.x;
}

struct S{
  vec2 p1;
  vec2 cp1;
  vec2 cp2;
  vec2 p2;
};

vec2 calc(S points, float t) {
  // #282 - not a swizzle
  vec4 m1m2 = mix(vec4(points.p1, points.cp1), vec4(points.cp1, points.cp2), t);
  return m1m2.xy;
}
