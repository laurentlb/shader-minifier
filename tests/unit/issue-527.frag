#version 300 es
precision highp float;
float D(vec2 _) { return dot(_, _); }
float D(vec3 _) { return dot(_, _); }
float D(vec4 _) { return dot(_, _); }
float S(float _) { return clamp(_, 0.0, 1.0); }
vec2  S(vec2  _) { return clamp(_, 0.0, 1.0); }
vec3  S(vec3  _) { return clamp(_, 0.0, 1.0); }
vec4  S(vec4  _) { return clamp(_, 0.0, 1.0); }
out vec4 O;
void main() {
    O = vec4(1, S(D(gl_FragCoord.xy / 500.0)), 1, 1);
}
