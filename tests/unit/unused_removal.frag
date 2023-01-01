float f();

float actually_unreachable() { return 1.5; }

float f(){
  float r = 1.;

  return actually_unreachable();
}

vec3 g() { return vec3(0.); }

vec2 pos = vec2(0.5);

void main(){
  gl_FragColor = vec4(1., g());
}
