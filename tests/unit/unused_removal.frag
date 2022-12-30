float f();

float f(){
  float r = 1.;

  return r;
}

vec3 g() { return vec3(0.); }

vec2 pos = vec2(0.5);

void main(){
  gl_FragColor = vec4(1., g());
}
