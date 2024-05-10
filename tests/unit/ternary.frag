#version 450

// https://github.com/laurentlb/Shader_Minifier/issues/320
float f0() {
    float a = 1., b = 1.;
    a == 1. ? b = 2. : b = 3.;
    return b; // 2
}

float f1() {
    float a = 1., b = 1.;
    a = true ? b = 2. : b = 3.;
    return a;
}

// Fix for #385

out vec4 O;
int k;
void main() {
  int d=0,e=0;
  k==0 ? (sin(O.x),d=1) : (cos(O.x),e=2) ;
  O.x=d;
  O.y=e;
}
int f2() {
  k++ == 0 ? 1 : 2 ;
  return k;
}
