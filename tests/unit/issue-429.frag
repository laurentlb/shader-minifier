#version 330
// Constant folding must respect int to float conversions.
uniform int i;
out vec4 O;
void main() {
  O.r = (1.*487)/5;
  O.g = 487./5;
  O.b = 1./3;
  O.a = float(i*1.) + (i+0.)/2;
}
