#version 460

uniform float u;
out vec4 O;

void main()
{
  float a = 1e30;
  float b = 1e-30;
  float c = 1./max(abs(u), 1e-30);
  float d = 1e-20f + 2e30lf;
  O = vec4(a, b, c, d);
}
