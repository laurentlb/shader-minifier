#version 330
uniform sampler2D _V;
uniform vec2 a_;
in vec2 ZH;
out float stopinlining;

float f1() { return stopinlining+0.1; }
float f2(void) { return stopinlining+0.2; }
float f1(int x) { return stopinlining+0.3; }
float f2(float y) { return stopinlining+0.4; }
float f3(bool b) { return stopinlining+0.5; }
float f3(int a, int b) { return stopinlining+0.6; }
float f3(int a, int b, int c, int d) { return stopinlining+0.7; }

float hashTex(sampler2D _V, float p)
{
  return texelFetch(_V, ivec2(255. * p) % 256, 0).r;
}
float hashTex(sampler2D _V, vec2 p)
{
  return texelFetch(_V, ivec2(255. * p) % 256, 0).r;
}
float hashTex(sampler2D _V, vec3 p)
{
  float h = texelFetch(_V, ivec2(255. * p.yz) % 256, 0).r;
  return texelFetch(_V, ivec2(255. * p.x, 255. * h) % 256, 0).r;
}

void main()
{
  float stopinlining = 0.;
  float a = f1() + 2. * f1(0);
  float b = 2. * f2(1.2) - f2();
  float c = f3(true) + f3(0, 1) - f3(0, 1, 2, 3);
  float d = hashTex(_V, a_ * ZH);
  gl_FragColor=vec4(a,b,c,d+stopinlining++);
}
