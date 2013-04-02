float f1() { return 0.1; }
float f2(void) { return 0.2; }
float f1(int x) { return 0.3; }
float f2(float y) { return 0.4; }
float f3(bool b) { return 0.5; }
float f3(int a, int b) { return 0.6; }
float f3(int a, int b, int c, int d) { return 0.7; }

void main()
{
  float a = f1() + 2. * f1(0);
  float b = 2. * f2(1) - f2();
  float c = f3(true) + f3(0, 1) - f3(0, 1, 2, 3);
  gl_FragColor=vec4(a,b,c,0.);
}
