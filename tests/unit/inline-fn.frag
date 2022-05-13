float i_foo(float f, vec3 g) {
  return f*g.x + g.y;
}

float a() {
  return i_foo(1.0, vec3(2.0, 3.0, 4.0));
}

float b(float g) {
  float x = i_foo(g + 10.0, vec3(20.0, 30.0, 40.0));
  float y = i_foo(g + 10.1, vec3(20.1, 30.1, 40.1));
  return x + y;
}

float i_bar(float f, float g) {
  return f*g + 1.0;
}

float c() {
  return i_bar(i_bar(2.0, 3.0), i_bar(4.0, 5.0)) + 6.0;
}

float d() {
  float f = i_bar(2.0, 3.0);
  float g = i_bar(f, f);
  return g + 4.0;
}

float i_multipass(float x) {
  float f = 42.0;
  float g = 2.0;
  return f*g*x;
}

float e() {
  return i_multipass(1.0);
}
