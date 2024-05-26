float f();

float actually_unreachable() { return 1.5; }
float noinline_actually_unreachable2() { return 1.5; }

float f(){
  float r = 1.;

  return actually_unreachable();
}

vec3 g() { return vec3(0.); }

vec2 pos = vec2(0.5);

// hidden() is unused and should be removed even if a parameter named 'hidden' is used in another function.
int hidden() {
	int noinlinevar = 0;
	noinlinevar++;
	return noinlinevar;
}

float foo(float hidden) {
	float noinlinevar = 0.;
	noinlinevar++;
	return hidden+noinlinevar;
}

float glob;
float side_effect() {
	return glob++;
}

vec4 noinline_test()
{
  vec4 bb;
  return bb;
}

void main(){
	float unused_var = side_effect();

  if (false) {
    noinline_actually_unreachable2();
	return;
  }
  gl_FragColor = vec4(foo(3.), g()) + noinline_test();
}
