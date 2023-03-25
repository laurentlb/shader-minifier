float f();

float actually_unreachable() { return 1.5; }

float f(){
  float r = 1.;

  return actually_unreachable();
}

vec3 g() { return vec3(0.); }

vec2 pos = vec2(0.5);

// hidden() is unused and should be removed even if a parameter named 'hidden' is used in another function.
int hidden() {
	int noinline = 0;
	noinline++;
	return noinline;
}

float foo(float hidden) {
	float noinline = 0.;
	noinline++;
	return hidden+noinline;
}

void main(){
  gl_FragColor = vec4(foo(3.), g());
}