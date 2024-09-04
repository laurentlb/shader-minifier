#version 330

int i;

float bar(float x)
{
	float a = x;
	a = 6.;
	a = a + x;
	
	float b = x;
	b = b + x;
	b = 34.;
	b = b + x;

    float arr[2] = float[2](7.,8.);
	arr = float[2](5.,float(arr.length()));
	
	float m = 14.+length(vec3(sin(a *= 10), sin(a *= 20), sin(a *= 30)))+b++;
	m = arr[1] + arr[i++];
		
	return a + b + m;
}
float baz(float a)
{
	float b = a + 4.;
	b += sin(a);
	float c = b + 5.;
	c += sin(b);
	return c + (-(c - -c));
}

int n = 2;
float y = 47.;
out vec3 outputvar;

float foo(float a) { if (n == 1) return 0.; if (n == 2) return a; return 1.; }
float foo(float a, float b) { if (y > a) return 0.; if (y < b) return a; return 1.; }

void notMain(float x) {
	vec3 v = vec3(foo(42.) + foo(50., 70.));
	outputvar.rgb = v + vec3(bar(x) + baz(x));
}

// #442: function inlining when a parameter is read more than once, but the argument expr is pure and trivial, like a variable that's read (but not written)
bool IsMoto(float mid) {
  return mid>=1.&&mid<=8.;
}
int foo442(float x) {
  if (IsMoto(x)) return 1;
  return 2;
}

// #450: argument inlining of read-only globals
in vec3 camPos;
float len(vec3 pos) {
  return length(pos);
}
void main450() {
  float a = len(camPos);
  float b = len(camPos);
  f(a, b);
}
