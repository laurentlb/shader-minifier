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
	m = 58.;
		
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
float reuse_a_instead_of_declaring_c()
{
  float a = 34.;
  sin(a *= 10), sin(a *= 20), sin(a *= 30);
  float b = a + 4.;
  b += sin(a);
  float c = b + 5.;
  c += sin(b);
  return c + (-(c - -c));
}

/*
float more_var_decl_reuse(float x) {
	float a = 1.+x;
	int b = 3+int(x), b2 = 5+int(a);
	float c = a, c2 = 9.+a;
	int d = 3+b+b2, d2 = d+b2-b;
	float e = c*c2, e2 = 4.-c-c2;
	int f = 3*d*d2, f2 = 4/d2-d;
	float g = e-float(f)+e2, g2 = 4.-g+e2+e;
	int h = 3*f-f2, h2 = 7*f2-f;
	return float(h*h2)*g*g2*x + g2/h2-h;
}
*/

int n = 2;
float y = 47.;
out vec3 output;

float foo(float a) { if (n == 1) return 0.; if (n == 2) return a; return 1.; }
float foo(float a, float b) { if (y > a) return 0.; if (y < b) return a; return 1.; }

void notMain(float x) {
	vec3 v = vec3(foo(42.) + foo(50., 70.));
	output.rgb = v + vec3(bar(x) + baz(x) + reuse_a_instead_of_declaring_c(x));
}