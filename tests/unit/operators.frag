#version 330

float notmain()
{
  int a = 1 + 2 * 3 + 4;
  int b = 14 / 2;
  int c = 8 % 3;
  
  float f = 1.23456 * 2.1234;
  f = 1.4 / 2.;
  f = 1.4 / 3.;
  float g = mod(8., 3.);
  
  float z =
    (1.  < 2. ? 1 : 2) +
    (1.  > 2. ? 1 : 2) * 10 +
    (1. <= 2. ? 1 : 2) * 100 +
    (1. >= 2. ? 1 : 2) * 1000 +
	(1. == 2. ? 1 : 2) * 10000 +
	(1. != 2. ? 1 : 2) * 100000;

  return a+b+c+f+g+z;
}

int no_parens(int a, int b, int c) {
	return a + (b + c) + a + (b * c);
}

int no_parens2(int a, int b, int c) {
	int d = a*(b*c);
	return a - (b - 1) - (d - c);
}

int other(int a, int b, int c, int d) {
    return (a*b)*(c*d)*(a*b);
}

float f(float x)
{
	float a = (x += 1.0, length(vec3(x)));
	return a*a*sin((a*=a, a));
}

float desugar_compound_assignment_for_ternary(float x)
{
	x += x * x;
	if (x == sqrt(x))
		x += cos(x);
	else
		x *= sin(x);
	return x * x;
}

float cool_ops(float g)
{
	float f = 0.0;
	f += +f++ + ++f;
	f /= -f - -f;
	f -= -f-- - --f;
	f *= +f + +f;
	f += --f + (- +(++f));
	return f;
}

float swap_op_order(float g)
{
	float a = g*((g--)*(g--));
	float b = g*((g+2.)*(g+3.));
	return a*b*g;
}

mat2 dont_swap_op_order(float g)
{
	return mat3x2(g) * (mat4x3(g) * mat2x4(g));
}

void augmented_mul(inout vec3 p, inout vec3 q, inout vec3 r, mat3 m, float s)
{
	p = s * p; // p*=s
	q = q.yzx * q; // q*=q.yzx
	r = m * r; // not commutative: unchanged
}

float fold_mixed_constants()
{
	float a = (1.*487)/5; // 97.4, not 97 (#429)
	float b = 487./5;
	float c = 487/5.;
	float d = 2.+1;
	return a+b+c+d;
}

void typed_calls(inout vec3 p, inout vec3 q, inout vec3 r, inout vec3 s, inout vec3 u, float t)
{
	p = sin(t) * p; // p*=sin(t)
	q = step(.5, q) * q; // q*=step(.5,q)
	r = normalize(r) * r; // r*=normalize(r)
	s = vec3(t) * s; // s*=vec3(t)
	u = sin(1) * u; // not simplified: the type of sin(1) is unknown (the int is converted to float)
}
