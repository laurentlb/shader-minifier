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
	return a*b;
}
