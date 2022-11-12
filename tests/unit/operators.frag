#version 330

void main()
{
  int a = 1 + 2 * 3 + 4;
  int b = 14 / 2;
  int c = 8 % 3;
  
  float f = 1.23456 * 2.1234;
  f = 1.4 / 2.;
  f = 1.4 / 3.;
  float g = mod(8., 3.);
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
