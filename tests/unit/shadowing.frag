float f(float g)
{
	float f = 0.0;
	f += f++ + ++f;
	return f;
}
float g(float f)
{
	{
		f++;
		float f = f + 1.0, g = 9.0;
		f+=g;
	}
	return f;
}

#define noinline_random 4

int m1()
{
	int x = 1;
	if (noinline_random > 0.5)
	{
		int x = 2, y = x; // y is initialized to 2
		return y;
	}
}

struct S{ int x; };

S m2()
{
	S S = S(0); // 'S' is only visible as a struct and constructor
	return S; // 'S' is now visible as a variable
}

void m4(int k)
{
	//int k = k + 3; // redeclaration error of the name k
	{
		int k = k + 3; // 2nd k is parameter, initializing nested first k
		int m = k; // use of new k, which is hiding the parameter
	}
	return k;

	//int x = x; // Error if x has not been previously defined.
	// If the previous definition of x was in this
	// same scope, this causes a redeclaration error.
}

void m5(float k)
{
	float m = 9.;
	m = 14.;
	{
		float k = k + 3.; // 2nd k is parameter, initializing nested first k
		m = k; // use of new k, which is hiding the parameter
	}
	return k + 2. * m;
}
void m6()
{
	m5(55.);
}