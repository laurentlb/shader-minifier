#version 120

float result;

void main()
{
  float x = 0.5;
  float i_y = 0.6*x;
  float a = x * i_y;
  result = a;
}

int arithmetic()
{
  int i_a = 2;
  int i_b = 3;
  int i_c = i_a + i_b;
  return 4 * i_a * i_c;
}

int vars(int arg, int arg2)
{
  int i_a = arg;
  int i_b = arg2;
  int i_c = i_a + i_b;
  return i_a * i_c;
}

int arithmetic2()
{
  int a = 2;
  int b = 3;
  int c = a + b;
  return 4 * a * c;
}

int unusedVars() {
  int a = arithmetic();
  int b = 13;
  int c = 10;
  int d = c * 3;
  return d;
}

int unusedVars2() {
  int var1 = 1, var2 = 2, var3 = 3, var4 = 4, var5 = 5, var6 = 6;
  int var7 = 7, var8 = 8, var9 = 9, var10 = 10, var11 = 11, var12 = 12;
  return var1 + var5 + var12;
}

int multiPass()
{
  int one = 1;
  int two = one * 2;
  int three = two + 1;
  return three;
}

float multiPass2() {
  float i_a = 4.0;
  float b = i_a + 5.0;
  return b;
}

atomic_uint hydrogen;
uint builtin_with_or_without_side_effects(uint x)
{
	uint not_inlined = atomicCounterIncrement(hydrogen);
	uint inlined = max(x * x, x + 1);
	atomicCounterIncrement(hydrogen);
	return x + inlined + not_inlined;
}

float dmin(float a, float b)
{
    return a+b+a*b;
}
float reduce_consecutive_assignments(float x)
{
    float dmat=34.+x;
    dmat=dmin(dmat,.2);
    dmat=dmin(dmat,.2);
    dmat=dmin(dmat,.3);
    dmat=dmin(dmat,.4);
    dmat=dmin(dmat,.2);
    dmat=dmat*dmat+dmat;
    return dmat+x;
}

// repro for #176
int dont_inline_lvalue() {
  int a = 1;
  a = 2;
  return 3;
}

// repro for #247
vec4 fragColor247;
const float t247 = 1.5+(1.+.5);
void main247() {
  fragColor247 = vec4(t247);
}

// repro for #248
vec4 fragColor248;
void main248() {
  float t = 1.5+(1.+.5);
  fragColor248 = vec4(t);
}

float arr[] = float[](3.4, 4.2);
void lvalues() {
  int a = 1;
  arr[a] = 2.;
}

uniform int time;
in int sync;

int dependOnConst() {
  int x = time + sync;
  int y = x * 2;
  return y*3;
}

// repro for #179
float noinl179(float x) {
	float old = x;
	x = 100.0;
	return old + x;
}

// repro for a bug
float inlineWithShadowing(float x) {
	float inl = sin(2.5);
	if (x < 0.)
	{
	    float inl = 3.4;
		inl++;
	}
	return inl;
}

// repro for a bug
float inline_uninitialized()
{
    float c;
    return c;
}

// repro for a bug
float glo;
float noinline_readsTheGlobal()
{
	return glo;
}
float dontCompressAssigments()
{
	glo = 10.;
	glo = 50. + noinline_readsTheGlobal();
	return glo*glo;
}
