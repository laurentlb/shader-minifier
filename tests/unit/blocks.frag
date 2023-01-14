float test_if()
{
  int foo = 2;
  float bar;
  if (foo == 2)
    foo++;
  if (foo < 5)
    if (foo == 3)
      bar = 0.2;
    else
      bar = 0.3;
  else
    bar = 0.4;
  return bar;
}

int k = 5;

float test_for()
{
  int foo = 2;
  int n = 0;
  for (int i = 0; i < 4; i++)
    foo += i;
  for (foo++; n < 4 ; n++)
    {
      int k = n - 1;
      foo += k;
    }
  return 1. / float(k);
}

int test_block()
{
  {}
  {if (k == 0) {} else {}}
  for (int i = 0; i < 2; i++)
  {
    if (k == 1) {k++; return 2;} else {break;}
  }
}

float removeUselessElseAfterReturn1(float f)
{
	if (f < 2.)
		return 1.;
	else
		f = 4.;
	return 5.;
}
float removeUselessElseAfterReturn2(float f)
{
	float a = 2.;
	if (f < a)
		return 1.;
	else
		float a = 4.;
	return 5.;
}
float replaceIfReturnsByReturnTernary1(float f)
{
	if (f < 2.)
		return b;
	return c;
}

void main()
{
  float a = test_if();
  float b = test_for();
  removeUselessElseAfterReturn1(0.);
  removeUselessElseAfterReturn2(0.);
  replaceIfReturnsByReturnTernary1(0.);
  gl_FragColor=vec4(.2,a,b,0.);
  if (a<b) { }
  if (a<b) { } else { }
}
