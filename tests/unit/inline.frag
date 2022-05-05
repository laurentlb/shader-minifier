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
