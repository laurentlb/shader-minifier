float min(float a, float b)
{
  return a < b ? a : b;
}

// https://github.com/laurentlb/Shader_Minifier/issues/20
float foo()
{
  float a = 1.2;
  float b = 2.3;
  return min((a=1.0,b+a), 0.0);
}

float bar()
{
  float a = 1.2;
  float b = 2.3;
  return min(b+=a, (a, b));
}
