#version 460

out vec4 O;

float f(float x)
{
  if (x < 0.) discard;
  return x;
}

void main()
{
  float unused = f(O.x);
  O = vec4(1);
}
