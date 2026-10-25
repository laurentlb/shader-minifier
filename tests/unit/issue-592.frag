#version 460
#line 1 2

uniform float u;
#line 10
out vec4 O;
#line 20 "x.frag"
void main()
{
  O = vec4(u);
}
#line 40
