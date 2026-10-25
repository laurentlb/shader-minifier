#version 460

uniform vec3 P;
uniform int index;
out vec4 fragColor;

void main()
{
  vec3 A = P*9.;
  vec3 B = P*P;
  vec3 C = -P;
  vec3 D = index>0 ? P : P*2.;
  fragColor = vec4(A[index] + B[index] + C[index] + D[index], (P*9.).yz, (-P).x);
}
