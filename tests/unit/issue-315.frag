// Inlining must keep the implicit int to float conversion.
uniform int u;
uniform float other_value;
out vec4 O;
float a = 4;
vec3 b = vec3(a / 8, a / 8, 0);
void main() {
  float distance = 100000; // #329
  float c = u * 2;
  if (other_value < distance * distance)
    O = vec4(b, c / 3);
}
