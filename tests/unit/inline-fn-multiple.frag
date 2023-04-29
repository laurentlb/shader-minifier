uniform vec3 params[42];

vec3 i_getFoo() {
  return params[4];
}

vec3 f1() {
  return i_getFoo();
}

vec3 f2(float a) {
  return i_getFoo() + a;
}
