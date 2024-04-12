#   extension GL_EXT_gpu_shader4 : enable
#  define TEST
# ifndef A
# endif

vec3 color; // TLDecl

struct s { // TypeDecl
  vec4 n, f;
  bool ok;
};

float sdf(vec3 p) { // Function
  color = vec3(1.);
  return length(p) - 1.;
}

void main() {
    int ijk = 32;
    ijk += ijk;
}