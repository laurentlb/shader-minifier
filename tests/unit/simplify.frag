int n = 2;
float y = 47.;

float foo(float a) { if (n == 1) return 0.; if (n == 2) return a; return 1.; }
float foo(float a, float b) { if (y > a) return 0.; if (y < b) return a; return 1.; }

void main() {
	vec3 v = vec3(foo(42.) + foo(50., 70.));
	gl_fragColor.rgb = v;
}