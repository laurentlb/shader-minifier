// manual inlining (i_ prefix)

float i_foo(float f, vec3 g) {
  return f*g.x + g.y;
}

float a() {
  return i_foo(1.0, vec3(2.0, 3.0, 4.0));
}

float b(float g) {
  float x = i_foo(g + 10.0, vec3(20.0, 30.0, 40.0));
  float y = i_foo(g + 10.1, vec3(20.1, 30.1, 40.1));
  return x + y;
}

float i_bar(float f, float g) {
  return f*g + 1.0;
}

float c() {
  return i_bar(i_bar(2.0, 3.0), i_bar(4.0, 5.0)) + 6.0;
}

float d() {
  float f = i_bar(2.0, 3.0);
  float g = i_bar(f, f);
  return g + 4.0;
}

float i_multipass(float x) {
  float f = 42.0;
  float g = 2.0;
  return f*g*x;
}

float e() {
  return i_multipass(1.0);
}

float globalFloat = 6.;

// automatic inlining

float shadowedFunc(inout float notinlinable) { notinlinable = 1.; return -1.0; }
float notShadowedFunc(inout float notinlinable) { notinlinable = 2.; return -2.0; }
float shadowedVar = 0.0;
float notShadowedVar = 1.0;
float A1_PRESERVED() { return shadowedVar; }
float A2_INLINED() { return notShadowedVar; }
float A3_PRESERVED() { float a = 10.; return shadowedFunc(a); }
float A4_INLINED() { return notShadowedFunc(globalFloat); }

float B1_PRESERVED(float x) { return pow(2.0, x); }
float B2_INLINED() { return 2.0; }

float C1_PRESERVED() { float x = sin(0.); x++; return 3.0 + x; }
float C2_INLINED() { return 3.0 + sin(0.); }

float D1_INLINED(float d, float dd) { return d+d; }
float D2_INLINED(float d, float dd) { return d+dd; }
float D3_PRESERVED(float d, float dd) { return d+d; }

float glob;
float watchout(out float x) { return x = 9.0; }
float inuit(inout float greenland) { return greenland++; }
float E1_PRESERVED(float bad1, float ok) { return bad1++; }
float E2_INLINED(float ok2a, float ok2b) { return ok2a + watchout(glob) - glob-- * (glob *= ok2b) + inuit(glob); }
float E3_PRESERVED(float bad3, float ok) { return bad3+=1.0; }
float E4_PRESERVED(float bad4, float ok) { return watchout(bad4); }
float E5_PRESERVED(float bad5, float ok) { return inuit(bad5); }

float F1_INLINED(in float f) { return 7.0; }
float F2_PRESERVED(out float ff) { return 7.0; }
float F3_PRESERVED(inout float f) { return 7.0; }

#pragma function inline
float PRAGMA_INLINED(float x) { return vec3(x).x; }
#pragma function noinline
float PRAGMA_PRESERVED(float x) { return x; }

float setup() {
	return shadowedVar++; // prevent inlining of the global shadowedVar
}

float f() {
    // [A] Only inline a function if it never refers to a global function or variable by a name that is hidden by a local variable in scope at the call site.
	float shadowedVar = -1.0, shadowedFunc = -2.0;
	float _A1 = A1_PRESERVED(); // not inlined
	float _A2 = A2_INLINED(); // inlined
	float _A3 = A3_PRESERVED(); // not inlined
	float _A4 = A4_INLINED(); // inlined
	
	int sep;
	
    // [B] Only inline a function if it has only one call site.
    float _B1 = B1_PRESERVED(3.) + B1_PRESERVED(4.); // not inlined
    float _B2 = B2_INLINED() * 2.0; // inlined
	
	sep++;
    // [C] Only inline a function if it is a single expression return.
	float _C1 = C1_PRESERVED(); // not inlined
	float _C2 = C2_INLINED(); // inlined

	sep++;
    // [D] Only inline a function if it uses its 'in' parameters at most once.
	float four = 4.0, five = 5.0;
	float _D1 = D1_INLINED(four, five); // not inlined
	float _D2 = D2_INLINED(four, five); // inlined
	float _D3 = D3_PRESERVED(four + 1.0, sin(five)); // not inlined
	four++, five++; // prevent variable inlining and argument inlining into D1 and D2.

	sep++;
    // [E] Only inline a function if its 'in' parameters are never written to (through assignOps or calling an out or inout function or operator).
	float six = 6.0, ten = 10.0;
	float _E1 = E1_PRESERVED(six, ten); // not inlined
	float _E2 = E2_INLINED(six, ten); // inlined
	float _E3 = E3_PRESERVED(six, ten); // not inlined
	float _E4 = E4_PRESERVED(six, ten); // not inlined
	float _E5 = E5_PRESERVED(six, ten); // not inlined
	six++, ten++; // prevent variable inlining and argument inlining.

	sep++;
    // [F] Only inline a function if it has no 'out' or 'inout' parameters.
	float _F1 = F1_INLINED(7.0); // inlined
	float o; float _F2 = F2_PRESERVED(o); // not inlined
	float _F3 = F3_PRESERVED(o); // not inlined

	sep++;
	float _P1 = PRAGMA_INLINED(9.0)+PRAGMA_INLINED(8.0);
	float _P2 = PRAGMA_PRESERVED(9.0);

	setup();
	shadowedVar++;
	shadowedFunc++;
	return // ensure things are not unused
		shadowedVar+shadowedFunc+four+five+six+ten+
		_A1+_A2+_A3+_A4+
		_B1+_B2+
		_C1+_C2+
		_D1+_D2+_D3+
		_E1+_E2+_E3+_E4+_E5+
		_F1+_F2+_F3+
		_P1+_P2;
}


float Z() { return 0.0; }	
float g() {
	float Z=Z(); // should be inlined but there's a bug, using "not in env.vars" won't see the fn?
	return 1.;
}