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
float A1() { return shadowedVar; }
float A2_INLINED() { return notShadowedVar; }
float A3() { float a = 10.; return shadowedFunc(a); }
float A4_INLINED() { return notShadowedFunc(globalFloat); }

float B1() { return 2.0; }
float B2_INLINED() { return 2.0; }

float C1() { float x = sin(0.); x++; return 3.0 + x; }
float C2_INLINED() { return 3.0 + sin(0.); }

float D1(float d, float dd) { return d+d; }
float D2_INLINED(float d, float dd) { return d+dd; }

float glob;
float watchout(out float x) { return x = 9.0; }
float inuit(inout float greenland) { return greenland++; }
float E1(float bad1, float ok) { return bad1++; }
float E2_INLINED(float ok2a, float ok2b) { return ok2a + watchout(glob) - glob-- * (glob *= ok2b) + inuit(glob); }
float E3(float bad3, float ok) { return bad3+=1.0; }
float E4(float bad4, float ok) { return watchout(bad4); }
float E5(float bad5, float ok) { return inuit(bad5); }

float F1_INLINED(in float f) { return 7.0; }
float F2(out float ff) { return 7.0; }
float F3(inout float f) { return 7.0; }

float G1(float g1, float g2) { return 7.0; }

float f() {
    // [A] Only inline a function if it never refers to a global function or variable by a name that is hidden by a local variable in scope at the call site.
	float shadowedVar = -1.0, shadowedFunc = -2.0;
	float _A1 = A1(); // not inlined
	float _A2 = A2_INLINED(); // inlined
	float _A3 = A3(); // not inlined
	float _A4 = A4_INLINED(); // inlined
	
	int sep;
	
    // [B] Only inline a function if it has only one call site.
    float _B1 = B1() + B1(); // not inlined
    float _B2 = B2_INLINED() * 2.0; // inlined
	
	sep++;
    // [C] Only inline a function if it is a single expression return.
	float _C1 = C1(); // not inlined
	float _C2 = C2_INLINED(); // inlined

	sep++;
    // [D] Only inline a function if it uses its 'in' parameters at most once.
	float _D1 = D1(4.0, 5.0); // not inlined
	float _D2 = D2_INLINED(4.0, 5.0); // inlined

	sep++;
    // [E] Only inline a function if its 'in' parameters are never written to (through assignOps or calling an out or inout function or operator).
	float _E1 = E1(6.0, 10.0); // not inlined
	float _E2 = E2_INLINED(6.0, 10.0); // inlined
	float _E3 = E3(6.0, 10.0); // not inlined
	float _E4 = E4(6.0, 10.0); // not inlined
	float _E5 = E5(6.0, 10.0); // not inlined

	sep++;
    // [F] Only inline a function if it has no 'out' or 'inout' parameters.
	float _F1 = F1_INLINED(7.0); // inlined
	float o; float _F2 = F2(o); // not inlined
	float _F3 = F3(o); // not inlined

	sep++;

	shadowedVar++;
	shadowedFunc++;
	return // ensure things are not unused
		shadowedVar+shadowedFunc+
		_A1+_A2+_A3+_A4+
		_B1+_B2+
		_C1+_C2+
		_D1+_D2+
		_E1+_E2+_E3+_E4+_E5+
		_F1+_F2+_F3;
}


float Z() { return 0.0; }	
float g() {
	float Z=Z(); // should be inlined but there's a bug, using "not in env.vars" won't see the fn?
	return 1.;
}