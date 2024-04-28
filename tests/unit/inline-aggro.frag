// *** Things we should inline

float inl1() {
	float f = 42.0;
	float g = 2.0 * f;
	return f + g;
}

float inl2(float x) {
	float f = 123456789.0;
	if (x > 2.0*f) return 100.0;
	if (x > f) return 101.0;
	return 102.0;
}

float inl3() {
	const float f = acos(-1.0);
	const float g = 2.0*f;
	return 0.75*g;
}

float inl4() {
	const float f = acos(-1.0), g = 2.0*f;
	return 0.75*g;
}

const float foo = 123.0;
float inl5() {
	float f = 456.0;
	return foo + f;
}

const float bar = acos(-1.0);
const float baz = 2.0*bar;
float inl6() {
	return 2.0 * baz;
}

float notevil(in float x, float y) { x = 42.0; return x + (y *= y); }
float inl7(float u) {
	float f = 101.0;
	float z = notevil(f, u);
	return f + z;
}

int inl8(in ivec3 x) {
	int i = 1;

	// i is not technically in an lvalue position but not smart enough right now to see that.
	x[i] += 1;
	return x[i] + i;
}

// *** Things we shouldn't inline

float noinl1() {
	float f = 42.0;
	f = 101.0;
	return f;
}

float noinl2() {
	float f = 42.0;
	f++;
	return f;
}

float noinl3() {
	float f = 42.0;
	if (acos(-1.0) < 3.0) {
		f = 101.0;
	}
	return f;
}

float noinl4() {
	float f = 42.0;
	for (f = 0.0; false;);
	return f;
}

float noinl5() {
	float f = 42.0;
	for (; false; f++);
	return f;
}

float noinl6(float x) {
	float f = x + 1.0;
	x = 100.0;
	return f + 2.0;
}

float noinl7(const float x) {
	return x + 1.2;
}

float quux = 1.0;
float noinl8() {
	return quux + 2.0;
}

float noinl9(float x) {
	float bar = x + 1.0; // Shadows inlinable const above
	x = 100.0;
	return bar + 2.0;
}

void evil(inout float x) { x = 42.0; }
float noinl10() {
	float f = 101.0;
	evil(f);
	return f;
}

int noinl11(in ivec3 x) {
	int i = 1;
	x[i++] += 1;
	return x[i] + i;
}

int noinl12() {
	int i = 10;
	while (--i > 0) {
	}
	return 1;
}
