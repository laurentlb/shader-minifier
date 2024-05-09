int noinline;
bool success() { noinline++; return true; }
bool fail() { noinline++; return false; }

// After optimization, no call to fail should remain.

bool ternary() {
  return 1 < 2 ? success() : fail();
}

bool ternary2() {
  return 2 < 1 ? fail() : success();
}

bool or() {
  return true || fail();
}

bool or2() {
  return false || success();
}

bool or3() {
  return success() || false;
}

bool and() {
  const bool a = true;
  return a && success();
}

bool and2() {
  const bool a = false;
  return a && fail();
}

bool and3() {
  return success() && true;
}

const bool debug = false;

int foo() {
  int a = 1;
  if (debug) {
    a = 2;
  }
  return a;
}

int glo;
int sideEffect(int n)
{
	return glo += n;
}

// test of "turn if-else into ternary"
float ifStmtToExpr(float f) {
	float r;
	if (f > 0.0)
		r = sideEffect(1);
	else
		r = sideEffect(2);
	float r2;
	if (f > 1.0) {
		r2 = sideEffect(1);
    } else {
		sideEffect(99);
		r2 = sideEffect(2);
	}
	float r3;
	if (f > 1.0) {
		r3 = sideEffect(1);
    } else {
		r3 = sideEffect(2);
		sideEffect(99);
	}
	return r+r2+r3;
}