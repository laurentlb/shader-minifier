bool success() { return true; }
bool fail() { return false; }

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
  int a = -2;
  return a && success();
}

bool and2() {
  int a = 0;
  return a && fail();
}

bool and3() {
  return success() && true;
}
