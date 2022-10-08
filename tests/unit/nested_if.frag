float getNum(float a) {
  // else statement, so curly braces are required
  if (a > 0.0) {
    if (a > 1.0) return 1.0;
  }
  else return 0.0;
  return a;
}

int nested(bool x, bool y) {
  if (x) {
    // no else statement, so curly braces can be removed
    if (y) {
      return 0;
    }
  }
  return 2;
}

int no_braces1(int x) {
  if (x < 0) {
    for (;;)
      if (x > 0) {
        return 0;
      } else {
        return 1;
      }
  } else {
    return 2;
  }
}

int dandling_else(int x) {
  if (x < 0) {
    for (;;)
      if (x > 0) {
        return 0;
      } else {
        if (x == 0) return 1; // dangling
      }
  } else {
    return 2;
  }
}
