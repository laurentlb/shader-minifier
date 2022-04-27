void switchConst() {
  switch (42) {
    case 42:
      break;
  }
}

#define GOOD 42
void switchDefine() {
  switch (42) {
    case GOOD:
      break;
  }
}

void switchArg(in int someArg) {
  switch (someArg) {
    case 0:
      break;
  }
}

void switchExpr(in int someArg) {
  switch (someArg + 42) {
    case 0:
      break;
  }
}

float switchMultiple() {
  switch (42) {
    case 42:
      float someVar = 4.2;
      float otherVar = 2.4;
      return someVar + otherVar;
    case 43:
      float someVar = 4.3;
      return someVar + 3.4;
    default:
      return 0.0;
  }
}
