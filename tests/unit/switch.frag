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
      float anotherVar = 4.3;
      return anotherVar + 3.4;
    default:
      return 0.0;
  }
}

const int LBL1 = 1, LBL2 = 2, LBL3 = 3, LBL4 = 4, LBL5 = 5;
void foo() {}

int switchStringLabels(int value)
{
  switch (value)
  {
    case LBL1: foo();
    case LBL2: foo();
    case LBL3: { foo(); break; }
    case LBL4: foo(); foo(); break;
    case LBL5: foo(); foo(); return 18;
  }
  return 1;
}
