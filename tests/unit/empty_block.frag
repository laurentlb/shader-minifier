void more_semicolons(){}

int f() {
  int x = 5;
  int y = 7;
  
  if (x < 3) {
    return 2;
  } else {;}

  if (x == 5) {
     ;
  } else {}

  if (y == 1); else;

  more_semicolons();;;
  return 3;
}
