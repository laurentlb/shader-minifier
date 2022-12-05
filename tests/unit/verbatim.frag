//[
// Test case for https://github.com/laurentlb/Shader_Minifier/issues/168
//]

//[
int foo(int x) {
  return 2;
}
//]

int my_function() {
  int i = 2;
//[
foo(1   +   1  )  ;
//]
  return i;
}

int verbatim_with_newlines() {
//[


int        x;



int     y;



//]
  return x + y;
}
