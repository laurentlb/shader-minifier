struct SomeNamedStruct {
  int SomeFunction() {
    return 5;
  }

  float member_x = 3.9;
  float member_y = 4.2;

  float GetX(int off) {
    return member_x + off;
  }
};

float Foo(float x) {
  struct {
    float member_p;

    float GetP() {
      return member_p;
    }
  } some_anonymous_struct;
  
  some_anonymous_struct.member_p = 4;
  return some_anonymous_struct.GetP() * x;
}