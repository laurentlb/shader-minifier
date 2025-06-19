struct MyStructT{
  float f;
  static MyStructT Create(float f_in)
  {
    MyStructT my_struct;
    my_struct.f = f_in * 10;
    return my_struct;
  }
};

float Foo(float x)
{
  MyStructT my_struct = MyStructT::Create(x);
  return my_struct.f;
}