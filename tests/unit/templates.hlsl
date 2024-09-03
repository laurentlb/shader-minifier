template <typename T>
struct literal {
  T value;
};

template <typename T>
literal<T> DoSomething(T arg, literal<T> b) {
  T a = arg;
  literal<T> c = b;
  return c;
}
