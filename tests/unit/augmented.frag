int foo(int x, int y) {
    int a=x;
    a=a+x;
    a=a*y;
    a=a>>x;
    a=a^y;
    a=a|y;
    return a;
}

void main() {
    foo(0, 0);
}
