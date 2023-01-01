int f(int x) {
#if FOO
    return x;
#else
    return x + 2;
#endif
}

void main() {
    int a = 2;
    f(a);
    return;
    f(a + 1);
}
