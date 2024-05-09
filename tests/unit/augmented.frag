#extension GL_EXT_gpu_shader4 : enable

int foo(int x, int y) {
    int a=x;
    a=a+x;
    a=a*y;
    a=a>>x;
    a=a^y;
    a=a|y;
    return a;
}

int output;

void main() {
    output = foo(0, 0);
}
