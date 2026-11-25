#version 450
out vec4 o;

float foo(float a, float b) {
    return a * 10.0 + b;
}

void main() {
    float a = 1.0;
    float b = 2.0;
    float c = 3.0;
    float d = 4.0;

    float r1 = foo(true ? a, b : c, d);
    float r2 = foo(true ? a : b, c);

    o = vec4(r1, r2, 0.0, 1.0);
}
