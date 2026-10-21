#version 300 es
precision highp float;

// Only called from a global initializer: must not be removed.
float f() { return 2.0; }

// Always called with the same argument: the parameter must not be removed,
// because the call in the global initializer would keep the argument.
float g(float x) {
    float y = x * x;
    return y + sin(y);
}

float a = f();
float b = g(1.5);

out vec4 o;
void main() {
    o = vec4(a, b + g(1.5), 0.0, 1.0);
}
