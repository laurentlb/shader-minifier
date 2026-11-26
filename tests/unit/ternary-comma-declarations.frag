#version 450
out vec4 o;

void main() {
    float a = 1.0;
    float b = 2.0;
    float c = 3.0;

    float h = true ? a, b : c, k = 5.0;

    o = vec4(h, k, 0.0, 1.0);
}
