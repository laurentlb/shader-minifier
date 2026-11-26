#version 450
out vec4 o;

void main() {
    float a = 1.0;
    float b = 2.0;
    float c = 3.0;

    float result = false ? a : (b, c);

    o = vec4(result);
}
