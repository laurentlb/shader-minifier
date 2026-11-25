#version 450
out vec4 o;

void main() {
    float a = 1.0;
    float b = 2.0;

    float result = 0.0;

    result = (true ? a : b), result += 3.0;

    o = vec4(result);
}
