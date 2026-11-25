#version 450
out vec4 o;

void main() {
    float a = 1.0;
    float b = 2.0;
    float c = 3.0;
    float d = 4.0;

    float r1 = true ? a : false ? b, c : d;
    float r2 = false ? a : true ? b, c : d;

    o = vec4(r1, r2, 0.0, 1.0);
}
