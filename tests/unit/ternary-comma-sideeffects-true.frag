#version 450
out vec4 o;

void main() {
    float counter = 0.0;

    float result = true
        ? (counter += 1.0, counter += 2.0)
        : (counter += 4.0);

    o = vec4(result, counter, 0.0, 1.0);
}
