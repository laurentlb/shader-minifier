#version 330
// Unsized arrays in the type.
uniform int i;
out vec4 O;
void main() {
    int[] letters = int[](99, 114, 105, 109, 115, 111);
    float[] a = float[](1., 2.), b = float[](3., 4.);
    O = vec4(letters[i], a[i], b[i], letters.length());
}
