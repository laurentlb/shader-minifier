#version 120

float a[3+2] = float[5](3.4, 4.2, 5.0, 5.2, 1.1);

const int size = 5;
float b[size] = float[size](3.4, 4.2, 5.0, 5.2, 1.1);

float c[] = float[](3.4, 4.2, 5.0, 5.2, 1.1);
float d[5] = float[](3.4, 4.2, 5.0, 5.2, 1.1);
float e[] = float[5](3.4, 4.2, 5.0, 5.2, 1.1);

void arrayTypes() {
	vec4 a[3];
	vec4[2] b[3];
	vec4[3] c;

	int code[] = int[1](123);
}
