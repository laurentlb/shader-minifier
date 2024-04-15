#version 450

// https://github.com/laurentlb/Shader_Minifier/issues/320
float f0() {
    float a = 1., b = 1.;
    a == 1. ? b = 2. : b = 3.;
    return b; // 2
}

float f1() {
    float a = 1., b = 1.;
    a = true ? b = 2. : b = 3.;
    return a;
}
