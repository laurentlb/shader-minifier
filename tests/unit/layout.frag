#version 330

// #486
layout(       location   =  1  )     out vec4 f_color2;

// #17
layout(location=0) out vec4 f_color;
//subroutine float functionType(int i);
//layout(location = 0) subroutine uniform functionType tbl[2];
//layout(index = 1) subroutine (functionType) float func1(int i) { return i * 1.0/255.0; }
//layout(index = 2) subroutine (functionType) float func2(int i) { return 1.0 - i * 1.0/255.0; }

//lowp float foo(lowp float bar) { return 1.0; }
float foo(lowp float bar) { return 1.0; }