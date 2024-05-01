#version 120

precision highp float;
precision mediump int;
precision lowp sampler2D;
precision lowp samplerCube;

layout(triangles) in; // #29
layout(triangle_strip, max_vertices = 3) out; // #29
layout(local_size_x = 32) in; // #22

// #17
layout(location=0) out vec4 f_color;
//subroutine float functionType(int i);
//layout(location = 0) subroutine uniform functionType tbl[2];
//layout(index = 1) subroutine (functionType) float func1(int i) { return i * 1.0/255.0; }
//layout(index = 2) subroutine (functionType) float func2(int i) { return 1.0 - i * 1.0/255.0; }
