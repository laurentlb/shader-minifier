#version 330
  #define INDENTED 1
	#if INDENTED
int kept_indented;
  #endif
#
#pragma optimize(on)
#extension GL_GOOGLE_include_directive : enable
/*
#if 1
int commented_out;
#endif
*/
#define COMMENTED 1 // trailing comment
#define BLOCK /* comment */ 2
int a = COMMENTED + BLOCK;
#define MULTI 1 + \
    2
int b = MULTI;
int c = INDENTED; // INDENTED in a comment
