#version 330
// Object-like macros are expanded and their #define is removed.
#define N 3
#define PI 3.14159
#define TAU (2.*PI)
#define EMPTY
#define NEG -N

float f_basic() { return PI * float(N); }
float f_nested() { return TAU; }
float f_empty() { return 1. EMPTY; }
int f_neg() { return -NEG; } // must not become --3
float f_number() { return 1e3 + float(N); } // 1e3 is not split
int NN = 4;
int f_boundaries() { return NN + N; } // NN is a different identifier

// Redefinition and #undef.
#define V 1
int f_v1() { return V; }
#define V 2
int f_v2() { return V; }
#undef V
int V = 0;
int f_v3() { return V; }

// Recursive macros stop expanding.
#define self self
int self = 1;
#define A B
#define B A
int A = 2;
int f_recursive() { return self + A; }
