#version 330
// Function-like macros are expanded and their #define is removed.
#define _TV(name, value) (value)
#define SQ(x) ((x)*(x))
#define ADD(a, b) ((a)+(b))
#define ZERO() 0.
#define ID(x) x
#define NEGATE(x) -x
#define CONST 5.

float f_tv() { return _TV(speed, 2.5) * _TV(scale, vec2(1., 2.)).x; }
float f_nested() { return SQ(ADD(1., 2.)); }
float f_parens() { return ADD(min(1., 2.), max(3., 4.)); }
float f_zero() { return ZERO() + ZERO( ); }
float f_multiline() {
    return ADD(
        1.,
        2.);
}
float f_macro_arg() { return SQ(CONST); }
float f_negate() { return NEGATE(-1.); } // must not become --1.
float f_rescan() { return ID(SQ)(3.); }

// A function-like macro name without parentheses is not expanded.
float ID = 1.;
float f_no_call() { return ID; }
