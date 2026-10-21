#version 330
// Tested with --preprocess-no-expand KEEP,F,PAREN
#define KEEP 1
#define BASE 2.
#define F(x) ((x) * BASE)
#define G(x) F(x)
#define PAREN (3.) // object-like: the space must be kept

float f() { return float(KEEP) + F(1.) + G(2.) + PAREN; }

// Macros that are not expanded are still evaluated.
#if KEEP
int kept_known_value;
#else
int removed;
#endif

#undef KEEP
#ifdef KEEP
int removed_undef;
#endif

void end() {}
