#version 330
// Names starting with GL_ or __ are defined by the implementation: they are unknown.
#define ONE 1
#define TWO 2

#ifdef GL_ES
precision mediump float;
#endif

#if GL_ES && 0
int removed_short_circuit;
#endif

#if GL_ES || ONE
int kept_short_circuit;
#endif

// The condition is kept, with the macros expanded.
#if __VERSION__ > TWO * 100
int kept_unknown_expanded;
#endif

#if 0
int removed;
#elif GL_ES
int unknown_elif_becomes_if;
#else
int unknown_else;
#endif

#if GL_ES
int unknown_if;
#elif 1
int unknown_elif_true_becomes_else;
#elif GL_ES
int removed_after_true;
#else
int removed_else_after_true;
#endif

#if GL_ES
#  if 1
int nested_known_in_unknown;
#  else
int removed_nested;
#  endif
#endif

// A macro defined in an unknown region becomes unknown: all its #define lines are kept.
#define MAYBE 1
#if GL_ES
#  undef MAYBE
#  define MAYBE 2
#  define NEW
#endif
int use_maybe = MAYBE;
#if MAYBE == 2
int maybe_two;
#endif
#ifdef NEW
int maybe_new;
#endif

void end() {}
