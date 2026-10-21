#version 330
#define DEBUG 0
#define LEVEL 2
#define FEATURE

#if DEBUG
int removed_debug;
#endif

#if !DEBUG
int kept_not_debug;
#endif

#if LEVEL > 1 && defined(FEATURE)
int kept_and;
#endif

#if LEVEL == 3 || defined NOT_DEFINED
int removed_or;
#endif

#if (LEVEL * 2 + 1) % 4 == 1 // comment
int kept_arith;
#endif

#if UNDEFINED_NAME
int removed_undefined_is_zero;
#endif

#if LEVEL >= 2 \
    && LEVEL < 10
int kept_continuation;
#endif

#if 0x10 == 16 && (1 ? 2 : 3) == 2 && -1 < 0 && (1 << 3) == 8 && 7 / 2 == 3
int kept_operators;
#endif

#ifndef DEBUG
int removed_ifndef;
#endif

#ifdef FEATURE
int kept_ifdef;
#endif

#if DEBUG /* block comment */
int removed_block_comment;
#endif

#if 1
int kept_if_1;
#endif

#if 0
this is not valid code and is removed
#endif

void end() {}
