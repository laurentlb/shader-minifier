#version 330
// Tested with -D RELEASE=1 -D FAST

#if RELEASE
int kept_release;
#else
int removed_debug;
#endif

#ifdef FAST
int kept_fast;
#endif

#if RELEASE == 2
int removed;
#endif

float f() { return float(RELEASE); }
