#version 330
// Everything inside a dead region is removed, including nested directives.
#if 0
#  ifdef X
int removed1;
#  else
int removed2;
#  endif
#  if 1
int removed3;
#  endif
#  if GL_ES
int removed4;
#  elif 1
int removed5;
#  endif
#  define DEAD 1
#  undef LIVE
#  pragma whatever
#endif

// Defines in a dead region are ignored.
#ifdef DEAD
int removed_dead_define;
#endif

#define LIVE 1
#if 0
#undef LIVE
#endif
#ifdef LIVE
int kept_live;
#endif

#ifdef X
#elif 1
int kept_after_empty_branch;
#endif

void end() {}
