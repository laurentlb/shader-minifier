#version 130

#define DEF 1

#if 1
void keep_if() {}
#endif

#if 0
void remove_if() {}
#endif

#if DEF
#  if 0
int remove_this_line;
#  elif 1
void keep_nested() {}
#  endif
void also_keep() {}
# endif

void end() {}
