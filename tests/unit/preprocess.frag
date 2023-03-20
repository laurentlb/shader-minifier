#version 130

#define DEF bar
#define foo() bar
#

#ifdef DEF
void keep_ifdef() {}
#endif

#ifdef UNKNOWN
void remove_ifdef() {}
#endif

#ifdef DEF
# ifdef DEF // nested
#  ifdef UNKNOWN
#    ifdef DEF
int remove_this_line;
#    endif
int remove_this_line_too;
#  endif
void keep_nested() {}
# endif
void keep_outernest() {}
#endif

#ifdef UNKNOWN
int this_is_removed;
#else
void keep_else() {}
#endif

void end() {}
