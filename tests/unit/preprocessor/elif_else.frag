#version 330
#define X 1

#if 1
int a_kept;
#elif 0
int a_removed1;
#else
int a_removed2;
#endif

#if 0
int b_removed;
#elif X
int b_kept;
#elif 1
int b_removed2;
#else
int b_removed3;
#endif

#if 0
int c_removed;
#elif 0
int c_removed2;
#else
int c_kept;
#endif

// GL_ES is unknown, but the first branch is already taken.
#if 1
int d_kept;
#elif GL_ES
int d_removed;
#else
int d_removed2;
#endif

void end() {}
