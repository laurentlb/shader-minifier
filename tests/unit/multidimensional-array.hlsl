// All different sizes will not be merged
float a[4][5];
float b[SIZE_X][SIZE_Y];
UserT c[SIZE_X][3];

// Neighboring declarations with the same size will be merged
float d[4][3];
float e[4][3];

// Non-neighboring mergable declarations are not merged
float f[5][2];
float g[9][15];
float h[5][2];