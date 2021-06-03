int sum0() { return 0; }

int sum1(int x) { return sum0() + x; }

int sum2(int x, int y) { return sum1(x) + y; }

int sum3(int x, int y, int z) { return sum2(x, y) + z; }

int sum4(int x, int y, int z, int w) { return sum3(x, y, z) + w; }

int mult1(int x) { return x; }

int mult2(int x, int y) { return mult1(x) * y; }

int mult3(int x, int y, int z) { return mult2(x, y) * z; }
