void a();
void b();
void c();
void d();

void a() {
  b();
}

void b() {
  c();
  d();
}

void e() {
  a();
  c();
}

int x;
void c() {x;}

void d() {x;}
