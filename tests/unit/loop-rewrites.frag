#version 330
// Rewrites of while loops, if/else and simple expression simplifications.
uniform float u;
float g;
out vec4 fragColor;

void s(float x) { g += x; }

// while(c){a();b();i++;}  ->  for(;c;i++){a();b();}
float severalStatements(float x) {
  float i = 0.;
  while (i < x) { s(i); s(i * 2.); i += 1.; }
  return i;
}

// The last statement can't become the for increment when the loop has a continue.
float withContinue(float x) {
  while (x < 10.) { if (x > 5.) continue; x += 1.; }
  return x;
}

// A switch in the body is searched for continue.
float withSwitch(float x) {
  while (x < 20.) {
    switch (int(x)) { case 1: x += 2.; break; default: x += 1.; }
    x *= 2.;
  }
  return x;
}

// A switch containing a continue (that applies to the loop).
float withContinueInSwitch(float x) {
  while (x < 20.) {
    switch (int(x)) { case 1: x += 2.; continue; default: x += 1.; }
    x *= 2.;
  }
  return x;
}

// Single statement body, preceded by an assignment that goes in the for init.
float singleStatement(float x) {
  float i;
  i = x;
  while (i < 10.) i += u;
  return i;
}

// The body is neither a block nor an expression.
float loopBody(float x) {
  while (x < 10.) for (int i = 0; i < 2; i++) x += u;
  return x;
}

// if(c)a();else{}  ->  if(c)a();
float emptyElse(float x) {
  if (x > u) s(1.); else {}
  return g;
}

float expressions(float x) {
  float a = x - (1. - 3.5); // x - -2.5  ->  x + 2.5
  float b = (x, (a, u));    // a,(b,c)  ->  (a,b),c
  return a + b;
}

// Consecutive assignments to a builtin variable are kept.
void builtinAssignment() {
  fragColor = vec4(u);
  fragColor = vec4(g);
}

// Same with a variable that is not declared in the shader.
void builtinAssignment2() {
  gl_FragDepth = u;
  gl_FragDepth = g;
}

void main() {
  builtinAssignment();
  builtinAssignment2();
  fragColor += vec4(severalStatements(u), withContinue(u), withSwitch(u), withContinueInSwitch(u));
  fragColor += vec4(singleStatement(u), emptyElse(u), expressions(u), loopBody(u));
}
