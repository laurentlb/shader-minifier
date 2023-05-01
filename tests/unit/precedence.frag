float foo(bool a, bool b, bool c) {
  return (a?b:c)?.1:.2;
}

void main()
{
float f1 = 2. * 3. / 4. - 5. / 6.;
float f2 = (true ? false ? 1. : 2. : 3. * 4.) * 5.;
float f3 = 4. * (false ? (true ? 1. : 2.) : 3.);
float f4 = float((2+3) * (4+5*6) - (7-8));
float f5 = (2.+f1) * (4.+f2*6.) - (7.-f3);

float n = 1. / (f1 + f2 + f3);
float o = 1. / (f4 * f5);

gl_FragColor=vec4(n,o,n,0.);
}
