void main()
{
float f1 = 1.5;
float f2 = .0000003;
float f3 = .42;
float f4 = .0;
float f5 = 0.;
float f6 = -2e-3;
float f7 = 2E-9;
float f8 = 2E+6;
float f9 = 2e10;

float n = 1. / (f1 + f2 + f3 + f4 + f5);
float o = 1. / (f6 * f8 * f7 * f9);

gl_FragColor=vec4(n,o,n,0.);
}
