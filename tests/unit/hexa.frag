void main()
{
float n = float(0xff) / 1000.;
float o = float(-0XFF) / 1000.;
float p = n - o + float(4 * 0x0);
gl_FragColor=vec4(.2,.4,n,0.);
}
