void main()
{
float oct = float(042) / 1000.;
float oct2 = float(-071) / 1000.;
int dec = 65535;
int dec2 = -65536;
float n = oct - oct2 + float(4 * 0x0) + float(dec + dec2) / 20.;
gl_FragColor=vec4(n,n,n,0.);
}
