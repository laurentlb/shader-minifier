#version 130

uint large() { return 3812015801U; }

vec4 f()
{
float oct = float(042) / 1000.;
float oct2 = float(-071) / 1000.;
int dec = 65535;
int dec2 = -65536;
float leadingZeros = 01. + 0001.42321 + 007e1 + 09.5;
float n = oct - oct2 + float(4 * 0x0) + float(dec + dec2) / 20. + leadingZeros;
return vec4(n,n,n,0.);
}
