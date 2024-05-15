precision highp float;
void main()
{
  int x=1;
  for (int i=0;i<3;i++) {
    x+=1;
  }
  gl_FragColor.x=float(x);
}
