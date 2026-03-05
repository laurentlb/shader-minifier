// Generated with  (https://github.com/laurentlb/Shader_Minifier/)




#ifdef INCLUDE_leizex_frag
#extension GL_EXT_gpu_shader4:enable

uniform vec2 x;
uniform float v;
uniform sampler2D y,f,z,m;
float t(int v)
{
  v^=v<<13;
  v=v*(v*v*15731+789221)+1376312589&2147483647;
  return float(v);
}
float t(vec3 v,int f)
{
  ivec3 m=ivec3(floor(v));
  vec3 x=fract(v);
  x=x*x*(3.-2.*x);
  f+=m.x+m.y*57+113*m.z;
  float y=mix(mix(mix(t(f),t(f+1),x.x),mix(t(f+57),t(f+58),x.x),x.y),mix(mix(t(f+113),t(f+114),x.x),mix(t(f+170),t(f+171),x.x),x.y),x.z);
  return 1.-y*(1./1073741824.);
}
vec2 t(vec3 v)
{
  ivec3 m=ivec3(floor(v));
  vec3 x=fract(v);
  vec2 f=vec2(1);
  for(int v=-1;v<=1;v++)
    for(int y=-1;y<=1;y++)
      for(int i=-1;i<=1;i++)
        {
          int z=m.x+i+57*(m.y+y)+113*(m.z+v);
          vec3 s=vec3(float(i),float(y),float(v))-x+vec3(t(z),t(z+1217),t(z+2513))/2147483647.;
          float a=dot(s,s);
          if(a<f.x)
            f.y=f.x,f.x=a;
          else if(a<f.y)
            f.y=a;
        }
  return.25*sqrt(f);
}
float s(vec3 v)
{
  return.5*t(v,0)+.25*t(v*2.,0)+.125*t(v*4.,0)+.0625*t(v*8.,0);
}
float s(vec3 v,out float x)
{
  vec3 y=mod(vec3(1024)+v,1.)-.5;
  float f=sqrt(dot(y,y))-.09,m=t(4.*v,0);
  f+=.8*m;
  x=clamp(-1.5*m,0.,1.);
  vec2 i=t(16.*v);
  m=clamp(i[1]-i[0],0.,1.);
  f-=m;
  x*=clamp(m*12.,0.,1.);
  return f;
}
vec3 n(vec3 v)
{
  float x;
  vec3 y=vec3(s(v+vec3(2e-4,0,0),x)-s(v-vec3(2e-4,0,0),x),s(v+vec3(0,2e-4,0),x)-s(v-vec3(0,2e-4,0),x),s(v+vec3(0,0,2e-4),x)-s(v-vec3(0,0,2e-4),x));
  return normalize(y);
}
void n(out vec3 f,out vec3 x,vec2 m)
{
  float y=v;
  vec2 i=m;
  float z=i.x*i.x*.32+i.y*i.y;
  i=i*(7.-sqrt(37.5-11.5*z))/(z+1.);
  vec3 s=vec3(0,1.5,2);
  x=s+vec3(-sin(6.2831853*y/20.),.75*cos(6.2831853*y/20.+.5),-cos(6.2831853*y/20.));
  s+=.075*vec3(t(vec3(2.*y,0,.5),0),t(vec3(2.*y,.1,.4),7),t(vec3(2.*y,.2,.3),9));
  z=.1*t(vec3(2.*y,0,0),13);
  s=normalize(s-x);
  vec3 a=normalize(cross(s,vec3(0,cos(z),sin(z))));
  f=normalize(i.x*a+i.y*normalize(cross(a,s))+s);
}
vec3 n(vec3 v,vec3 y)
{
  float x=s(256.*y);
  return normalize(v+vec3(s(256.*(y+vec3(5e-4,0,0)))-x,s(256.*(y+vec3(0,5e-4,0)))-x,s(256.*(y+vec3(0,0,5e-4)))-x));
}
void main()
{
  vec2 v=-1.+2.*gl_FragCoord.xy/x.xy;
  vec3 y,f;
  n(f,y,v);
  float z,m;
  vec3 i,a;
  for(z=.1;z<5.;)
    {
      a=y+z*f;
      float v=s(a,m);
      if(v<.001)
        break;
      z+=v*.12;
    }
  i=n(a);
  y=vec3(1.+.5*s(96.*a));
  i=n(i,.25*a);
  y=y*(vec3(.5,.55,.6)+vec3(.6,.5,.3)*clamp(dot(i,vec3(.8,.5,-.1)),0.,1.)*3.)*m/(1.+z)+vec3(1.06,1.14,1)*(1.-exp2(-.25*z));
  y=clamp((y*y+y)*.5*vec3(1,1.2,1),0.,1.)*(.5+.5*(1.-v.x)*(1.+v.x));
  gl_FragColor=vec4(y,1);
}
#endif // leizex_frag

#ifdef INCLUDE_robin_frag
#define SUN_COLOUR vec3(1.,.76,.6)

vec4 i,r;
vec2 a;
vec4 c[2],d[2];
vec3 n(samplerCube v,vec3 x,float f)
{
  float y=max(max(abs(x.x),abs(x.y)),abs(x.z));
  f=(float(f)-1.)/float(f);
  if(abs(x.x)!=y)
    x.x*=f;
  if(abs(x.y)!=y)
    x.y*=f;
  if(abs(x.z)!=y)
    x.z*=f;
  return texture(v,x).xyz;
}
float n(float v)
{
  vec2 f=fract(vec2(v*5.3983,v*5.4427));
  f+=dot(f.yx,f.xy+vec2(21.5351,14.3137));
  return fract(f.x*f.y*95.4337);
}
float n(vec2 v)
{
  v=fract(v*vec2(5.3983,5.4427));
  v+=dot(v.yx,v.xy+vec2(21.5351,14.3137));
  return fract(v.x*v.y*95.4337);
}
float p(vec3 v)
{
  vec3 i=floor(v),x=fract(v);
  x=x*x*(3.-2.*x);
  vec2 f=textureLod(iChannel1,(i.xy+vec2(37,17)*i.z+x.xy+.5)/256.,0.).yx;
  return mix(f.x,f.y,x.z);
}
float s(vec3 v)
{
  float f=1.6*p(v);
  v*=2.02;
  f+=.35*p(v);
  v*=2.33;
  return f+.225*p(v);
}
vec2 n(vec2 v,float x)
{
  float y=sin(x);
  x=cos(x);
  return mat2(x,y,-y,x)*v;
}
float p(float v)
{
  float x=fract(v);
  v=floor(v);
  x=x*x*(3.-2.*x);
  return mix(n(v),n(v+1.),x);
}
float n(float v,float x)
{
  float f=fract(v);
  v=floor(v);
  f=smoothstep(0.,x,f);
  return mix(n(v),n(v+1.),f);
}
float n(vec3 v,float x)
{
  return length(v)-x;
}
vec3 p(vec3 v,vec3 x)
{
  vec3 y=texture(iChannel0,v.yz).xzy,f=texture(iChannel0,v.zx).xyz,m=texture(iChannel1,v.xy,2.).yyy;
  return(y*abs(x.x)+f*abs(x.y)+m*abs(x.z))/(abs(x.x)+abs(x.y)+abs(x.z))*.9;
}
float n(vec3 v,vec2 x)
{
  x=abs(vec2(length(v.xz),v.y))-x;
  return min(max(x.x,x.y),0.)+length(max(x,0.));
}
float n(vec3 v,vec3 x,vec3 y,float f,float z)
{
  v-=x;
  x=y-x;
  float m=clamp(dot(v,x)/dot(x,x),0.,1.);
  return length(v-x*m)-f+z*m;
}
float s(vec3 v,vec3 x)
{
  return length(max(abs(v)-x,0.))-.3;
}
float n(float v,float y,float x)
{
  float f=clamp(.5+.5*(y-v)/x,0.,1.);
  return mix(y,v,f)-x*f*(1.-f);
}
vec3 n(vec3 v,vec3 x,out float f)
{
  vec3 y=v;
  v.xz=n(v.xz,a.x);
  vec3 m=v;
  v.y-=r.z;
  v.yz=n(v.yz,r.x);
  vec3 z=p(v*.5,x).zxy*.45;
  f=0.;
  float e=n(s(v-vec3(0,-1.4,-.3),vec3(.3+.1*i.x,0,.1+.3*i.x)),n(v-vec3(0,-.25,0),1.33),2.2);
  vec3 t=v;
  t.x=abs(t.x);
  e=n(e,n(t,vec3(1.5,-.2,1.7),vec3(1.5,-1,-1.6),.3,.3),.4);
  vec3 c=vec3(.4,.32,.2)*texture(iChannel1,t.zy*vec2(.05,.3)).x;
  z=mix(c*c,z,clamp(n(t,vec3(1.3,.2,.2),vec3(1.5+r.y,-1,-1.6),.4,.4),0.,1.));
  c=vec3(.4)*texture(iChannel1,v.zy*vec2(.05,.3)).x;
  z=mix(c*c,z,clamp(n(t,vec3(.1,-.5,-1.5),vec3(.15,-1.2,-8),.2,.2),0.,1.));
  t=v;
  t.xy=n(t.xy,i.y);
  t.zy=n(t.zy,i.z);
  e=n(e,n(t-vec3(0,1,1.4),.8),1.);
  float w=min(n(v-vec3(0,-.9,2.1),1.5),n(t-vec3(0,.5,1.9),1.2));
  w+=s(v*20.)*.2;
  w=max(w,-n(t-vec3(0,2,1.2),1.1));
  z=mix(z,vec3(.4,.1,0)*(.8+s(v*12.)*.2),clamp((e-w)*3.-(1.-step(-3.,m.y))*3.,0.,1.));
  if(e>n(t-vec3(0,.9,2.5),vec3(0,0,-.4),vec3(0,i.w*.2,.4),.15,.1))
    f=.2,z=vec3(.02,.01,0);
  t.x=abs(t.x);
  if(n(t-vec3(.35,1.1,2.05),.11)<e)
    z=vec3(0),f=.25;
  c=mix(vec3(.2),vec3(.05,.04,0),s(y*2.)*.5);
  c=mix(c,vec3(.05),abs(sin(length(y.xz-vec2(.2,.4))*34.))*abs(x.y));
  z=mix(c,z,step(-3.4,y.y));
  m.y-=r.w;
  m.x=abs(m.x);
  w=1.-clamp(n(m,vec3(.5,-2.-r.z,-.5),vec3(.8,-3.5,.6),.53,.03)+(1.-step(-3.5,m.y))*50.,0.,1.);
  z=mix(z,vec3(.05,.02,.02),w);
  f=mix(f,.1,w);
  return z;
}
float e(vec3 v)
{
  float f;
  vec3 x=v;
  v.xz=n(v.xz,a.x);
  vec3 y=v;
  v.y-=r.z;
  v.yz=n(v.yz,r.x);
  f=n(s(v-vec3(0,-1.4,-.3),vec3(.3+.1*i.x,0,.1+.3*i.x)),n(v-vec3(0,-.25,0),1.33),2.2);
  vec3 m=v;
  m.x=abs(m.x);
  f=n(n(f,n(m,vec3(1.3,.2,.2),vec3(1.5+r.y,-.5,-1.6),.2,.2),.4),n(m,vec3(.6,-.5,-1.5),vec3(.15,-1.2,-8),.2,.2),2.4);
  v.xy=n(v.xy,i.y);
  v.zy=n(v.zy,i.z);
  f=n(n(n(f,n(v-vec3(0,1,1.4),.8),1.),n(v-vec3(0,1,2.5),vec3(0,0,-.4),vec3(0,i.w*.2,.15),.1,.096),.2),n(v-vec3(0,1.05,2.5),vec3(0,-i.w*.25,-1.5),vec3(0,-i.w,.16),.1,.1),.05);
  v.x=abs(v.x);
  f=min(f,n(v-vec3(.35,1.1,2.05),.11));
  f=min(f,n(x-vec3(.2,-12,.4),vec2(1.8,8.5)-s(x*5.)*.1));
  y.x=abs(y.x);
  y.y-=r.w;
  return min(min(min(min(f,n(y,vec3(.5,-2.+r.z+r.w,-.5),vec3(.8,-3.5,.5),.09,.1)),n(y,vec3(.8,-3.5,.5),vec3(.8,-3.5,1.2),.04,.04)),n(y,vec3(.8,-3.5,.5),vec3(1.4,-3.5,.8),.04,.04)),n(y,vec3(.8,-3.5,.5),vec3(.1,-3.5,.8),.04,.04));
}
float e(vec3 v,vec3 x)
{
  float y=1.,f=.01,m;
  for(int z=0;z<8;z++)
    m=e(v+x*f),y=min(2.5*m/f,y),f+=m*.5+.05;
  return max(y,0.);
}
vec3 e(vec3 v,vec3 y,vec3 x,vec3 f,float z,float m)
{
  vec3 i=normalize(vec3(-.1,.4,.3));
  float a=e(y,i);
  vec3 t=v*SUN_COLOUR*max(dot(i,x),0.)*a+v*n(iChannel3,x,64.)*.5;
  x=reflect(f,x);
  return t+max(dot(i,x),0.)*SUN_COLOUR*texture(iChannel3,x).xyz*m*a;
}
vec3 e(vec3 v,float x)
{
  vec2 f=vec2(.01,0);
  return normalize(vec3(e(v+f.xyy)-e(v-f.xyy),e(v+f.yxy)-e(v-f.yxy),e(v+f.yyx)-e(v-f.yyx)));
}
float e(float x)
{
  return x*.012;
}
float e(vec3 v,vec3 y,vec2 x)
{
  float f=2.+.1*n(x.xy*fract(iTime)),z=0.;
  vec3 i=vec3(0);
  int m=0;
  for(int x=0;x<40;x++)
    {
      if(m==8||z>=1.||f>45.)
        break;
      i=v+f*y;
      float t=e(f),a=e(i);
      if(a<t)
        {
          float v=max((1.-z)*min((t-a)/t,1.),0.);
          c[1].yzw=c[1].xyz;
          c[1].x=c[0].w;
          c[0].yzw=c[0].xyz;
          c[0].x=v;
          d[1].yzw=d[1].xyz;
          d[1].x=d[0].w;
          d[0].yzw=d[0].xyz;
          d[0].x=f;
          z+=v;
          m++;
        }
      f+=a*.8;
    }
  return clamp(z,0.,1.);
}
vec3 e(vec3 v,vec2 x)
{
  v=sqrt(v);
  
#define CONTRAST 1.1

  
#define SATURATION 1.2

  
#define BRIGHTNESS 1.15

  v=mix(vec3(.5),mix(vec3(dot(vec3(.2125,.7154,.0721),v*BRIGHTNESS)),v*BRIGHTNESS,SATURATION),CONTRAST)*(.5+.5*pow(1e2*x.x*x.y*(1.-x.x)*(1.-x.y),.4));
  return clamp(v,0.,1.);
}
vec3 s(float v)
{
  v+=5.;
  return vec3(1.4+sin(v)*3.5,-.2,5.-sin(v)*2.5);
}
float t(float v)
{
  float x=n(v*11.,.1)*abs(sin(v*14.))*.5;
  return x*smoothstep(.4,.9,n(v*.5+4.,.1));
}
void mainImage(out vec4 v,vec2 x)
{
  float y=iTime;
  vec2 f=x.xy/iResolution.xy,m=(-1.+2.*f)*vec2(iResolution.x/iResolution.y,1);
  float z=mod(y,40.);
  a.y=smoothstep(6.,0.,z)+smoothstep(39.,40.,z);
  vec3 w=s(y*.2);
  w.z+=a.y*8.;
  vec3 S=vec3(p(y*1.9-30.)*.5-.25,p(y*2.)*.5-.3,0),o=normalize(S-w),B=normalize(cross(o,vec3(0,1,0)));
  S=normalize(m.x*B+m.y*cross(B,o)+(1.-a.y*.4)*o);
  i.x=p(y*2.)+1.;
  i.y=(n(y*1.2+sin(y*.3)*.7+.7,.05)-.5)*1.5;
  i.z=n(y*.4-33.,.05)*.5;
  i.w=t(y)*.8+.02;
  r.x=sin(n(y*.5,.3)*4.14)*.25+.25;
  r.y=n(y*.73,.3);
  z=mod(y*2.,15.);
  r.z=-smoothstep(0.,.5,z)*smoothstep(.6,.5,z)*.5;
  i.z-=r.z;
  r.y=r.z;
  float u=smoothstep(.4,.6,z)*smoothstep(.8,.6,z)*.8;
  r.z+=u;
  r.w=u;
  a.x=smoothstep(.4,.8,z);
  if(mod(y*2.,30.)>=15.)
    a.x=1.-a.x;
  o=vec3(0);
  for(int v=0;v<2;v++)
    d[v]=vec4(-20),c[v]=vec4(0);
  u=e(w,S,x);
  for(int v=0;v<2;v++)
    for(int f=0;f<4;f++)
      {
        float x=0.,y=d[v][f];
        if(y<0.)
          continue;
        vec3 m=w+S*y,z=e(m,e(y)),i=n(m,z,x);
        o+=e(i,m,z,S,y,x)*c[v][f];
      }
  z=smoothstep(1.9,0.,a.y);
  S=mix(n(iChannel2,S,128.),n(iChannel3,S,64.)*.7,z);
  o+=S*S*SUN_COLOUR*(1.-u);
  o=e(o,f)*smoothstep(0.,2.,iTime);
  v=vec4(o,1);
}
#endif // robin_frag

#ifdef INCLUDE_fxaa_frag
// #version 150

out vec4 w;
const vec2 S=vec2(1920,1080);
uniform sampler2D o;
void main()
{
  vec2 v=1./S,x=gl_FragCoord.xy*v;
  vec4 f=vec4(x,x-v*.5);
  vec3 y=vec3(.299,.587,.114);
  float z=dot(textureLod(o,f.zw,0.).xyz,y),m=dot(textureLod(o,f.zw+vec2(1,0)*v.xy,0.).xyz,y),i=dot(textureLod(o,f.zw+vec2(0,1)*v.xy,0.).xyz,y),s=dot(textureLod(o,f.zw+vec2(1)*v.xy,0.).xyz,y),t=dot(textureLod(o,f.xy,0.).xyz,y);
  x=vec2(-z-m+i+s,z+i-m-s);
  float a=1./(min(abs(x.x),abs(x.y))+1./128.);
  x=min(vec2(8),max(vec2(-8),x*a))*v.xy;
  vec3 c=.5*(textureLod(o,f.xy+x*(1./3.-.5),0.).xyz+textureLod(o,f.xy+x*(2./3.-.5),0.).xyz),e=c*.5+.25*(textureLod(o,f.xy+x*-.5,0.).xyz+textureLod(o,f.xy+x*.5,0.).xyz);
  a=dot(e,y);
  w=a<min(t,min(min(z,m),min(i,s)))||a>max(t,max(max(z,m),max(i,s)))?
    vec4(c,1):
    vec4(e,1);
}
#endif // fxaa_frag

#ifdef INCLUDE_heart_frag
uniform float v;
uniform vec2 x;
uniform vec4 l;
void main()
{
  vec2 f=(2.*gl_FragCoord.xy-x)/x.y;
  float y=mod(v,2.)/2.,m=pow(y,.2)*.5+.5;
  m-=m*.2*sin(y*6.2831*5.)*exp(-y*6.);
  f*=vec2(.5,1.5)+m*vec2(.5,-.5);
  y=atan(f.x,f.y)/3.141593;
  m=length(f);
  y=abs(y);
  y=(13.*y-22.*y*y+10.*y*y*y)/(6.-5.*y);
  gl_FragColor=vec4(step(m,y)*pow(1.-m/y,.25),0,0,1);
}
#endif // heart_frag
