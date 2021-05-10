// Oscar's Chair by Eos
// From https://github.com/demoscene-source-archive/oscar-s-chair

#version 130

// Oscar's Chair

vec2 t=gl_FragCoord.xy/1280;

uniform sampler2D tex1;
float time=gl_TexCoord[0].x/44100;

float cd,d;
float lb=1,noiseoctaves;

vec3 bs=vec3(7,5,19);
vec3 chairpos=vec3(-4,0,3);
vec3 n,rp;
vec3 lpos=vec3(0,5,-7);
vec3 ldir;

float fbm2(vec2 p,float f)
{
   float x=0,s=1,a=1;
   for(int i=0;i<noiseoctaves;++i)
   {
      x+=(texture(tex1,(p+i*40)*s/vec2(1280,640)).b*2-1)*(a/=f);
      s*=2;
   }
   return x;
}


float heightcol(vec2 p,out vec3 c)
{
   vec2 op=p;
   float h;
   
   if(cd==0)
   {
      p+=fbm2(p,2)/50;
      p+=floor(p*vec2(.6,7)+12*cos(floor(p.y*4.5)))*31;
      c=vec3(1,.57,.2)*(3+fbm2(p,2))/16+fbm2(p*vec2(.8,16),2)/30;
      if(time>25&&time<=60&&op.y<1)
         c=mix(c,vec3(1,.01,.01)/9,smoothstep(.2,.204,fbm2(op,2.3))); // blood
      //h=(.9+.1*fbm2(p*vec2(.01,1)*12+fbm2(p,2)/2,1.4)+c.r*3)/6;
        h=.15+.017*fbm2(p*vec2(.12,12)+fbm2(p,2)/2,1.4)+c.r/2;
      c*=pow(h,4)*168;
      return h;
   }

   if(abs(p.x)<1.5&&abs(p.y-3)<3&&rp.z<-bs.z+.1)
   {
      c=vec3(fbm2(p*vec2(1,.2),2)+2)/4;
      p=abs(p+fbm2(p,2)/60-vec2(0,3));
      if(p.x>1.475||p.y>2.975)c*=.3;
      return 11-clamp(abs(max(abs(p.x-.7)+.3,abs(p.y-1.4)-.3)-.7),.05,.14)*11+fbm2(p*vec2(1,.2),2.5);
   }

   h=1-smoothstep(.1,.12,fbm2(p/2+10,2.5)-.1);
   
   // specks
   c=mix(vec3(.7),mix(vec3(1,1,.5)/1.4+fbm2(p*8,1.2)/15,vec3(.65,1,.5),h)/4*min(p.y*3-1.5+.8,1),step(.5,p.y))-step(.4,fbm2(p*3,1.6))/140;

   return fbm2(p,2)/2-c.g*4;
}

vec3 normal(vec2 p,out vec3 c)
{
   n = vec3(4e-3, 0,0);
   return normalize(vec3(heightcol(p + n.xy,c) - heightcol(p - n.xy,c), .16,
      heightcol(p + n.yx,c) - heightcol(p - n.yx,c)));
}



float dist(vec3 p)
{
   float b=time>81?-1.4:-2.827;
   p-=chairpos;
   p.y+=(fbm2(p.xz*=mat2(cos(b),sin(b),-sin(b),cos(b)),2.5)/2+.5)*clamp(time/3-29,0,5)+bs.y-1.25;
   p.x+=fbm2(p.yz,2.5)*clamp(time/3-29,0,5)-max(0,p.y)/10;
   p.z=abs(p.z);

   return min(min(min(max(length(max(abs(p.xz)-vec2(.45,.41),0))-.25,abs(p.y)-.02),
            length(p-vec3(clamp(p.x,-.6,.6),-.1,.6))-.025),
          length(vec3(abs(p.x)-.6+min(0,p.y)/5,p.y-clamp(p.y,-1.24,1.5*step(0,p.x)),p.z-.6))-.025),
               max(length(max(abs(p.yz-vec2(1,0))-vec2(.5,.35),0))-.25,abs(p.x-.6)-.03));
}

float sdftrace(vec3 ro,vec3 rd)
{
   float sdft=0,i=0;
   for(;i<200&&sdft<30;++i)
   {
      cd=min(cd,d=dist(ro+rd*sdft));
      sdft+=d/2;
   }
   return sdft;
}

vec3 sampleRoom(vec3 ro,vec3 rd)
{
   vec3 c;

   rp=ro+rd*(d=min((sign(rd.x)*bs.x-ro.x)/rd.x,min((sign(rd.y)*bs.y-ro.y)/rd.y,(sign(rd.z)*bs.z-ro.z)/rd.z)));
   
   if(abs(rp.y)>bs.y-1e-3)
   {
      cd=0;
      n=normal(rp.xz/2,c);        
        c*=min(vec3(.75,1,.75)+min(bs.x-abs(rp.x),bs.z-abs(rp.z))/4,1)*smoothstep(0,2.2,length(rp-chairpos-vec3(0,-bs.y+1.25,0)));
   }
   else
   {
      cd=1;
      n=normal(vec2(rp.x-rp.z-bs.z,rp.y+bs.y),c).xzy;

      if(abs((rp/bs).x)>abs((rp/bs).z)&&abs((rp/bs).x)>abs((rp/bs).y))
         n=n.zyx*vec3(-sign(rp.x),1,-1);
      c*=mix(vec3(1,1,.5),vec3(1),min((-abs(rp.y)+bs.y)/4,1))*sqrt(min((-abs(rp.y)+bs.y)*8+.4,1));   
   }

     c*=mix(4,1,smoothstep(1,2,length(rp/bs))) * max(0,1+3*dot(n,normalize(lpos-rp))) / 16 * (1-smoothstep(0,2.5,length(max(abs(rp)-bs+1,0))));

   c=(c+pow(clamp(dot(normalize(normalize(lpos-rp)-rd),n),0,1),64)*(c*.8+.2))*exp2(-length(lpos-rp)/1.2)*lb*20*mix(.02,2,smoothstep(.6,.7,dot(normalize(lpos-rp),ldir)));

   if(time<=97)
      c+=pow(vec3(2.2,.7,.3)*exp(-abs(fbm2(rp.xz/2+rp.y/4-time/2,1.5))*10*fbm2(-rp.xy+time-rp.z,1.5)+fbm2(rp.xy-time/3,2)*6-rp.y-bs.y-rp.z-bs.z+(time-88)/4+rp.x/9),vec3(2))/70; // fire


   return c;
}

void main()
{

if(gl_TexCoord[0].y==0)
{
  gl_FragColor.rgb=vec3(gl_TexCoord[0].xx,fract(sin(float((int(gl_FragCoord.x)*19349663^int(gl_FragCoord.y)*83492791)%38069))*43758.545));
}
else if(gl_TexCoord[0].y==1)
{
      gl_FragColor*=0;

      for(int y=-6;y<=6;++y)
         for(int x=-6;x<=6;++x)
            if(x*x+y*y<37)
               gl_FragColor=mix(gl_FragColor,max(gl_FragColor,texelFetch(tex1,ivec2(gl_FragCoord.xy+vec2(x,y)*texelFetch(tex1,ivec2(gl_FragCoord.xy),0).a),0)),.06);
}
else if(abs((t.y+=.22)-.5)<.2)
{

   vec3 c2=vec3(0);



   for(int i=0;i<8;++i)
   {
   float zoom=2.7;
   vec3 ro=vec3(10+time/4,-4,-1),target=vec3(-7,-2,0);
   
   d=0;
   
   if(time>97)
   {
      target=ro=vec3(0,-4,time-97);
      target.z-=1;
      lpos=vec3(0,3,-2-20);
      lb=.4;
      bs=vec3(3,7,28);
      chairpos=vec3(-4,0,13);
   }

   else if(time>81)
   {
      target=ro=vec3(0,-3.6,97-time);
      target.z-=1;
      lpos=vec3(0,5,-5);
      lb=.01;
      zoom=1.7;
      bs=vec3(7,5,9);
      chairpos=vec3(0,0,-2);
   }

   else if(time>60)
   {
      ro=vec3((time-60)/5-1,-2,13);   
      target=vec3(-7,-5,-7);
      lpos=vec3(0,5,-3);
      d=sin(time-60)/10-.9;
      bs=vec3(7,5,9);
      chairpos=vec3(-4,0,2);
   }
   else if(time>25)
   {
      ro=vec3(0,0,25-time/2.7);
      target=vec3(0,-4,18-time/2.7);
      lpos=vec3(-2,5,-7);
      bs=vec3(9,5,14);
      chairpos=vec3(3.5,0,-6);
   }
   else
   {
      d=-.9;
   }
      
      noiseoctaves=7;
   
      ldir=vec3(0,cos(d),sin(d));
   
      vec3 rd=normalize(target-ro),c=normalize(vec3(-rd.z,0,rd.x));
      
      if(time>81&&time<=97)
      {
         rd.y+=fbm2(vec2(time+6)/2,2)*.05;
         c.y+=fbm2(vec2(time)/3,2)*.08;
      }
      
      rd=mat3(c,cross(c,rd),rd)*normalize(vec3(t*2+vec2(i&1,i/2)/1200-1,zoom));
/*
      if(time>81&&time<=97)
      {
         rd.yz*=R(fbm2(vec2(time+6)/2,2)*.05);
         rd.xy*=R(fbm2(vec2(time)/3,2)*.08);
      }
         */
      float sdft=sdftrace(ro,rd);
   
      if(sdft<30)
      {
         rp=ro+rd*sdft;

         n=vec3(2e-2,0,0);
         n=normalize(vec3(dist(rp + n.xyy) - d, dist(rp + n.yxy) - d,
                                 dist(rp + n.yyx) - d));
         noiseoctaves=2;
         c=sampleRoom(rp,reflect(rd,n))*pow(1+dot(rd,n),3);
      }
      else
      {
         c=sampleRoom(ro,rd);
         zoom=(.3+.7*pow(1+dot(rd,n),2)/4)*(.1+.9*step(rp.y,-bs.y+1e-2));
         ro=reflect(rd,n);
         sdft=d;

         noiseoctaves=2;
         cd=1e2;
         rd=normalize(lpos-rp);
         sdftrace(rp+rd*1e-2,rd);
   
         c*=.4+.6*smoothstep(0,dist(rp)/5,cd);
         if(rp.y<-bs.y+1e-2)
            if(sdftrace(rp,ro)<30)
               zoom*=.2,c*=.7;
         c+=sampleRoom(rp,ro)*zoom*mix(vec3(1),c,.8);
      }
   
      if(time>60&&time<=97)
         gl_FragColor.a=time<=81?(sdft-15)/6:sdft/15;
      
   
   c*=smoothstep(0,2,abs(time-60))*smoothstep(0,3,abs(time-112))*(1-(pow(abs(t.x*2-1),4)+pow(abs(t.y*2-1)*2.5,4))*.4);
      c=pow(.003+c*40,vec3(1,1.1,1.2));
c2+=c/(c+.4)*1.1;
   }

   d=0;
   for(int i=0;i<64;++i)
   {
        d+=texture(tex1,clamp((t-vec2(.4,.532)+vec2(i&7,i/8)/10000)*4*vec2(2.25,4),0,1)).r+texture(tex1,clamp((t-vec2(.38,.33)+vec2(i&7,i/8)/10000)*vec2(2.25,4),0,1)).g;
   }
     
   //gl_FragColor.rgb=mix(sqrt(c2/8),vec3(1),d/64*clamp((6-abs(time-10))/5,0,1));
   gl_FragColor.rgb=sqrt(c2/8)+d/104*clamp((6-abs(time-10))/5,0,1);
}
}
