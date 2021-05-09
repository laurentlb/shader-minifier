// The Real Party Is In Your Pocket by Fizzer
// From https://github.com/demoscene-source-archive/the-real-party-is-in-your-pocket/blob/master/src/shaders/fragment.frag

// Game Boy Colours
// The Real Party Is In Your Pants
// The Real Party Is In Your Pocket
// change "GAME BOY" to "DEMO BOY", change "Nintendo" to "Revision" using Pretendo font (at high res)

// fix small pixel errors,
// make buttons flatter
// include a 1920x1080 executable! and make sure to adjust the bloom radius to compensate for this.

// a mediocre raymarcher using 15-year-old technology https://www.pouet.net/topic.php?post=561063
// (actually the accumulation buffer is probably even older)

#version 130

float smin( float a, float b, float k )
{
    k=4.1/k;
    float h = clamp( 0.5+0.5*(b-a)/k, 0.0, 1.0 );
    return mix( b, a, h ) - k*h*(1.0-h);
}


float smax( float a, float b, float k )
{
    return -smin(-a,-b,k);
}

mat2 rot(float a)
{
    return mat2(cos(a),sin(a),-sin(a),cos(a));
}

float sdbox3(vec3 p,vec3 b)
{
    vec3 d = abs(p)-b;
    return length(max(d,vec3(0))) + min(max(d.z,max(d.x,d.y)),0.0);
}

float sdbox2(vec2 p,vec2 b)
{
    return sdbox3(vec3(p,0),vec3(b,1));
}

float sdsegment(vec2 p,vec2 a,vec2 b)
{
    return length(p-mix(a,b,clamp(dot(p-a,normalize(b-a))/length(b-a),0.,1.)));
}


float sdBezier( in vec2 pos, in vec2 A, in vec2 B, in vec2 C )
{
    float d=1e4,t;
    for(int i=0;i<16;++i)
    {
        t=float(i)/15.;
        d=min(d,length(pos-mix(mix(A,B,t),mix(B,C,t),t)));
    }
    return d;
}



float gameboylogo(vec2 p)
{
   float d=1e4;

p.x-=p.y*.11;
    
   // D
   vec2 p3=p-vec2(-.26,0);
      p3.x=min(-p3.x,0.);
   d=min(d,max(-p.x-.26,abs(length((p3)*vec2(.9,1))-.022)-.006));
   d=min(d,sdbox2(p-vec2(-.265,-0.00),vec2(.006,.0278)));

   // M
   vec2 p2=p;
   p2.x-=-.16;
   p2.x=abs(p2.x);
   
   d=min(d,sdbox2(p2-vec2(p2.y/4.+.004,0),vec2(.006,.028)));
   d=min(d,sdbox2(p2-vec2(-p2.y/3.5+.02,0),vec2(.007,.028)));

   // E
   p.x+=.1;
   d=min(d,sdbox2(p-vec2(-.12,0),vec2(.006,.028)));
   d=min(d,sdbox2(p-vec2(-.11,0),vec2(.014,.007)));
   d=min(d,sdbox2(p-vec2(-.11,.021),vec2(.016,.007)));
   d=min(d,sdbox2(p-vec2(-.11,-.021),vec2(.014,.007)));
   p.x-=.1;

   // O
   d=min(d,abs(length((p-vec2(-.106,0))*vec2(1,.8))-.017)-.0055);

   // B
   //d=min(d,sdbox2(p-vec2(-.072,0),vec2(.006,.02)));
   //d=min(d,max(-p.x-.078,abs(length((vec2(max(p.x +.055,0.),abs(p.y)-0.012)))-.01)-.005));

   // B
   d=min(d,sdbox2(p-vec2(-.07,0),vec2(.006,.02)));
   d=min(d,max(-p.x-.076,abs(length((vec2(max(p.x+.055,0.),abs(p.y)-0.011)))-.011)-.0059));

   // O
   d=min(d,abs(length((p-vec2(-.014,0))*vec2(1,.8))-.017)-.0055);
   
   // Y
   p.x-=.023;
   p.x=abs(p.x);
   p.x-=max(p.y/2.+.002,0.);
   d=min(d,sdbox2(p-vec2(0,0),vec2(.0065,.028)));

   return d;
}


float revisiontext(vec2 p)
{
   p*=1.05;
          
   return max(max(min(min(min(smin(max(min(min(max(min(min(min(min(max(min(min(max(max(min(length((p-vec2(-.11,0.01))*vec2(1,1.3))-.016,
            length(vec2(p.x+.11,max(0.,abs(p.y+.02)-.009))*vec2(.8,1.))-.013),
            -(length(vec2(p.x+.112,max(0.,abs(p.y-0.009)-.004)))-.005)),-(length(vec2(p.x+.112,max(0.,abs(p.y+0.015)-.007)))-.005)),
            sdbox2(p-vec2(-.122,0),vec2(.007,.022))),
            max(-(length(vec2(p.x+.074,max(0.,abs(p.y+.006)-.006)))-.005),
          length((p-vec2(-.074,-.006))*vec2(.9,1))-.015)),
          -sdbox2(p-vec2(-.06,-.008),vec2(.01,.002))),
          sdbox2(p-vec2(-.075,-.0039),vec2(.01,.002))),
          sdbox2(vec2(abs(p.x+.037)-.01-p.y/2.2,p.y+.006),vec2(.006,.015))),
          sdbox2(p-vec2(-.006,0),vec2(.007,.022))),
          sdbox2(p-vec2(.045,0),vec2(.007,.022))),
          -sdbox2(p-vec2(0,0.01),vec2(.06,.002))),
          max(-sdbox2(p-vec2(.025,-0.005),vec2(.01,.007)),
          length(vec2(p.x-.019,p.y+.005-.007)*vec2(.65,1))-.009)),
          max(-sdbox2(p-vec2(.01,-0.005),vec2(.01,.007)),
          length(vec2(p.x-.019,p.y+.005+.007)*vec2(.65,1))-.009)),
          -(length(vec2(p.x-.019,max(0.,abs(p.y+.0055)-.007)))-.005)),
          sdbox2(rot(.0)*(p-vec2(.019,-0.005-(p.x-.019)/2.)),vec2(.0047,.004)),512.*3.),
          max(-(length(vec2(p.x-.0735,max(0.,abs(p.y+.006)-.006)))-.005),
          length((p-vec2(.0735,-.006))*vec2(.9,1))-.015)),
          sdbox2(p-vec2(.1,-.006),vec2(.007,.015))),
          length(vec2(p.x-.118,max(0.,abs(p.y+.012)-.009)))-.013),
          -(length(vec2(p.x-.112,max(0.,abs(p.y+.012)-.01)))-.005)),
          -p.y-.021)+.0003;
}


float wedge(vec4 w,vec2 uv)
{
   return length(max(vec4(0.0),vec4((rot(w.x)*uv).x, (rot(w.y)*-uv).x, length(uv)-w.z, -length(uv)+w.w)));
}

/*
float roundedcylinder(vec3 p,float l,float r0,float r1)
{
    return sdbox2(vec2(length(p.xy),p.z),vec2(r0,l))-r1;
}
*/

float speakerhole_d(vec3 p,float l,float r0,float r1,bool mode)
{
   vec3 speakerholesp=p;
   float an=.69;
   speakerholesp.xy-=vec2(.29,-.745);
   speakerholesp.xy=rot(an)*speakerholesp.xy;
   vec2 sz=vec2(.04,.05);
   vec2 speakerholesp2=speakerholesp.xy;
   speakerholesp.xy=mod(speakerholesp.xy,sz)-sz/2.;
//   float speakerholesd=roundedcylinder(speakerholesp,l,r0,r1);
   float speakerholesd=sdbox2(vec2(length(speakerholesp.xy),speakerholesp.z),vec2(r0,l))-r1;
   speakerholesd=max(speakerholesd,abs(speakerholesp2.y-.05)-(mode?.3:.15));
   if(mode)
      speakerholesd=max(speakerholesd,abs(speakerholesp2.x-.04)-.13);
   speakerholesp2.xy=rot(-an)*speakerholesp2.xy;
   speakerholesd=max(speakerholesd,abs(speakerholesp2.x-.12+sz.x)-.125);

   speakerholesd=max(speakerholesd,abs(speakerholesp2.y-speakerholesp2.x*.2+.015+(mode?-.03:0.))-.115*1.1);

   return speakerholesd;
}


float smoothstep2(float a, float x)
{
    x=clamp(x/a,0.,1.);
    return (x * -2. + 3.) * x * x;
}

vec4 sd(vec3 p) // vec4(distance, material id, texture u, texture v)
{    
    //float ww=1.25;
    
    p.x+=1.25/2.;
    float gbc=floor(p.x/1.25);
    
    if(gbc==0.)
        p.z-=.28;
    
    p.x=mod(p.x,1.25)-1.25/2.;
    
    p.xz = p.xz * rot(-.5*gbc);
    
    // body
    vec2 p2=p.xy;
    p2.x=abs(p2.x);
    float d=smax(smax(p2.x-.55,length(p2-vec2(0,.8))-1.76,49.),p2.y-.95,64.*1.3);
    
    
    float d2=p.z-.14;
    float d3=-p.z-.09-(1.-(smoothstep2(.9,(p.y+.3+.1))+smoothstep2(.55,-(p.y+.3+.17))))*.17; // back bump

    // outer screen
    float dos=smax(smax(p2.x-.48,length(p2-vec2(0,2.5))-2.5,39.*3.),p2.y-.87,80.);
    

    if(p.z>0.01)
        d2+=mix(.02,pow(max(0.,sdbox2(p.xy-vec2(0.,.25+pow(abs(p.x),3.)),vec2(.32,.94))-.05)*100.,1.3)/1500., 1.-smoothstep2(-.001,dos));

    // back cartridge slot
    if(p.z<0.)
        d3=smin(d3, sdbox3(p-vec3(0,0,-.1), vec3(.5,.5,.055))-.04,64.*1.);

    p2.x-=.09;
    p2.y+=.635;

    d=smax(d,d2,64.*3.);
    float id=d;
    float startselect_d = .001+ smin( length(p2)-.01, smin( length(p2-vec2(.036,0))-.01, length(p2-vec2(-.036,0))-.01, 40.), 40.);
    
   //float startselect_d = d_startselect(p.xy);

    d=smax(d,-p.z-.09,64.*1.);

    if(p.z<0.)
        d=smin(d,smax(id+.03,d3,64.*1.),64.*4.);
    
    id = 1.;


    // button holes
    d=smax(smax(d,min(p.z,-startselect_d+.003),32.*8.),min(p.z,-abs(dos)-.001),32.*14.);
    //d=smax(d,min(p.z-.13,-d_nintendooutline(p.xy)+.003),32.*16.);
    //d=smax(d,min(p.z-.13,-nintendologo(p.xy-vec2(0,-.1135))),32.*16.);
    d=smax(d,min(p.z-.13,-revisiontext(p.xy-vec2(0,-.118))),32.*16.);

    d=smax(d,min(p.z-.13,-abs(length(vec2(max(abs(p.x)-.12,0.),p.y+.117))-.035)+.003),32.*16.); // nintendo logo outline

    // inner screen    
    d=smax(smax(d,min(1.,-min(1.,max(smax(abs(p.x)-.31,abs(p.y-.49)-.29,500.),abs(p.z-.185)-.1))+.004),32.*18.),-abs(p.z-.01)-.0001,32.*12.);


	if(p.y<0.)
    {
        if(p.y < -.5)
        {
            // speaker holes
           d+=(1.-smoothstep(-.001,.002,speakerhole_d(p+vec3(-0,.03,-0.2),.13,.007,.001,true)))*.005; // indents
           d=smax(d,-speakerhole_d(p+vec3(0,0,-0.2),.13,.007,.001,false),64.*16.); // holes
        }

         vec2 dpadpos=vec2(-.32,-.314);
        vec2 dpadsz=vec2(.119+.005,.028+.005);

        float buttonA_d = length(p.xy-vec2(.405,-.28))-.07;
        float buttonB_d = length(p.xy-vec2(.202,-.345))-.07; 
        float dpad_d=smin( sdbox2(p.xy-dpadpos,dpadsz)-.005, sdbox2(p.xy-dpadpos,dpadsz.yx)-.005, 256.*2.);

        // button holes
        d=smax(d,min(p.z,-buttonA_d+.004),32.*10.);
        d=smax(d,min(p.z,-buttonB_d+.004),32.*10.);
	    d=smax(d,min(p.z,-min(1.,dpad_d)+.006),32.*6.);

        // buttons

        p.xy-=vec2(.405,-.28);
        float buttonA_h= (1.-smoothstep2(.002,min(min(sdsegment(p.xy,vec2(-.00,-.035),vec2(-.014,+.024)),
            sdsegment(p.xy,vec2(-.03,-.035),vec2(-.014,+.024))),
            sdsegment(p.xy,vec2(-.02,-.02),vec2(-.01,-.02)))-.0025))*.002;
        p.xy+=vec2(.405,-.28);


        p.xy-=vec2(.202,-.345);
        float buttonB_h=min(min(min(sdsegment(p.xy,vec2(-.03,-.035),vec2(-.03,+.024)),sdsegment(p.xy,vec2(-.03,-.035),vec2(-.03+.012,-.035))),
                     sdsegment(p.xy,vec2(-.03,.024),vec2(-.03+.012,.024))),
                     sdsegment(p.xy,vec2(-.03,(-.035+.024)/2.),vec2(-.03+.012,(-.035+.024)/2.)));
                     
        if(p.x>-.03+.012)
        {
            buttonB_h=min(min(buttonB_h,abs(length(p.xy-vec2(-.03+.012,(-.035+(-.035+.024)/2.)/2.))-(+.024 +.035)/4.)),
                        abs(length(p.xy-vec2(-.03+.012,(.024+(-.035+.024)/2.)/2.))-(+.024 +.035)/4.));
        }
        buttonB_h=(1.-smoothstep2(.002,buttonB_h-.0025))*.002;
        p.xy+=vec2(.202,-.345);

        float ssbuttonsd=max(-p.z,smax(abs(p.z)-.17,max(-.015,startselect_d),32.*3.));
        float buttonsd=min(max(-p.z,smax(abs(p.z)-.17+buttonB_h,buttonB_d,32.*4.)),
                           max(-p.z,smax(abs(p.z)-.17+buttonA_h,buttonA_d,32.*4.)));

        vec3 dpadp=p;
        dpadp.z-=pow(length(p.xy-dpadpos),2.)/2.-.01;
        
        buttonsd=smax(min(buttonsd,max(-p.z,smax(abs(dpadp.z)-.18,dpad_d,64.*12.))),-(length(dpadp-vec3(dpadpos,.19))-.026),64.*8.);

        // d-pad
        dpadp.xy-=dpadpos;
        dpadp.xy=abs(dpadp.xy);
        if(dpadp.x<dpadp.y)dpadp.xy=dpadp.yx;
        dpadp.x-=.075;
        dpadp.z-=.178;
        
        // dpad arrow indentations (equilateral triangles)
        p2=dpadp.yx;
        float a = sqrt(3.0);
        p2.x = abs(p2.x) - 1.0;
        p2.y = p2.y + 1.0/a;
        if( p2.x+a*p2.y>0.0 ) p2 = vec2(p2.x-a*p2.y,-a*p2.x-p2.y)/2.0;
        p2.x -= clamp( p2.x, -2.0, 0.0 );

        
        buttonsd=smax(buttonsd,-max(-dpadp.z,(-length(p2)*sign(p2.y)+.565)),64.*8.); // includes dpad arrows

        if(buttonsd<d)
        {
            d=buttonsd;
            id=2.;
        }

        if(ssbuttonsd<d)
        {
            d=ssbuttonsd;
            id=3.;
            if(abs(length(p.xy-vec2(-.09,-.635))-.015)<.005 || 
               sdsegment(p.xy-vec2(.09,-.635),vec2(0,.015),vec2(0,-.015)) <.005)
                id=3.1; // white "1 / 0" marks on buttons
        }
    }
    
    // screen indent and border
    if(sdbox3(p-vec3(0,.4,0),vec3(.49,.47,.1218))<.0)
        id=4.;

    return vec4(d, id+-gbc*8.+8., p.xy);
}

// Ray-box intersection.
vec2 box(vec3 ro,vec3 rd,vec3 p0,vec3 p1)
{
    vec3 t0 = (mix(p1, p0, step(0., rd * sign(p1 - p0))) - ro) / rd;
    vec3 t1 = (mix(p0, p1, step(0., rd * sign(p1 - p0))) - ro) / rd;
    return vec2(max(t0.x, max(t0.y, t0.z)),min(t1.x, min(t1.y, t1.z)));
}

vec2 intersectSphere(vec3 ro, vec3 rd)
{
    float a = dot(rd, rd), b = 2. * dot(rd, ro),
        c = dot(ro, ro) - 1., desc = b * b - 4. * a * c;

    if (desc < 0.)
        return vec2(-1);

    c = sqrt(desc);

    return (-b + vec2(-c, c)) / (2. * a);
}

vec4 traceRay(vec3 ro,vec3 rd,out vec3 outn)
{
    float t=10.;    
    vec2 res;
    
    vec3 n0;

    vec4 res0=vec4(-1);
    
    {
        float maxt=t;
        vec3 ro2=ro-vec3(0,0,.2),rd2=rd;

        vec3 scenesz=vec3(.6*3.,.98,.4);
        vec2 bi=box(ro2-vec3(0,0,.1),rd2,-scenesz,+scenesz);

        float t=-1.,d;

        vec4 res;

        if(bi.x<bi.y && bi.y > 0. && bi.x < maxt)
        {
            t=max(0.,bi.x);
            for(int i=0;i<60;++i)
            {
                res=sd(ro2+rd2*t);
                d=res.x;
                if(t>bi.y||abs(d)<1e-5)break;
                t+=d;
            }
            if(t<bi.y||t<0.)
            {
                vec3 e=vec3(2e-3,0,0);
                vec3 p = ro2+rd2*t;
                n0 = normalize(vec3(sd(p+e.xyy).x,sd(p+e.yxy).x,sd(p+e.yyx).x)-d);
                res0 = vec4(t,res.yzw);
            }
        }
    }


    if(res0.x>0.)
    {
        outn = n0;
        return res0;
    }


    t=-1.;
    
    vec2 bgpos=vec2(-1,-3);
    
    float pty=(bgpos.x-ro.y)/rd.y;
    float ptz=(bgpos.y-ro.z)/rd.z;
    
	vec2 ci = intersectSphere((ro-vec3(0,bgpos+2.))*vec3(0,.5,.5), rd*vec3(0,.5,.5));
    
    vec2 p=ro.yz+rd.yz*ci.y-(bgpos+2.);
    
    if(ci.x<ci.y&&ci.y>0.&&p.x<0.&&p.y<0.)
    {
        outn=vec3(-p*.5,0).zxy;
        t=ci.y;
    }    
    else if(pty>0.&&(pty<ptz||ptz<0.))
    {
        outn=vec3(0,1,0);
        t=pty;
    }
    else if(ptz>0.&&(ptz<pty||pty<0.))
    {
        outn=vec3(0,0,1);
        t=ptz;
    }

    vec3 rp=ro+rd*t;
    
    if(abs(rp.x)>5.||abs(rp.y)>2.5||abs(rp.z)>3.3)
        return vec4(-1);
    
    return vec4(t,0,0,0);
}


float seed;
float rand() { return fract(sin(seed++)*43758.5453123); }


// Sample the illumunation at a given position, in a given direction.
vec3 sampleRay(vec3 ro, vec3 rd)
{
    vec3 energy = vec3(0);
    vec3 spectrum = vec3(1.);

    bool hitdiffuse = false;
    
    for(int i = 0; i < 4; ++i)
    {        
        rd=normalize(rd);
        vec3 n, p0, p1;
        vec4 res=traceRay(ro, rd, n);
        float t = res.x;
        if(t<0.)
        {
            // No intersection.
            energy +=max( spectrum * vec3(.9)*(.5+.5*dot(rd,normalize(vec3(-2,4,1))))*.4*1.3*vec3(1,.9,.65)
            			+ spectrum * vec3(.9)*(step(.9,dot(rd,normalize(vec3(6,2,1)))))*10.4*2.3
            			+ spectrum * vec3(.9)*(step(.95,dot(rd,normalize(vec3(-2,1,3)))))*10.4*1.5
            			, 0.) *
                pow(max(.5+.5*-rd.z,0.),.2);
            break;
        }

        float gbc=floor(res.y/8.);
        res.y=mod(res.y,8.);
        
        vec3 rp=ro+rd*t;
        
        // lambert BRDF reflection sampling
         vec2 uv=vec2(6.28319 * rand(), rand() * 2. - 1.);
         vec3 lrd = n + vec3(sqrt(1.0 - uv.y * uv.y) * vec2(cos(uv.x), sin(uv.x)), uv.y);

        float fr = ((res.y>1.5?mix(rp.x>0.?.1:.05,.5, pow(1. - clamp(dot(-rd, n), 0., 1.), 2.)):
                    		mix(.2,.8, pow(1. - clamp(dot(-rd, n), 0., 1.), 1.5)))); // fresnel
        
        if(res.y>5.5&&res.y<6.5)
        {
            if(hitdiffuse)break; // kill caustics

            // dot matrix with stereo sound
            ro = rp + n * 2e-4;
            spectrum *= mix(.1,.8, pow(1. - clamp(dot(-rd, n), 0., 1.), 3.))*mix(vec3(1),vec3(1,.9,.5),.5);
            rd=reflect(rd,n)+(vec3(rand(),rand(),rand())-.5)*.3;
        }
        else if(res.y>4.5&&res.y<5.5)
        {
            if(hitdiffuse)break; // kill caustics

            fr = .02;
            // reflective/transparent screen
            if(rand()>fr)
            {
                ro = rp - n * 2e-4*-sign(rd.z);
                rd=rd+(vec3(rand(),rand(),rand())-.5)*.1;
            }
            else
            {
                ro = rp + n * 2e-4;
                rd=reflect(rd,n)+(vec3(rand(),rand(),rand())-.5)*.3;
            }
        }
        else if(res.y<0.5||res.y>2.5)
        {
            // backwall and floor, and other..
            
            vec3 col = res.y>2.5?vec3(.08):vec3(.7);
                
            if(res.y>3.5)
            {
           		col=vec3(.008);
              	if(!hitdiffuse) // only do this for primary and specular rays
                {
                    if(rp.y > .5)
                    {
                        //float r=.0018;
                        vec2 uv = res.zw-vec2(-.38,.6);
                        
                        // "POWER" text and symbol

                        float d=min(min(min(min(min(min(max(length(uv-vec2(-.0+.005,.005))-.0125,-(length(uv-vec2(-.017+.005,.005))-.025)),
                              max(length(uv-vec2(-.03+.01,.005))-.0125,-(length(uv-vec2(-.055+.01,.005))-.03))),
                              max(length(uv-vec2(-.0+.005,.005))-.0125,-(length(uv-vec2(-.017+.005,.005))-.025))),
                              max(length(uv-vec2(.03,.005))-.0125,-(length(uv-vec2(.02,.005))-.02))),
                              /*abs(length(uv-vec2(-.038, -.039))-.0105)),*/
                              abs(length(uv-vec2(-.037, -.0398))-.0104)),
                              sdsegment(uv,vec2(-.064, -.05),vec2(-.064, -.03))),
                              max(-uv.x -.063,abs(length(vec2(max(0.,uv.x +.06),uv.y + .035))-.006)));
                              
                        uv.x-=.091;
                        d=min(d,sdsegment(uv,vec2(-.064, -.05),vec2(-.064, -.03)));
                        d=min(d,max(-uv.x -.063,abs(length(vec2(max(0.,uv.x +.06),uv.y + .035))-.006)));
                        d=min(d,sdsegment(uv,vec2(-.054, -.05),vec2(-.059, -.041)));
                        uv.x+=.091;

                        d=min(d,sdsegment(uv,vec2(-.066+.075, -.05),vec2(-.066+.075, -.03)));
                        d=min(d,sdsegment(uv,vec2(-.066+.075, -.05),vec2(-.066+.084, -.05)));
                        d=min(d,sdsegment(uv,vec2(-.066+.075, -.03),vec2(-.066+.084, -.03)));
                        d=min(d,sdsegment(uv,vec2(-.066+.075, -.039),vec2(-.066+.084, -.039)));

                        uv.x=abs(uv.x+.01);
                        uv.x=abs(uv.x- .006);
                        d=min(d,sdsegment(uv,vec2(.0, -.05),vec2(.006, -.03)));

                        //return d-r;
                        col=mix(col,vec3(.3),step(d-.0018,.0));
                    }
           				//col=mix(col,vec3(.3),step(powerlogo(res.zw-vec2(-.38,.6)),.0));

                    // power light
                    if(length(res.zw-vec2(-.45+.02,.605))<.014)
                        energy += spectrum * 3. * vec3(1,.01,.01)*(.8+8.*(1.-smoothstep2(.01,length(res.zw-vec2(-.45+.02,.605)))));
                    
                    if(res.w>0. && rp.y < 0.2)
                    {
                        // "GAMEBOY" text
	           			col=mix(col,vec3(.3),step(gameboylogo(res.zw-vec2(0,.103)),.0));

                        // "COLOR" text

                        {
                            // C
                           vec2 pa=vec2(-.07,.02);
                           vec2 pb=vec2(-.09,.03);
                           vec2 pc=vec2(-.1,.0);
                           vec2 pd=pc+normalize(pc-pb)*.03;
                           vec2 pe=vec2(-.065,-.026);

                            col=mix(col,vec3(0.520661, 0.00153787 , 0.064975),step(min(sdBezier(res.zw-vec2(0.16,.103), pc, pd, pe ),
                                                       sdBezier(res.zw-vec2(0.16,.103), pa, pb, pc ))-.007,0.));
                        }

                        {
                            // O
                            vec2 p = res.zw-vec2(0.16,.103);
                            p-=vec2(-.029,-.015);
                            p.x+=cos(p.y*30.)*.01;
                            p.y-=cos(p.x*20.)*.01;
                            col=mix(col,vec3(0.093564, 0.0865052 , 0.434048),step(abs(length(p)-.02)-.007,0.));
                        }

                        {
                            // L
                            vec2 p = res.zw-vec2(0.16,.103);
                            vec2 pa=vec2(-.01,.02);
                            vec2 pb=vec2(-.007,.0);
                            vec2 pc=vec2(-.007,-.0205);
                            vec2 pd=pc+normalize(pc-pb)*.01;
                            vec2 pe=vec2(0.02,-.02);

                            col=mix(col,vec3(0.186082, 0.481799 , 0.0177778),
                                    step(min(sdBezier(p, pc, pd, pe ),sdBezier(p, pa, pb, pc ))-.007,0.));
                        }
                                                
                        {
                            // O
                            vec2 p = res.zw-vec2(0.16,.103);
                            p-=vec2(+.045,-.015);
                            p.x+=cos(p.y*20.+1.)*.005;
                            p.y-=cos(p.x*20.+25.)*.01;

                            col=mix(col,vec3(0.730857, 0.454964 , 0.000553633),step(abs(length(p)-.02)-.007,0.));
                        }

                        {
                            // R
                            vec2 p = res.zw-vec2(0.16+.08,.103);

                            vec2 pa=vec2(-.006,-.026);
                            vec2 pb=vec2(-.002,.0);
                            vec2 pc=vec2(-.007,.016);
                            vec2 pd=pc+normalize(pc-pb)*.012;
                            vec2 pe=vec2(0.02,.02);
                            vec2 pf=pd+normalize(pe-pd)*.05;
                            vec2 pg=vec2(.002,-.007);
                            vec2 ph=vec2(.025,-.028);
                            
                            col=mix(col,vec3(0, 0.332318 , 0.292872),step(min(min(min(sdBezier(p, pc, pd, pe ),
                                                                                      sdBezier(p, pa, pb, pc )),
                                  sdBezier(p, pe, pf, pg )),sdBezier(p, pg, (pg+ph)/2.+vec2(.001), ph ))-.007,0.));
                        }

                        
                    }

                    // dot matrix pixels
                    {
                       vec2 reso=vec2(160,144)/1.5;
                       vec2 uv=(res.zw-vec2(-0.001,.486))*3.75*vec2(reso.y/reso.x,1)*.5+.5;
                       vec2 uvf=fract(uv*reso);
                       uv=floor(uv*reso)/reso;
                       if(max(abs(uv.x-.5),abs(uv.y-.5))<.5)
                       {
                           vec3 scol=vec3(0);
                           
                           vec2 uv2=(uv*2.-1.)*vec2(reso.x/reso.y,1);
	// revision logo
   float d = min(min(min(min(min(min(min(min(min(min(min(min(min(min(min(min(min(wedge(vec4(1.54,0.53,0.91,0.72),uv2),
             	wedge(vec4(0.63,0.78,0.91,0.72),uv2)),
         		wedge(vec4(0.61,1.675,0.72,0.64),uv2)),
         		wedge(vec4(2.7,3.11,0.72,0.64),uv2)),
         		wedge(vec4(3.45,3.65,0.72,0.64),uv2)),
         		wedge(vec4(4.71,5.02,0.72,0.64),uv2)),
       			wedge(vec4(5.3,5.51,0.72,0.64),uv2)),
         		wedge(vec4(5.96,6.43,0.72,0.64),uv2)),
         		wedge(vec4(3.2,1.27,0.45,0.35),uv2)),
         		wedge(vec4(1.3,2.3,0.45,0.35),uv2)),
         		wedge(vec4(2.58,4.2,0.45,0.35),uv2)),
         		wedge(vec4(3.2,3.95,0.35,0.25),uv2)),
         		wedge(vec4(5.2,5.93,0.35,0.25),uv2)),
         		wedge(vec4(7.9,8.15,0.35,0.25),uv2)),
         		wedge(vec4(0.2,1.16,0.32,0.17),uv2)),
         		max(length(uv2)-0.84, -length(uv2)+0.72)),
         		max(length(uv2)-0.52, -length(uv2)+0.45)),
       			max(length(uv2)-0.17, -length(uv2)+0.08));
                           
                          float yy=floor(uv.y*8.);
                           scol=.5+.5*cos(vec3(.8,.3,2.)*(yy+1.));
                          
                           scol=mix(scol,vec3(1),step(d,0.));
                           scol*=vec3(1,1,.8);
                           scol*=smoothstep(0.1,.2,uvf.x)*smoothstep(0.1,.2,uvf.y);

                           // backlit screen because it's 2020
                           energy += spectrum * 3. * scol;
                           col=vec3(.1);
                       }
                    }
                }
            }

            if(res.y>3.&&res.y<3.2)
                col=vec3(.4); // white "0 / 1" marks on buttons
             
            spectrum *= col;
            ro = rp + n * 2e-4;
            rd = lrd;
            hitdiffuse=true;
        }
        else
        {
            vec3 col=gbc>1.5?vec3(1,.02,.2):gbc>.5?vec3(.2,1,.02):vec3(.02,.2,1);
            if(rand() > mix(.2,1.,fr))
            {
                if(res.y>1.5)
                {
                    // dpad / buttons
                    col=vec3(.02); 
                }

                // Diffuse (body)
                spectrum *= col;
                ro = rp + n * 2e-4;
                rd = lrd;
	            hitdiffuse=true;
            }
            else
            {
                if(hitdiffuse)break; // kill caustics
                
                spectrum *= (res.y>1.5?vec3(.5):.9*mix(col,vec3(1),.25));
                ro = rp + n * 2e-4;
                rd=reflect(rd,n)+(vec3(rand(),rand(),rand())-.5)*.4;
            }
        }
        
        if(max(spectrum.x, max(spectrum.y, spectrum.z)) < 1e-3)
            break;
    }
    
    // includes vignetting
//	return energy * (0.5 + 0.5*pow( 16.0*uv2.x*uv2.y*(1.0-uv2.x)*(1.0-uv2.y), 0.1 ));

    return energy;
}


void main()
{
    vec3 fragColor = vec3(0);

   for(int i=0;i<16;++i)
   {
      //rstate = uvec2(i,uint(gl_TexCoord[0].x));
      seed = float(i)+gl_TexCoord[0].x*16.;
   
      // cone sampling for AA
       float a = rand() *  6.28319;
       vec2 aaOffset = vec2(cos(a), sin(a)) * sqrt(1. - sqrt(1. - rand())) * 1.2;

//       if((int(seed)%6)==0)
//           aaOffset *= 8. / .75; // 1280x720
//           aaOffset *= 16. / .75; // 1920x1080
    
    if((int(seed)%4)==0)
//       aaOffset *= 20. * length(aaOffset); // 1280x720
       aaOffset *= 15. * length(aaOffset); // 1920x1080

    fragColor+=sampleRay(vec3(0.,-0.,4.8),
//            vec3((gl_FragCoord.xy - vec2(640,360) + aaOffset)/360.,-3.5)); // 1280x720
            vec3((gl_FragCoord.xy - vec2(960,540) + aaOffset)/540.,-3.5))/48.;
   }
   

   //fragColor/=float(16*3);

   // tonemapping
    fragColor /= (fragColor + 1.) / 2.;
    
    // Gamma correction
    
   gl_FragColor.rgb=pow(fragColor + .01*vec3(1,1,.5), vec3(1. / 2.2))+rand()/100.;
}