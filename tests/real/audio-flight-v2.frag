/** 
    Copied from https://www.shadertoy.com/view/7tfyRl
    License: Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License
    
    AudioFlight 🚀 v2 - music Boris Brejcha - Gravity
    4/14/22 | @byt3_m3chanic
    
    Path shader based around @Shane's stuff - he has a ton of amazing ones.
    https://www.shadertoy.com/view/MlXSWX
    
    Music EQ based around @blackle's domain rep tricks
    
    Lots of fo

*/


#define R           iResolution
#define T           iTime
#define M           iMouse

#define PI2         6.28318530718
#define PI          3.14159265358

#define MINDIST     .0001
#define MAXDIST     125.

#define r2(a) mat2(cos(a),sin(a),-sin(a),cos(a))

float hash21(vec2 p){  return fract(sin(dot(p, vec2(27.609, 57.583)))*43758.5453); }
float sampleFreq(float freq) {
    return texture(iChannel0, vec2(freq, 0.1)).x;
}

//http://mercury.sexy/hg_sdf/
float pMod(inout float p, float size) {
	float c = floor((p + size*0.5)/size);
	p = mod(p + size*0.5, size) - size*0.5;
	return c;
}
vec2 pMod(inout vec2 p, float size) {
	vec2 c = floor((p + size*0.5)/size);
	p = mod(p + size*0.5, size) - size*0.5;
	return c;
}
vec3 pMod(inout vec3 p, float size) {
	vec3 c = floor((p + size*0.5)/size);
	p = mod(p + size*0.5, size) - size*0.5;
	return c;
}
float pModPolar(inout vec2 p, float repetitions) {
    float angle = 2.*PI/repetitions;
    float a = atan(p.y, p.x) + angle/2.,
          r = length(p),
          c = floor(a/angle);
    a = mod(a,angle) - angle/2.;
    p = vec2(cos(a), sin(a))*r;
    if (abs(c) >= (repetitions/2.)) c = abs(c);
    return c;
}
float vmax(vec2 v) {	return max(v.x, v.y);						}
float vmax(vec3 v) {	return max(max(v.x, v.y), v.z);				}
float fBox(vec3 p, vec3 b) {
	vec3 d = abs(p) - b;
	return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}
float fBox2(vec2 p, vec2 b) {
	vec2 d = abs(p) - b;
	return length(max(d, vec2(0))) + vmax(min(d, vec2(0)));
}
//@iq
float sdCap( vec3 p, float h, float r ){
  p.y -= clamp( p.y, 0.0, h );
  return length( p ) - r;
}
// @Shane - https://www.shadertoy.com/view/MlXSWX
vec2 path(in float z){ 
    vec2 p1 =vec2(2.35*sin(z * .125)+2.38*cos(z * .25), 3.5*cos(z * .0945));
    vec2 p2 =vec2(3.2*sin(z * .19), 4.31*sin(z * .125)-2.38*cos(z * .115));
    return (p1 - p2)*.3;
}

// globals
float time,tm,travelSpeed;

// globals and stuff
float glow,iqd,flight,beams,gcolor,objglow,offWobble,boxsize;
float ga,sa,slp;
vec3 g_hp,s_hp;
mat2 r4,r5;

const vec3 cxz = vec3(3.15,4.75,3.);
const float scale = 3.0;

vec2 fragtail(vec3 pos) {
    float ss=1.15;
    float r = 1e5;
    
    for (int i = 0;i<2;i++) {
        pos=abs(pos);
        if ( pos.x- pos.y<0.) pos.yx = pos.xy;
        if ( pos.x- pos.z<0.) pos.zx = pos.xz;
        if ( pos.y- pos.z<0.) pos.zy = pos.yz;
        
        pos.x=scale * pos.x-cxz.x*(scale-1.);
        pos.y=scale * pos.y-cxz.y*(scale-1.);
        pos.z=scale * pos.z;
        
        if (pos.z>0.5*cxz.z*(scale-1.)) pos.z-=cxz.z*(scale-1.);

        r = fBox2(pos.xy,vec2(5,1.5+.25*sin(pos.x*5.)))-.0015;
        ss*=1./scale;
    }

    return vec2(r*ss,1.);
}

//@blackle domain rep https://www.shadertoy.com/view/Wl3fD2 
vec2 edge(vec2 p) {
    vec2 p2 = abs(p);
    if (p2.x > p2.y) return vec2((p.x < 0.) ? -1. : 1., 0.);
    else             return vec2(0., (p.y < 0.) ? -1. : 1.);
}
float ths= 13.25;
// scene map
vec2 map (in vec3 p, float sg) {
  
    vec2 res = vec2(100.,-1.);
    float msize = 7.25;
    
    // set path(s) vector(s)
    vec2 tun = p.xy - path(p.z);
    vec3 q = vec3(tun,p.z);
    vec3 o = vec3(tun+vec2(0.,.0),p.z+travelSpeed+4.25);
   
    vec3 s = q;

    o.zx*=r5;
    o.yz*=r4;
    o = abs(o)-(offWobble*.25);
    float obj = fBox(o,vec3(.15*offWobble))-.015;
    if(obj<res.x ) {
        res = vec2(obj,11.);
        g_hp=o;
    }
    
    // mods and vectors
    float pid = floor((q.z+(msize/2.))/msize);
    float trackmod = mod(pid,18.);
    float deg = trackmod<12. ? trackmod<6. ? 4. : 6. : 10.;
    pModPolar(q.xy,deg);
    pModPolar(s.xy,deg*.5);
    
    vec3 r =s;
    vec3 fs=s-vec3(2.85,0,0);
    r = vec3(abs(r.x),abs(r.y),r.z);

    // audio bards
    fs.z*=2.;
    vec2 center = floor(fs.xz) + .5;
    vec2 neighbour = center + edge(fs.xz - center);

    float chs = floor(center.y);
    float bmod = mod(chs,16.);

    float height = (sampleFreq(bmod*.0465));
    height=smoothstep(.001,1.,height);
    
    ga=height;
    float ids = pMod(s.z,msize);
    vec3 qid = pMod(q,msize);
    float ld = mod(ids,6.);
    float lq = mod(ids,2.);    

    iqd=qid.x;

    float zprs= mod(chs, tm <8.? tm <4.? tm <4.? 2.: 2.: 5.: floor(height*1.45));

    float d4a = length(r.xy-vec2(2.5,1.75))-.1;
    float d4 =  length(r.xy-vec2(2.5,1.75))-.04+.027+.027*sin(r.z-time*4.5);
    if(d4<res.x ) {
        res = vec2(d4,12.);
        g_hp=p;
    }
   
    // fractal
    vec2 d1 = fragtail(q);
    d1.x = max(d1.x,-d4a);
 
    s.z=abs(s.z);
    float blt = sdCap(s-vec3(2.45,-.58,2.725),1.16 ,.015);
    if(lq<2.) d1.x = min(blt,d1.x);
    if(d1.x<res.x) {
        res = d1.xy;
        g_hp = p;
    }
    
    float me =   fBox(fs-vec3(0,0,center.y),   vec3(.05,.150+height,.25));
    float next = fBox(fs-vec3(0,0,neighbour.y),vec3(.05,.001+height,.25));
    float dlt = min(me, next);
    if(dlt<res.x) {
        //float mid= zprs<4.? zprs<3.? zprs<2.? 3. : 4. : 4.  : 3.;
        res = vec2(dlt,4.);//tm <8. ? mid : 4.);
        g_hp = p;
    }

    if(sg==1.)beams += .0001/(.000003+d4*d4);
    if(sg==1.&&lq<1.)flight += .00025/(.0000001+blt*blt);
    if(sg==1.&&zprs<.1)glow += .00015/(.000002+dlt*dlt);
    if(sg==1.&&tm<ths)objglow += .0005/(.0005+obj*obj);
    
    
    return res;
}

vec2 marcher(vec3 ro, vec3 rd, int maxstep, float sg){
    float d =  0.,
          m = -1.;
        for(int i=0;i<maxstep;i++){
            vec3 p = ro + rd * d;
            vec2 t = map(p,sg);
            if(abs(t.x)<d*MINDIST||d>MAXDIST)break;
            d += i<42? t.x*.35 : t.x;
            m  = t.y;
        }
    return vec2(d,m);
}

vec3 normal(vec3 p, float t) {
    float e = MINDIST*t;
    vec2 h = vec2(1,-1)*.5773;
    return normalize( 
        h.xyy*map( p + h.xyy*e,0.).x + 
        h.yyx*map( p + h.yyx*e,0.).x + 
        h.yxy*map( p + h.yxy*e,0.).x + 
        h.xxx*map( p + h.xxx*e,0.).x );
}

//iq of hsv2rgb
vec3 hsv2rgb( in vec3 c ) {
    vec3 rgb = clamp( abs(mod(c.x*6.0+vec3(0.0,4.0,2.0),6.0)-3.0)-1.0, 0.0, 1.0 );
    return c.z * mix( vec3(1.0), rgb, c.y);
}

void mainImage( out vec4 O, in vec2 F ) {
    // precal
    time = iTime;
    tm = mod(time*.3, 18.);
    travelSpeed = (time * 5.);
    
    offWobble = 1.5+1.15*sin(tm+time*.1);
    
    r4 =r2(time);
    r5 =r2(time);
    
    // pixel screen coordinates
    vec2 uv = (F.xy - R.xy*0.5)/max(R.x,R.y);
    vec3 C = vec3(0.),
         FC = vec3(.03);

    float crop = clamp((-.05)+(T*.05),0.,.18);
    if(uv.y<crop&&uv.y>-crop){
    vec3 lp = vec3(0.,0.,0.-travelSpeed);
    vec3 ro = vec3(0.,0,.15);

    // mouse
    float x = M.xy==vec2(0) || M.z<0. ? 0.:(M.y/R.y*1.-.5)*PI;
    float y = M.xy==vec2(0) || M.z<0. ? 0.:-(M.x/R.x*2.-1.)*PI;

    ro.zy*=r2(x);
    
    ro +=lp; 

    lp.xy += path(lp.z);
    ro.xy += path(ro.z);

    // set camera
    vec3 f=normalize(lp-ro),
         r=normalize(cross(vec3(0,1,0),f)),
         u=normalize(cross(f,r)),
         c=ro+f*.183,
         i=c+uv.x*r+uv.y*u,
        rd=i-ro;

    // center tracking
        rd.xy = r2( (.2*sin(time*.3))-path(lp.z).x/ 24. )*rd.xy;
        rd.xz = r2( y-path(lp.z+1.).y/ 14. )*rd.xz;

    // march
    vec2 t = marcher(ro,rd, 164,1.);
    float d = t.x,
          m = t.y;
    s_hp=g_hp;
    
    // if visible 
    if(d<MAXDIST)
    {
        vec3 p = ro+rd*d;

        vec3 n = normal(p,d);
        vec3 lpos = vec3(0,0,.25)-p;
        lpos +=lp;
        lpos.xy += path(lpos.z);
        vec3 l = normalize(lpos);
        
        float diff = clamp(dot(n,l),.01,1.);

        float spec = pow(max(dot(reflect(l,n),rd),.01),24.);

        vec3 h = m==11.? vec3(.005): vec3(1.0);
        if(m==3.||m==4.) h = vec3(.012);
        if(tm>ths) {
            C =(h * diff + spec);
        } else {
            if(m==3.||m==4.) C =(hsv2rgb(vec3(s_hp.z*.01,.8,.6))  * diff);
        }
        
    } 
    
    if(tm>ths) {
        if(mod(T,.1)<.05)FC=vec3(.8);
    }else{

        C += abs(glow*.7)*hsv2rgb(vec3(s_hp.z*.01,.8,.6));
        C += abs(objglow*.65)*vec3(1,1,1);
    }
    C = mix( C, FC, 1.-exp(-0.000075*t.x*t.x*t.x));
    C += abs(beams*.65)*hsv2rgb(vec3(s_hp.z*.025,.8,.6));
    C += abs(flight*.75)*vec3(.5,1,.2);
    }
    
    
    float px = 1./R.x;

            
    float d1 = fBox2(uv+vec2(-.485,.2675),vec2(.005))-.002;
    d1=smoothstep(px,-px,d1);
    C=mix(C,vec3(0.212,0.671,0.576),d1);
    
    d1 = fBox2(uv+vec2(-.465,.2675),vec2(.005))-.002;
    d1=smoothstep(px,-px,d1);
    C=mix(C,vec3(0.757,0.686,0.341),d1);
    
    d1 = fBox2(uv+vec2(-.445,.2675),vec2(.005))-.002;
    d1=smoothstep(px,-px,d1);
    C=mix(C,vec3(0.882,0.459,0.867),d1);
    
    C=pow(C, vec3(0.4545));
    O = vec4(C,1.0);
}
