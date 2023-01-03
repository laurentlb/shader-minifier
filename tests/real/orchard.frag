// Copied from https://www.shadertoy.com/view/wlVSz3
// Orchard (Alternative Projection)
// by Dave Hoskins. March 2020
// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.

// This breaks the effect a little...
//#define MOVE_CAMERA


int spointer;
vec3 sunLight;
#define SUN_COLOUR vec3(1., .9, .8)
#define FOG_COLOUR vec3(1., .7, .7)

struct Stack
{
    vec3 pos;
    float alpha;
    float dist;
    int mat;

};

#define STACK_SIZE 8
Stack stack[STACK_SIZE];

//==============================================================================
//--------------------------------------------------------------------------
float getGroundHeight(vec2 p)
{
    float y =(sin(p.y*.23)+cos(p.x*.18))*.8;
    return y;
}
//--------------------------------------------------------------------------
mat3 getCamMat( in vec3 ro, in vec3 ta, float cr )
{
	vec3 cw = normalize(ta-ro);
	vec3 cp = vec3(sin(cr), cos(cr),0.0);
	vec3 cu = normalize( cross(cw,cp) );
	vec3 cv = normalize( cross(cu,cw) );
    return mat3( cu, cv, cw );
}

//--------------------------------------------------------------------------
// Loop the camposition around a uneven sine and cosine, and default the time 0
// to be steep at a loop point by adding 140...
vec3 getCamPos(float t)
{
    //t = sin(t*.01)*200.;
    t+=140.;
    vec3 p = vec3(3.0+50.0*sin(t*.03),
                  1.5,
                  4.0 + 50.0*cos(t*.044));
    p.y-=getGroundHeight(p.xz);
    return p;
}

//----------------------------------------------------------------------------------------
//  1 out, 1 in...
float hash11(float p)
{
    p = fract(p * .1031);
    p *= p + 33.33;
    p *= p + p;
    return fract(p);
}
//----------------------------------------------------------------------------------------
//  1 out, 2 in...
float hash12(vec2 p)
{
	vec3 p3  = fract(vec3(p.xyx) * .1031);
    p3 += dot(p3, p3.yzx + 33.33);
    return fract((p3.x + p3.y) * p3.z);
}

//  1 out, 3 in...
vec3 hash31(float p)
{
   vec3 p3 = fract(vec3(p) * vec3(.1031, .1030, .0973));
   p3 += dot(p3, p3.yzx+33.33);
   return fract((p3.xxy+p3.yzz)*p3.zyx); 
}

//  3 out, 3 in...
vec3 hash33(vec3 p3)
{
	p3 = fract(p3 * vec3(.1031, .1030, .0973));
    p3 += dot(p3, p3.yxz+33.33);
    return fract((p3.xxy + p3.yxx)*p3.zyx);
}


//------------------------------------------------------------------------------
float randomTint(vec3 pos)
{
    float r = texture(iChannel1, pos.xz*.0027).x;
    return r+.5;
}

//----------------------------------------------------------------------------------------
vec3 texCube(sampler2D sam, in vec3 p, in vec3 n )
{
	vec3 x = texture(sam, p.yz).xyz;
	vec3 y = texture(sam, p.zx).xyz;
	vec3 z = texture(sam, p.xy).xyz;
	return (x*abs(n.x) + y*abs(n.y) + z*abs(n.z))/(abs(n.x)+abs(n.y)+abs(n.z));
}

//------------------------------------------------------------------------------
vec4 grassTexture(vec3 pos, vec3 nor)
{
    
    float g = texture(iChannel1, pos.xz*.5).x;
    float s = texture(iChannel1, pos.xz*.015).x*.2;
    
    
    vec3 flower = texture(iChannel2, pos.xz*.15).xyz;
    float rand = texture(iChannel1, pos.xz*.003).x;
    rand *= rand*rand;
    
    flower =pow(flower,vec3(8, 15, 5)) *10. * rand;
    vec4 mat = vec4(g*.05+s, g*.65, 0, g*.1);
    mat.xyz += flower;

    // Do the red ground lines...
    pos = fract(pos);
    mat = mix(mat, vec4(.2, 0,0,0), smoothstep(.05, .0,min(pos.x, pos.z))
              					  + smoothstep(.95, 1.,max(pos.x, pos.z)));

    
	return min(mat, 1.0);
}

//------------------------------------------------------------------------------
vec4 barkTexture(vec3 p, vec3 nor)
{
    vec2 r = floor(p.xz / 5.0) * 0.02;
    float br = texture(iChannel1, r).x;
	vec3 mat = texCube(iChannel3, p*.4, nor) * vec3(.4, .3, .1*br) *br;
    mat += texCube(iChannel3, p*.53, nor)*smoothstep(0.0,.3, mat.x)*br;
   	return vec4(mat, .1);
}

//------------------------------------------------------------------------------
vec4 leavesTexture(vec3 p, vec3 nor)
{
    
    vec3 rand = texCube(iChannel2, p*.15,nor);
	vec3 mat = vec3(0.4,1.2,0) *rand;
   	return vec4(mat, .0);
}

//------------------------------------------------------------------------------
vec4 fruitTexture(vec3 p, vec3 nor, float i)
{
    
    
    float rand = texCube(iChannel2, p*.1 ,nor).x;
    float t = dot(nor, normalize(vec3(.8, .1, .1)));
	vec3 mat = vec3(1.,abs(t)*rand,0);
    mat = mix(vec3(0,1,0), mat, i/10.);

   	return vec4(mat, .5);
}



//------------------------------------------------------------------------------
float distanceRayPoint(vec3 ro, vec3 rd, vec3 p, out float h)
{
    h = dot(p-ro,rd);
    return length(p-ro-rd*h);
}

//------------------------------------------------------------------------------
const int   SEEDS = 8 ;
const float STEP_SIZE = 2.;
#define SIZE .03


// This seed code is the starfield stuff from iapafoto
// I've just removed the alpha part...
// https://www.shadertoy.com/view/Xl2BRR
mat2 rotMat2D(float a)
{
	float si = sin(a);
	float co = cos(a);
	return mat2(si, co, -co, si);
}

vec3 floatingSeeds(in vec3 ro, in vec3 rd, in float tmax)
{ 
 
    float d =  0.;
    ro /= STEP_SIZE;
	vec3 pos = floor(ro),
	     ri = 1./rd,
		 rs = sign(rd),
		 dis = (pos-ro + .5 + rs*0.5) * ri;
	
    float dint;
	vec3 offset, id;
    vec3 col = vec3(0);
    vec3 sum = vec3(0);
    //float size = .04;
    
	for( int i=0; i< SEEDS; i++ )
    {
        id = hash33(pos);

        offset = clamp(id+.2*cos(id*iTime),SIZE, 1.-SIZE);
        d = distanceRayPoint(ro, rd, pos+offset, dint);
        
        if (dint > 0. && dint * STEP_SIZE < tmax)
        {
            col = vec3(.4)*smoothstep(SIZE, 0.0,d);
            sum += col;
        }
		vec3 mm = step(dis.xyz, dis.yxy) * step(dis.xyz, dis.zzx);
		dis += mm * rs * ri;
        pos += mm * rs;
	}
  
	return sum * .7;
}

//--------------------------------------------------------------------------
float findClouds2D(in vec2 p)
{
	float a = 1.5, r = 0.0;
    p*= .000001;
    for (int i = 0; i < 5; i++)
    {
        r+= texture(iChannel1,p*=2.2).x*a;
        a*=.5;
    }
	return max(r-1.5, 0.0);
}
//------------------------------------------------------------------------------
// Use the difference between two cloud densities to light clouds in the direction of the sun.
vec4 getClouds(vec3 pos, vec3 dir)
{
    if (dir.y < 0.0) return vec4(0.0);
    float d = (4000. / dir.y);
    vec2 p = pos.xz+dir.xz*d;
    float r = findClouds2D(p);
    float t = findClouds2D(p+normalize(sunLight.xz)*30.);    
    t = sqrt(max((r-t)*20., .2))*2.;
    vec3 col = vec3(t) * SUN_COLOUR;
    // returns colour and alpha...
    return vec4(col, r);
} 


//------------------------------------------------------------------------------
// Thanks to Fizzer for the space-folded tree idea...
/// https://www.shadertoy.com/view/4tVcWR
vec2 map(vec3 p, float t)
{
 
    float matID, f;
    p.y += getGroundHeight(p.xz);
	float num = (floor(p.z/5.))*5.+(floor(p.x/5.0))*19.;
	p.xz = mod(p.xz, 5.0)-2.5;
    //p.xz *= rotMat2D(p.y*num/300.); // ... No, just too expensive. :)
    
    float d = p.y;
    matID = 0.0;

    float s=1.,ss=1.6;
    
    // Tangent vectors for the branch local coordinate system.
    vec3 w=normalize(vec3(-1.5+abs(hash11(num*4.)*.8),1,-1.));
    vec3 u=normalize(cross(w,vec3(0,1.,0.)));

    float scale=3.5;
    p/=scale;
    vec3 q = p;
    // Make the iterations lessen over distance for speed up...
    int it = 10-int(min(t*.03, 9.0));

	float h  = hash11(num*7.)*.3+.3;
    vec3 uwc = normalize(cross(u,w));
    int dontFold = int(hash11(num*23.0) * 9.0)+3;
    
    float thick = .2/(h-.24);
    for (int i = 0; i < it; i++)
    {
		f = scale*max(p.y-h,max(-p.y,length(p.xz)-.06/(p.y+thick)))/s;
        if (f <= d)
        {
            d = f;
            matID = 1.0;
        }

        // Randomly don't fold the space to give more branch types...
        if (i != dontFold)
        	p.xz = abs(p.xz);

        p.y-=h;
        p*=mat3(u,uwc,w);
        p*=ss;
		s*=ss;
    }

    float fr = .2;
    f = (length(p)-fr)/s;
    if (f <= d)
    {
        d = f;
        matID = 2.0;
    }
    
    q.y -= h*1.84;
    h *= 1.1;
    for (int i = 0; i < it; i++)
    {
      	p = (normalize(hash31(num+float(i+19))-.5))*vec3(h, 0.1, h);
     	p+=q;
        float ds =length(p)-.015;
     	if (ds <= d)
        {
            matID = 3.0+float(i);
         	d = ds;
        }
    }

	return vec2(d, matID);
}

//------------------------------------------------------------------------------
float sphereRadius(float t)
{
	t = abs(t-.0);
	t *= 0.003;
	return clamp(t, 1.0/iResolution.y, 3000.0/iResolution.y);
}

//------------------------------------------------------------------------------
float shadow( in vec3 ro, in vec3 rd, float dis)
{
	float res = 1.0;
    float t = .1;
	float h;
	
    for (int i = 0; i < 15; i++)
	{
        vec3 p =  ro + rd*t;

		h = map(p,dis).x;
		res = min(3.*h / t, res);
		t += h;
	}
    res += t*t*.08; // Dim over distance
    return clamp(res, .6, 1.0);
}

//-------------------------------------------------------------------------------------------
// Taken almost straight from Inigo's shaders, thanks man!
// But I've changed a few things like the for-loop is now a float,
// which removes the need for the extra multiply and divide in GL2
float calcOcc( in vec3 pos, in vec3 nor, float d )
{
	float occ = 0.0;
    float sca = 1.0;
    for(float h= 0.05; h < .3; h+= .07)
    {
		vec3 opos = pos + h*nor;
        float d = map( opos, d ).x;
        occ += (h-d)*sca;
        sca *= 0.95;
    }
    return clamp( 1.0 - 2.0*occ, 0.0, 1.0 );
}

//-------------------------------------------------------------------------------------------
float marchScene(in vec3 rO, in vec3 rD, vec2 co)
{
	float t = hash12(co)*.5;
	vec4 normal = vec4(0.0);
	vec3 p;
    float alphaAcc = 0.0;

    spointer = 0;
	for( int j=min(0,iFrame); j < 140; j++ )
	{
        // Check if it's full or too far...
		if (spointer == STACK_SIZE || alphaAcc >= 1.) break;
		p = rO + t*rD;
		float sphereR = sphereRadius(t);
		vec2 h = map(p, t);
		if( h.x <= sphereR)
		{
            //h = max(h,0.0);
            float alpha = (1.0 - alphaAcc) * min(((sphereR-h.x+.01) / sphereR), 1.0);
			stack[spointer].pos = p;
            stack[spointer].alpha = alpha;
            stack[spointer].dist = t;
            stack[spointer].mat = int(h.y);
            alphaAcc += alpha;
	        spointer++;
        }
		t +=  h.x+t*0.007;
	}
    return alphaAcc;
}	

//-------------------------------------------------------------------------------------------
vec3 lighting(in vec4 mat, in vec3 pos, in vec3 normal, in vec3 eyeDir, in float d)
{
  
	float sh = shadow(pos+sunLight*.01,  sunLight, d);
    float occ = calcOcc(pos, normal, d);
    // Light surface with 'sun'...
	vec3 col = mat.xyz * SUN_COLOUR*(max(dot(sunLight,normal)*.5+.2, 0.0))*sh;
    // Ambient...
	
    float fre = clamp(1.0+dot(normal,eyeDir),0.0,1.0)*.3;
    float bac = clamp(.8*dot( normal, normalize(vec3(-sunLight.x,0.0,-sunLight.z))), 0.0, 1.0 );
    normal = reflect(eyeDir, normal); // Specular...
	col += pow(max(dot(sunLight, normal), 0.0), 16.0)  * SUN_COLOUR * sh * mat.w * occ;
    col += bac*mat.xyz * occ;
    col += mat.xyz * abs(normal.y)*.3*occ;
	col += SUN_COLOUR * fre *.2*occ;

	return min(col, 1.0);
}

//------------------------------------------------------------------------------
vec3 getNormal2(vec3 p, float e)
{
    return normalize( vec3( map(p+vec3(e,0.0,0.0), 0.).x - map(p-vec3(e,0.0,0.0), 0.).x,
                            map(p+vec3(0.0,e,0.0), 0.).x - map(p-vec3(0.0,e,0.0), 0.).x,
                            map(p+vec3(0.0,0.0,e), 0.).x - map(p-vec3(0.0,0.0,e), 0.).x));
}

vec3 getNormal(vec3 pos, float ds)
{

    float c = map(pos, 0.).x;
    // Use offset samples to compute gradient / normal
    vec2 eps_zero = vec2(ds, 0.0);
    return normalize(vec3(map(pos + eps_zero.xyy, 0.0).x, map(pos + eps_zero.yxy, 0.0).x,
                          map(pos + eps_zero.yyx, 0.0).x) - c);
}


//------------------------------------------------------------------------------
vec3 getSky(vec3 dir)
{
	vec3 col = mix(vec3(FOG_COLOUR), vec3(.0, 0.4,0.6),(abs(dir.y)));
    return col;
}

vec2 rot2D(inout vec2 p, float a)
{
    return cos(a)*p - sin(a) * vec2(p.y, -p.x);
}

/* quaternions */

vec4 qmult(vec4 p, vec4 q) {
    vec3 pv = p.xyz, qv = q.xyz;
    return vec4(p.w * qv + q.w * pv + cross(pv, qv), p.w * q.w - dot(pv, qv));
}

vec4 qrotor(vec3 axis, float phi) {
    phi *= 0.5;
    return vec4(sin(phi) * normalize(axis), cos(phi));
}

vec3 rotate(vec3 point, vec4 rotor) {
    vec3 rotv = rotor.xyz;
    return qmult(rotor, vec4(point * rotor.w - cross(point, rotv), dot(point, rotv))).xyz;
}

vec4 slerp(vec3 u0, vec3 u1, float t) {
    return qrotor(cross(u0, u1), t * acos(dot(u0, u1)));
}
// Thaanks to DR2 for this matrix code...
mat3 viewMat (float ay, float az)
{
  vec2 o, ca, sa;
  o = vec2 (ay, az);
  ca = cos (o);
  sa = sin (o);
  return mat3 (ca.y, 0., - sa.y, 0., 1., 0., sa.y, 0., ca.y) *
         mat3 (1., 0., 0., 0., ca.x, - sa.x, 0., sa.x, ca.x);
}

//==============================================================================
void mainImage( out vec4 fragColour, in vec2 fragCoord )
{
    vec2 mouseXY = iMouse.xy / iResolution.xy;
    vec2 uv = (-iResolution.xy + 2.0 * fragCoord ) / iResolution.y;
    sunLight = normalize(vec3(-.8,1.8,-1.5));
    
    // Camera stuff...
    // A simple cylindrical projection with normal Y
    float time = iTime*1.+mouseXY.x*10.0;
    
    #ifdef MOVE_CAMERA
	
    vec3 camera = getCamPos(time*.2);
	vec3 lookat = getCamPos(time*.2+10.);

    #else
    
    // Don't move the camera bodge...
    vec3 camera = getCamPos(0.0);
	vec3 lookat = getCamPos(0.0);
    lookat += vec3(40.0*sin(time * 5.28), 0., 40.0*cos(time * .28) );

    #endif
    
    float ride = sin(time*.5)*.4;
    
    
    //mat3 camMat = getCamMat(camera, lookat, 0.0);
    //uv *= .7;
    // I didn't want to normalize here because it'll distort across the vertical slightly...
    // But it looks like it's needed for a unit ray length...
//	vec3 seedDir = camMat * normalize(vec3(0.0, uv.y,  1.));
//	seedDir.xz = rot2D(seedDir.xz, uv.x*1.13); // Adjusted for rotation PI (roughly!) :)
    

    // Now using Quaternions, thanks to munrocket for the Quat code.
    // https://www.shadertoy.com/view/3stXR2
// Quarternion version..
//    vec3 seedDir  = normalize(vec3(0,0, 1.));
//    vec2 rotXY = vec2(uv.x+time*.5, -uv.y+ride);
//    vec4 rotor = qrotor(vec3(0., 1., 0.), rotXY.x);
//    rotor = qmult(rotor, qrotor(vec3(1., 0., 0.), rotXY.y));
    //seedDir = rotate(seedDir, rotor);
    
    vec3 seedDir  = normalize(vec3(0,0, 1.));
    seedDir = viewMat (uv.y+ride, uv.x+time*.5)*seedDir;
  
    

	vec3 rd = seedDir;

	vec3 col = vec3(0);

	vec3 sky  = getSky(rd);
  

    // Build the stack returning the final alpha value...
    float alpha = marchScene(camera, rd, fragCoord);
    vec4 mat;
    // Render the stack...
    if (alpha > .0)
    {
        for (int i = 0; i < spointer; i++)
        {
            vec3  pos = stack[i].pos; 
            float d = stack[i].dist;
            
            vec3 nor =  getNormal(pos, sphereRadius(d));
            int matID = stack[i].mat;
            if (matID == 0) mat =  grassTexture(pos, nor);
            else
				if (matID == 1) mat = barkTexture(pos, nor);
            else
                if (matID == 2) mat = leavesTexture(pos, nor);
            else
                mat = fruitTexture(pos, nor, float(matID - 3));

            mat *= randomTint(pos);
 
            vec3  temp = lighting(mat, pos, nor, rd, d);
            if (matID == 3) temp=temp*.4+vec3(.15, .01,0);
            
            temp = mix(sky, temp , exp(-d*.01));
            col += temp * stack[i].alpha;
        }
    }
    vec4 cc = getClouds(camera, rd);
    sky+= pow(max(dot(sunLight, rd), 0.0), 20.0)*SUN_COLOUR*.03;
    //sky = clamp(sky, 0.0,1.);
	sky = mix(sky, cc.xyz, cc.w);
	col += sky *  (1.0-alpha);
    
    float d = stack[0].dist;
    col+= floatingSeeds(camera, rd, d);
    
   
    // Sun glow effect...
    col+=pow(max(dot(sunLight, rd), 0.0), 6.0)*SUN_COLOUR*.2;
    
    // Clamp and contrast...
    //col = col * vec3(1.1, 1.1,.9);
    col = clamp(col,0.,1.);
    col = col*col*(3.0-2.0*col);

    
    // The usual vignette...which manages to add more vibrancy...
    vec2 q = fragCoord / iResolution.xy;
    col *= 0.3 + 0.7*pow(90.0*q.x*q.y*(1.0-q.x)*(1.0-q.y), 0.5);
	// A nice fade in start...
    
    
    col *= smoothstep(0.0, 5.0, time);
    fragColour = vec4(sqrt(col), 1.0);
    
}
