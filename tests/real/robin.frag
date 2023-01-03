// Copied from https://www.shadertoy.com/view/4tl3RM
// Robin
// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.
// Created by David Hoskins.

// The winter Robin - a UK resident bird.
// They occasionally sing at night next to street lights.
// Despite their cute appearance, they are aggressively territorial. 

// Sphere tracing based on eiffie's circle-of-confusion ideas.
// Distance estimation shapes, like 'Segment' - thanks to iq.

//--------------------------------------------------------------------------
#define SUN_COLOUR vec3(1., .76, .6)

vec4 animParts; // .x = Puff Chest, head tilt, head nod, .w = tweet!
vec4 body;  	// .x = Tilt down, wings, jump, .w = crouching. 
vec2 zoomTurn;  // Zoom and turn hacked into a vec2 to keep the global vars low.

vec4 aStack[2];
vec4 dStack[2];

// CubeMap OpenGL clamping fix, thanks to w23/reinder...
vec3 CubeMap(in samplerCube sam, in vec3 v, float size)
{
   float M = max(max(abs(v.x), abs(v.y)), abs(v.z));
   float scale = (float(size) - 1.) / float(size);
   if (abs(v.x) != M) v.x *= scale;
   if (abs(v.y) != M) v.y *= scale;
   if (abs(v.z) != M) v.z *= scale;
   return texture(sam, v).xyz;
}

//----------------------------------------------------------------------------------------
float Hash(float p)
{
	vec2 p2 = fract(vec2(p * 5.3983, p * 5.4427));
    p2 += dot(p2.yx, p2.xy + vec2(21.5351, 14.3137));
	return fract(p2.x * p2.y * 95.4337);
}

float Hash(vec2 p)
{
	p  = fract(p * vec2(5.3983, 5.4427));
    p += dot(p.yx, p.xy + vec2(21.5351, 14.3137));
	return fract(p.x * p.y * 95.4337);
}

//----------------------------------------------------------------------
float noise( in vec3 x )
{
    vec3 p = floor(x);
    vec3 f = fract(x);
	f = f*f*(3.0-2.0*f);
	
	vec2 uv = (p.xy+vec2(37.0,17.0)*p.z) + f.xy;
	vec2 rg = textureLod( iChannel1, (uv+ 0.5)/256.0, 0.0 ).yx;
	return mix( rg.x, rg.y, f.z );
}

//----------------------------------------------------------------------
float fbm( vec3 p )
{
    float f;
    f  = 1.600*noise( p ); p = p*2.02;
    f += 0.3500*noise( p ); p = p*2.33;
    f += 0.2250*noise( p ); p = p*2.01;
    return f;
}

vec2 Rot2(vec2 p, float a)
{
	float si = sin(a);
	float co = cos(a);
	return mat2(co, si, -si, co) * p;
}
float Noise(float n)
{
    float f = fract(n);
    n = floor(n);
    f = f*f*(3.0-2.0*f);
    return mix(Hash(n), Hash(n+1.0), f);
   
}

float NoiseSlope(float n, float loc)
{
    float f = fract(n);
    n = floor(n);
    f = smoothstep(0.0, loc, f);
    return mix(Hash(n), Hash(n+1.0), f);
   
}
//----------------------------------------------------------------------------------------
float  Sphere( vec3 p, float s )
{
    return length(p)-s;
}

//--------------------------------------------------------------------------
vec3 TexCube(in vec3 p, in vec3 n )
{
	vec3 x = texture(iChannel0, p.yz ).xzy;
	vec3 y = texture(iChannel0, p.zx ).xyz;
    //y = y*y;
	vec3 z = texture(iChannel1, p.xy, 2.0).yyy;
	return (x*abs(n.x) + y*abs(n.y) + z*abs(n.z))/(abs(n.x)+abs(n.y)+abs(n.z))*.9;
}

float Cylinder( vec3 p, vec2 h )
{
  vec2 d = abs(vec2(length(p.xz),p.y)) - h;
  return min(max(d.x,d.y),0.0) + length(max(d,0.0));
}

//----------------------------------------------------------------------------------------
float Segment(vec3 p,  vec3 a, vec3 b, float r1, float r2)
{
	vec3 pa = p - a;
	vec3 ba = b - a;
	float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
	return length( pa - ba*h ) - r1 + r2*h;
}

//----------------------------------------------------------------------------------------
float RoundBox( vec3 p, vec3 b, float r )
{
	return length(max(abs(p)-b,0.0))-r;
}

//----------------------------------------------------------------------------------------
float sMin( float a, float b, float k )
{
    
	float h = clamp(0.5 + 0.5*(b-a)/k, 0.0, 1.0 );
	return mix( b, a, h ) - k*h*(1.-h);
}
//----------------------------------------------------------------------------------------
vec3 Colour( vec3 p, vec3 nor, out float spec)
{
    vec3 post = p; 
    p.xz = Rot2(p.xz,  zoomTurn.x);
    vec3 orig = p;
    p.y-=body.z;
    p.yz = Rot2(p.yz, body.x);
    vec3 mat = TexCube(p*.5, nor).zxy * .45;
    
	spec = 0.0;
    
    // Body
	float d = RoundBox(p-vec3(0.0, -1.4,-.3),vec3(.3+.1*animParts.x, .0, .1+.3*animParts.x), .3);
    d = sMin(d, Sphere(p-vec3(0.0, -.25,0.0), 1.33), 2.2);
    
    // Wings...
  	vec3 p2 = p;
    p2.x = abs(p2.x);
    d = sMin(d,Segment(p2,vec3(1.5, -.2,1.7), vec3(1.5, -1.,-1.6), .3, .3), .4);
    vec3 wing = vec3(.4, 0.32, 0.2)*texture(iChannel1, p2.zy*vec2(.05, .3)).x;
    mat = mix(wing*wing, mat, clamp(Segment(p2,vec3(1.3, 0.2,.2), vec3(1.5+body.y, -1.,-1.6), .4, .4), 0., 1.0));
    
    // Tail...
    vec3 tail = vec3(.4)*texture(iChannel1, p.zy*vec2(.05, .3)).x;
    mat = mix(tail*tail, mat, clamp(Segment(p2,vec3(.1, -.5, -1.5), vec3(.15, -1.2, -8.0), .2, .2), 0.0, 1.0));
    
    p2 = p;
    p2.xy = Rot2(p2.xy, animParts.y); // Tilt head
    p2.zy = Rot2(p2.zy, animParts.z); // Nod
   
    // Red Breast and head...
    d = sMin(d, Sphere(p2-vec3(0.0, 1.,1.4), .8), 1.0);
    float f = Sphere(p-vec3(0.0, -.9, 2.1), 1.5);
    f = min(f, Sphere(p2-vec3(0.0, .5, 1.9),1.2));
    f += fbm(p*20.0)*.2;
   	f = max(f, -Sphere(p2-vec3(0.0, 2.0, 1.2), 1.1));
    mat = mix(mat,vec3(.4, .1, 0.0) * (.8+fbm(p*12.)*.2), clamp((d-f)*3.0 - (1.0-step(-3.0, orig.y))*3.0,0.0, 1.0));
 
    // Beak...
    if (d > Segment(p2-vec3(0.0, .9,2.5),vec3(0.0, 0.0,-.4), vec3(.0, animParts.w*.2,.4), .15, .1))
    {
        spec = .2;
        mat = vec3(0.02, 0.01, 0.);
    }
    // Black shiny eyes
    p2.x = abs(p2.x);
    if (Sphere(p2-vec3(.35, 1.1,2.05), .11) < d)
    {
    	mat = vec3(0.);
        spec = .25;
    }
    // Post...
    vec3 pCol = mix(vec3(.2), vec3(.05,.04, .0),fbm(post*2.0)*.5);
    pCol = mix( pCol,vec3(.05), abs(sin( length(post.xz-vec2(0.2, .4))*34.0))*abs(nor.y));
    mat = mix(pCol,mat, step(-3.4, post.y));
    
    // Legs...
    orig.y-=body.w;
    orig.x = abs(orig.x);
    f = 1.0-clamp(Segment(orig,vec3(0.5, -2.0-body.z,-.5), vec3(.8, -3.5,0.6), .53, .03)+(1.0-step(-3.5,orig.y))*50.0, .0, 1.0);
    mat = mix( mat, vec3(.05,.02, .02), f);
    spec = mix(spec, .1, f);

    return mat;

    
}

//--------------------------------------------------------------------------

float Map( vec3 p )
{
	float d;
    vec3 post = p;
    
    p.xz = Rot2(p.xz,  zoomTurn.x);
	vec3 o = p;
    
    p.y-=body.z;
    p.yz = Rot2(p.yz, body.x);
    // Body 
    d = RoundBox(p-vec3(0.0, -1.4,-.3),vec3(.3+.1*animParts.x, .0, .1+.3*animParts.x), .3);
    d = sMin(d, Sphere(p-vec3(0.0, -.25,0.0), 1.33), 2.2);
    
	//Wings...    
	vec3 p2 = p;
    p2.x = abs(p2.x);
    d = sMin(d,Segment(p2,vec3(1.3, 0.2,.2), vec3(1.5+body.y, -.5,-1.6), .2, .2), .4);
  
   
    // Tail...
    d = sMin(d,Segment(p2,vec3(.6, -.5, -1.5), vec3(.15, -1.2, -8.0), .2, .2), 2.4);
    
    // Rotate head..
    p.xy = Rot2(p.xy, animParts.y);
   	p.zy = Rot2(p.zy, animParts.z);
   
    // Head...
    d = sMin(d, Sphere(p-vec3(0.0, 1.,1.4), .8), 1.0);
    //animParts.w = .1;
    // Beak...
    d = sMin(d,Segment(p-vec3(0.0, 1.,2.5),vec3(0.0, 0.0,-.4), vec3(.0, animParts.w*.2,.15), .1, .096), .2);
    d = sMin(d,Segment(p-vec3(0.0, 1.05,2.5),vec3(0.0, -animParts.w*.25,-1.5), vec3(.0, -animParts.w,.16), .1, .1), .05);
    // Eyes...
    p.x = abs(p.x);
    d = min(d, Sphere(p-vec3(.35, 1.1,2.05), .11));
    // Post...
	d = min(d, Cylinder(post-vec3(0.2, -12.0, .4), vec2(1.8, 8.5)- fbm(post*5.0)*.1));
    // Legs...
    o.x = abs(o.x);
    o.y-=body.w;
    d = min(d, Segment(o,vec3(0.5, -2.+body.z+body.w,-.5), vec3(.8, -3.5,0.5), .09, .1));
    d = min(d, Segment(o,vec3(.8, -3.5,0.5), vec3(.8, -3.5,1.2), .04, .04));
    d = min(d, Segment(o,vec3(.8, -3.5,0.5), vec3(1.4, -3.5,0.8), .04, .04));
    d = min(d, Segment(o,vec3(.8, -3.5,0.5), vec3(.1, -3.5,0.8), .04, .04));

    return d;
}


//--------------------------------------------------------------------------
float Shadow( in vec3 ro, in vec3 rd)
{
	float res = 1.0;
    float t = 0.01;
	float h;
	
    for (int i = 0; i < 8; i++)
	{
		h = Map( ro + rd*t );
		res = min(2.5*h / t, res);
		t += h*.5+.05;
	}
    return max(res, 0.0);
}

////--------------------------------------------------------------------------
vec3 DoLighting(in vec3 mat, in vec3 pos, in vec3 normal, in vec3 eyeDir, in float d, in float specular)
{
    vec3 sunLight  = normalize( vec3(  -.1, 0.4,  0.3 ) );
	float sh = Shadow(pos,  sunLight);
    // Light surface with 'sun'...
	vec3 col = mat * SUN_COLOUR*(max(dot(sunLight,normal), 0.0)) *sh;
    // Ambient...
    col += mat * CubeMap(iChannel3, normal, 64.0)*.5;
    
    normal = reflect(eyeDir, normal); // Specular...
    col += pow(max(dot(sunLight, normal), 0.0), 1.0)  * SUN_COLOUR* texture(iChannel3, normal).xyz * specular *sh;
	return col;
}


//--------------------------------------------------------------------------
vec3 GetNormal(vec3 p, float sphereR)
{
	vec2 eps = vec2(.01, 0.0);
	return normalize( vec3(
           Map(p+eps.xyy) - Map(p-eps.xyy),
           Map(p+eps.yxy) - Map(p-eps.yxy),
           Map(p+eps.yyx) - Map(p-eps.yyx) ) );
}

//--------------------------------------------------------------------------
float SphereRadius(in float t)
{
	return t*.012;
	//return max(t, 4.0/iResolution.x);
}

//--------------------------------------------------------------------------
float Scene(in vec3 rO, in vec3 rD, in vec2 fragCoord)
{
    //float t = 0.0;
	float t = 2.+.1 * Hash(fragCoord.xy*fract(iTime));
	float  alphaAcc = 0.0;
	vec3 p = vec3(0.0);
    int hits = 0;

	for( int j=0; j < 40; j++ )
	{
		if (hits == 8 || alphaAcc >=1.0 || t > 45.0) break;
		p = rO + t*rD;
		float sphereR = SphereRadius(t);
		float h = Map(p);
        // Is it within the sphere?...
		if( h < sphereR)
		{
			// Accumulate the alphas with the scoop of geometry from the sphere...
            // Think of it as filling up an expanding ice-cream scoop flying out of the camera! 
			float alpha = max((1.0 - alphaAcc) * min(((sphereR-h) / sphereR), 1.0),0.0);
			// put it on the 2 stacks, alpha and distance...
			aStack[1].yzw = aStack[1].xyz; aStack[1].x = aStack[0].w; aStack[0].yzw = aStack[0].xyz; aStack[0].x = alpha;
			dStack[1].yzw = dStack[1].xyz; dStack[1].x = dStack[0].w; dStack[0].yzw = dStack[0].xyz; dStack[0].x = t;
			alphaAcc += alpha;
			hits++;
		}
		t +=  h*.8;
	}
	return clamp(alphaAcc, 0.0, 1.0);
}


//--------------------------------------------------------------------------
vec3 PostEffects(vec3 rgb, vec2 xy)
{
	// Gamma first...
	rgb = sqrt(rgb);

	// Then...
	#define CONTRAST 1.1
	#define SATURATION 1.2
	#define BRIGHTNESS 1.15
	rgb = mix(vec3(.5), mix(vec3(dot(vec3(.2125, .7154, .0721), rgb*BRIGHTNESS)), rgb*BRIGHTNESS, SATURATION), CONTRAST);

	// Vignette...

	rgb *= .5 +.5* pow(100.0*xy.x*xy.y*(1.0-xy.x)* (1.0-xy.y), 0.4);	

        
	return clamp(rgb, 0.0, 1.0);
}


//--------------------------------------------------------------------------
vec3 CameraPath( float t )
{
    //t = sin(t*.3);
    t+= 5.0;
    vec3 p = vec3(1.4+sin(t)*3.5, -.2, 5.0-+sin(t)*2.5);
	return p;
} 

float TweetVolume(float t)
{
    float n = NoiseSlope(t*11.0, .1) * abs(sin(t*14.0))*.5;
    n = (n*smoothstep(0.4, 0.9, NoiseSlope(t*.5+4.0, .1)));
    return n;
}
    

//--------------------------------------------------------------------------
void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
	float m = (iMouse.x/iResolution.x)*20.0;
	float gTime = iTime;
    vec2 xy = fragCoord.xy / iResolution.xy;
	vec2 uv = (-1.0 + 2.0 * xy) * vec2(iResolution.x/iResolution.y,1.0);
    
    float t = mod(gTime, 40.0);
     zoomTurn.y = smoothstep(6.0, 0.0, t)+smoothstep(39.0, 40.0, t);
	
	vec3 cameraPos 	= CameraPath(gTime*.2);
    cameraPos.z +=  zoomTurn.y*8.0;
    
	vec3 camTarget 	= vec3(Noise(gTime*1.9-30.0)*.5-.25,Noise(gTime*2.0)*.5-.3,.0);
  

	vec3 cw = normalize(camTarget-cameraPos);
	vec3 cp = vec3(0.0, 1.0, 0.0);
	vec3 cu = normalize(cross(cw,cp));
	vec3 cv = cross(cu,cw);
	vec3 dir = normalize(uv.x*cu + uv.y*cv + (1.0-  zoomTurn.y*.4)*cw);
    
   	// Puff Chest...
    animParts.x = Noise(gTime*2.0)+1.0;
    // head tilt...
    animParts.y = (NoiseSlope(gTime*1.2+sin(gTime*.3)*.7+.7, .05)-.5)* 1.5;
    // Nod...
    animParts.z = (NoiseSlope(gTime*.4-33.0, .05))* .5;
    // Tweet...

    animParts.w = TweetVolume(gTime)*.8+.02;
    body.x  = sin(NoiseSlope(gTime*.5, .3)*4.14) *.25+.25;
    body.y  = NoiseSlope(gTime*.73, .3);
    t = mod(gTime*2., 15.0);
    body.z = -(smoothstep(0.0, .5, t)* smoothstep(.6,.5, t )) * .5;
    animParts.z -= body.z; 
    body.y = body.z;
    float jump = smoothstep(0.4, .6, t)* smoothstep(.8,.6, t ) * .8;
    body.z += jump;
    body.w = jump;
    zoomTurn.x = smoothstep(0.4, .8, t);
    if (mod(gTime*2.0, 30.0) >= 15.0)
    {
		 zoomTurn.x = 1.0- zoomTurn.x;
    }
	vec3 col = vec3(.0);
	
    for (int i = 0; i <2; i++)
    {
		dStack[i] = vec4(-20.0);
        aStack[i] = vec4(0.0);
    }
	float alpha = Scene(cameraPos, dir, fragCoord);
    
    // Render both stacks...
    for (int s = 0; s < 2; s++)
    {
        for (int i = 0; i < 4; i++)
        {
            float specular = 0.0;
            
            float d = dStack[s][i];
            if (d < 0.0) continue;
            float sphereR = SphereRadius(d);
            vec3 pos = cameraPos + dir * d;
            vec3 normal = GetNormal(pos, sphereR);
            vec3 alb = Colour(pos, normal, specular);
            col += DoLighting(alb, pos, normal, dir, d, specular)* aStack[s][i];
		}
    }
    // Fill in the rest with woodland background...
    float mi = smoothstep(1.9, .0,  zoomTurn.y);
    vec3 back = mix(CubeMap(iChannel2, dir, 128.0), CubeMap(iChannel3, dir, 64.0)*.7, mi);
    col += back* back*SUN_COLOUR*  (1.0-alpha);
    

	col = PostEffects(col, xy) * smoothstep(.0, 2.0, iTime);	
	
	fragColor=vec4(col,1.0);
}
