#version 110
#define float4 vec4
#define float3 vec3
#define float2 vec2
#define oUV gl_TexCoord[0]
#define tex3D texture3D
#define tex2D texture2D
#define tex1D texture1D

#define SSAO se= ep+ddd*sign (dot (ray,norm))*ray; \
occluderFragment = tex2D(texture2d_1,se.xy); \
shadow=occluderFragment.w;\
occluderFragment.a=tex2D (texture2d_2,se.xy).x;\
occNorm = (occluderFragment.xyz*2.0)-vec3 (1.0);\
depthDifference = depth-occluderFragment.a;\
normDiff = 1.0-1.0*dot(occNorm,norm);\
addition=step(0.00002,depthDifference)*normDiff*(1.0-smoothstep(0.000002,0.20,depthDifference));\
 bl += addition;







				uniform sampler2D texture2d_1;
				uniform sampler2D texture2d_2;
				uniform sampler2D texture2d_3;
				uniform sampler2D texture2d_projected;
				uniform sampler2D texture2d_random;
				uniform float Limit;
				uniform float fttime;
uniform mat4 ProjectionMatrix;











void main (void) 
{
float4 cl;
float depth;



// random vector
vec3 fres = normalize((texture2D(texture2d_random,oUV.xy*2.0+float2 (Limit*1.0)).xyz*2.0) - vec3(1.0));

// depth 
depth=tex2D (texture2d_2,oUV.xy).x;

vec3 ep=vec3 (oUV.xy,depth);
float bl=0.0;
vec3 ray;
vec3 se;
vec3 occNorm;
float depthDifference,normDiff;
float3 norm=((tex2D (texture2d_1,oUV.xy).xyz-0.5)*2.0);
float4 GlobalIllumination=float4 (0.0);
bl=1.0;



float ddd=0.002;
float shadow;
float addition;
vec4 occluderFragment;

	//for (int i=0;i<24;i++) 

bool occluder;
occluder=true;
//if (tex2D(texture2d_1,oUV.xy).w<0.9) occluder=false;

if (occluder) { 


//1
ray= reflect (vec3(0.53812504, 0.18565957, -0.43192),fres);
SSAO
//2
ray= reflect (vec3(0.13790712, 0.24864247, 0.44301823),fres);
SSAO
//3
ray= reflect (vec3(0.33715037, 0.56794053, -0.005789503),fres);
SSAO
//4
ray= reflect ( vec3(-0.6999805, -0.04511441, -0.0019965635),fres);
SSAO
//5
ray= reflect ( vec3(0.06896307, -0.15983082, -0.85477847),fres);
SSAO
//6
ray= reflect (vec3(0.056099437, 0.006954967, -0.1843352),fres);
SSAO
//7	
ray= reflect ( vec3(-0.014653638, 0.14027752, 0.0762037),fres);
SSAO
//8
ray= reflect ( vec3(0.010019933, -0.1924225, -0.034443386),fres);
SSAO	
//9	
ray= reflect ( vec3(-0.35775623, -0.5301969, -0.43581226),fres);
SSAO
//10	
ray= reflect ( vec3(-0.3169221, 0.106360726, 0.015860917),fres);
SSAO
//11
ray= reflect (  vec3(0.010350345, -0.58698344, 0.0046293875),fres);
SSAO
//12 

ray= reflect ( vec3(-0.08972908, -0.49408212, 0.3287904),fres);
SSAO
//13
ray= reflect ( vec3(0.7119986, -0.0154690035, -0.09183723),fres);
SSAO
//14
ray= reflect ( vec3(-0.053382345, 0.059675813, -0.5411899),fres);
SSAO
//15
ray= reflect (  vec3(0.035267662, -0.063188605, 0.54602677),fres);
SSAO
//16
/*
ray= reflect (   vec3(-0.47761092, 0.2847911, -0.0271716),fres);
SSAO
//17 
ray= reflect ( vec3 ( -0.559644, -0.554896, -0.61554),fres);
SSAO
//18
ray= reflect (vec3 ( 0.65487, 0.496541, -0.569729),fres);
SSAO
//19 
ray= reflect (vec3 ( 0.382319, -0.719566, 0.579705),fres);
SSAO
//20
ray= reflect ( vec3 ( 0.800123, 0.344357, 0.491142),fres);
SSAO
//21
ray= reflect (  vec3 ( 0.426191, -0.414526, 0.804071),fres);
SSAO
*/



bl=1.0-bl*0.09*5.10;
//bl=1.0-bl*0.09*0.80;



//bl=1.0-clamp (bl,0.0,1.0);
//bl*=2.0;
//bl+=0.02;
}

//bl=0.0;
norm=normalize(norm);
// SS lighting
float ccl=1.0-0.0*pow(dot (norm,float3 (0.0,0.0,1.0)),2.0);	

// shadow

//bl=1.0;
//bl*=1.0-tex2D(texture2d_1,oUV.xy).w*0.5;

 
// SS Reflections
//float4 NormMot=norm.xyzz;
///float2 DiffusedUV=norm.xy*0.025+oUV.xy;
///float4 Refl=2.5*tex2D (texture2d_3,DiffusedUV);


gl_FragData[0]=bl*ccl*tex2D(texture2d_3,oUV.xy);

gl_FragData[0]=tex2D(texture2d_1,oUV.xy);
gl_FragData[1]=tex2D(texture2d_3,oUV.xy);


int count=0;
vec4 Comp=vec4 (0.0);
vec4 Comp2=vec4 (0.0);
for (int y=-3;y<4;y++) {
	for (int x=-3;x<4;x++){
count++;
float2 dd=float2 (x,y)*0.0005+oUV.xy;
Comp+=tex2D (texture2d_1,dd);
	}
}

Comp/=float (count);
Comp2/=float (count);
vec4 ResDP=tex2D (texture2d_1,oUV.xy);

Comp*=2.0;
Comp-=vec4 (1.0);

ResDP*=2.0;
ResDP-=vec4 (1.0);


float dt=1.0*length (Comp.xyz-ResDP.xyz);
dt=4.0+3.8*(1.0-max (dt,1.0*(abs(dot (Comp,ResDP)))));
//dt=10.0*(max (dt,(abs(dot (Comp,ResDP)))));
//dt=0.2*abs(dot (Comp,ResDP));
//dt=(3.0+3.0*(abs(dot (Comp,ResDP))));
//dt=1.0;
//dt=1.0;
dt+=0.4;

gl_FragData[0]=tex2D (texture2d_3,oUV.xy)*1.0*vec4 (dt);

// fogging
gl_FragData[0]*=1.0-vec4 (pow (tex2D (texture2d_2,oUV.xy).x,30.0));

if (fttime>260.0)
gl_FragData[0]=tex2D (texture2d_3,oUV.xy);


//if (sin (oUV.y*1100.0)<0.0) gl_FragData[0]*=2.8*vec4 (0.6,0.6,0.6,0.6);
//if (sin (oUV.x*1200.0)<0.0) gl_FragData[0]*=vec4 (1.0,0.6,0.6,0.6);
//if (int(oUV.y*100.0)%2==0) gl_FragData[0]=vec4 (0.0);

//gl_FragData[0]+=1.3*vec4 (pow(tex2D (texture2d_3,oUV.xy).x,3.0));

//gl_FragData[0]*=0.3;
//tex2D (texture2d_3,oUV.xy);


//dt=10.0*length (Comp.xyz-ResDP.xyz);
//gl_FragData[0]=vec4 (dt);

//gl_FragData[0]=tex2D (texture2d_3,oUV.xy);
//gl_FragData[0]=vec4 (dt);
//gl_FragData[0]=tex2D (texture2d_3,oUV.xy)*vec4 (bl);

//gl_FragData[0]=vec4(bl)*tex2D(texture2d_3,oUV.xy)+0.0*vec4 (0.0,0.05,0.1,0.0);;

/*
if (oUV.y<0.5)
{
if (oUV.x>0.5)
gl_FragData[0]=tex2D(texture2d_3,vec2 (1.0-oUV.x,oUV.y));
else
gl_FragData[0]=tex2D(texture2d_3,oUV.xy);
}
else
{
	
if (oUV.x>0.5)
gl_FragData[0]=tex2D(texture2d_3,vec2 (1.0-oUV.x,1.0-oUV.y));
else
gl_FragData[0]=tex2D(texture2d_3,vec2 (oUV.x,1.0-oUV.y));
}
*/

/*
float ff=0.5*3.1515926535+atan ((oUV.x-0.5)/(oUV.y-0.5));
ff=sin (0.5*ff);
float fd=length (oUV.xy-vec2 (0.5));
///ff*=2.0;
//ff*=0.1;
vec2 Res=fd*vec2 (sin(ff),cos(ff));

gl_FragData[0]=tex2D (texture2d_3,Res);
//0.5+0.5*vec4(ff);

*/



//gl_FragData[0]=tex2D(texture2d_3,oUV.xy)*1.2;

//gl_FragData[0]=5.0*vec4 (pow(tex2D(texture2d_2,oUV.xy).x-0.7,4.0));
//gl_FragData[0]=vec4(bl)*0.2+0.1*tex2D(texture2d_2,oUV.xy);

//gl_FragData[0]=vec4(bl);
/*
if (occluder) 
gl_FragData[0]=vec4(1.0);
else
gl_FragData[0]=vec4(0.0);
*/

// Highlights
//if (tex2D (texture2d_1,oUV.xy).w>0.9) gl_FragData[0]=tex2D (texture2d_3,oUV.xy);
//gl_FragData[0].w=tex2D (texture2d_1,oUV.xy).w;

//gl_FragData[1]=tex2D(texture2d_3,oUV.xy);

//gl_FragData[0]=tex2D (texture2d_2,oUV.xy);




}














