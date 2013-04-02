#version 110
#define float4 vec4
#define float3 vec3
#define float2 vec2
#define oUV gl_TexCoord[0]
#define tex3D texture3D
#define tex2D texture2D
#define tex1D texture1D

				uniform sampler2D texture2d_1;
				uniform sampler2D texture2d_velocities;
				uniform sampler2D texture2d_2;
				uniform sampler2D texture2d_random;
/*				uniform sampler1D texture_transfer_function;
				uniform sampler1D texture_palette_function;

					
				uniform float3 VolumeSize;
				uniform float3 RelativeSize;
				uniform float3 Registration_Offset;
				uniform float pet_alpha_blending;
				uniform float3 MousePos;
				uniform int Projection_mode;
uniform float3 InVolumePosition;
				uniform float3 Windowing_Levels;

				uniform float isosurface;
uniform float Sharpening;

*/

				uniform float Limit;
				uniform float Flo;

uniform float PosY;
uniform vec3 MotionBlur;





void main (void) 
{

vec2 MotionBlurPos=vec2 (0.0,0.0);



//float ff=tex2D (texture2d_1,oUV.xy).x;
//ff=abs (ff-cl.x);
//ff=0.5+0.5*sin(ff*10000.0);
//cl=ff*vec4 (1.0);



/*


float PI=3.1415926535;
float2 UV=oUV.xy;

UV-=float2 (0.5,0.5);
UV*=2.0;


float3 DirV =float3 (UV.x,UV.y,0.6);
float lgn=length (DirV);
DirV/=lgn;

UV.xy=UV.xy+(DirV.xy-UV.xy)*-0.0;
UV*=0.5;
UV+=float2 (0.5,0.5);
*/
	float2 UV=oUV.xy;
float4 cl=tex2D (texture2d_1,UV.xy);

cl=float4 (0.0);
float3 Direction=tex2D (texture2d_velocities,UV.xy).xyz;
//Direction.xy=float2 (0.5);
Direction.xyz-=float3 (0.5);
Direction*=2.0;

float alpha=1.0*length (Direction.xyz);
alpha=clamp (alpha,0.0,1.0);
//alpha=1-alpha;
//alpha*=alpha;
//alpha=1-alpha;

//alpha*=alpha;
//alpha=0.1;
Direction*=0.01;
if (alpha>0.01)
{
	for (int t=0;t<10;t++) { 
UV.xy-=Direction.xy;
	cl+=tex2D (texture2d_1,UV.xy);
};
cl*=1.0/10.0;
//cl.x+=0.15*alpha;
//=float4 (1.0,0.0,0.0,1.0)*alpha;
}
else
cl=tex2D (texture2d_1,UV.xy);

float4 clnow=tex2D( texture2d_1,oUV.xy);

cl=cl+(clnow-cl)*0.5;

UV=oUV.xy;






// blur effect
float lgn2=length (float2 (1.0,2.0)*(UV.xy-float2 (0.5)));
lgn2*=lgn2;
float xxx=1.0+(gl_Color.x)*5.0;
//xxx=1.0;
xxx=clamp (xxx,1.0,100.0);
//pow(lgn2,1.8);
int count=0;
{
	for (int y=-3;y<3;y++) {
	for (int x=-3;x<3;x++){
count++;

float2 dd=float2 (x,y);
if ( (x)==(y)) {
	//dd*=2.0;
}

float2 Dist=float2 (x,y)*0.0025*lgn2;
Dist=dd*0.0018*lgn2*xxx;

Dist+=MotionBlurPos;
MotionBlurPos+=MotionBlur.xy*0.01;

cl+=tex2D(texture2d_1,UV.xy+Dist);

}
}
cl/=float (count)*(lgn2+(1.0-lgn2)*0.85);
}

vec4 cll=cl*1.03;
cll+=gl_Color;
//cll=tex2D (texture2d_1,oUV.xy);



/*



 
//Depth blurring

float dpth=0.0;
 count=0;
 float Limit2=1.0-Limit;
for (int y=-4;y<5;y++) {
	for (int x=-4;x<5;x++){
count++;
float2 dd=float2 (x,y)*0.7*Limit2;

//dd=ray.xy*5.0;

float2 Dist=dd*0.0018;
dpth+=tex2D(texture2d_2,UV.xy+Dist).x;
}
}
dpth/=float (count);
//dpth=sin(dpth*3.14159);
//dpth+=0.1;
dpth=pow(dpth,126.0);
//dpth+=Limit;
dpth=1.0-1.0*dpth;
//if (dpth<0.7) dpth=0.0;
count=0;
cl=vec4(0.0);
dpth=clamp(dpth,0.0,1.0);

dpth=Limit+((1.0-Limit)-(Limit))*dpth;
dpth*=Flo;
//dpth=1.0;

float vel=tex2D(texture2d_velocities,UV.xy).x;
if (dpth>0.02)
{ 
for (int y=-4;y<5;y++) {
	for (int x=-4;x<5;x++){
count++;

float2 dd=float2 (x,y)*0.7;//;*vec2 (3.0,3.0);

//float2 dd2=float((x+4)*9+(y+4))*float2 (-1.0,1.0)*0.4;
//dd=dd+(dd2-dd)*-vel*0.4;


float2 Dist=dd*0.0018*dpth;
cl+=tex2D(texture2d_1,UV.xy+Dist);
//cl.x=dpth;
	}
}

cl/=float (count);
}
else
cl=tex2D(texture2d_1,UV.xy);



if (vel>0.01) { 
vec4 newcl=vec4 (0.0);
for (int x=0;x<15;x++){
float2 dd2=float(x)*float2 (-1.0,1.0)*0.003*vel;
newcl+=tex2D(texture2d_1,UV.xy+dd2);
}
newcl*=1.0/15.0;

cl=cl+(newcl-cl)*0.5;
}



//cl*=0.4;

//cl+=tex2D(texture2d_velocities,UV.xy)*0.5;


//cl=tex2D(texture2d_1,UV.xy);
//cl=vec4 (dpth);

//cl=vec4 (tex2D(texture2d_2,UV.xy).x);




*/

cl=cll;



gl_FragData[0]=cl;
	gl_FragData[1]=cl;

}














