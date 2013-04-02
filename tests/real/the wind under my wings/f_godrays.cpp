#version 110
#define float4 vec4
#define float3 vec3
#define float2 vec2
#define oUV gl_TexCoord[0]
#define tex3D texture3D
#define tex2D texture2D
#define tex1D texture1D

				uniform sampler2D texture2d_1;
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







void main (void) 
{




float v=0.102;

float dss=1.0-pow(length(oUV.xy-float2 (0.5,0.5)),1.0);
float2 dirr=float2 (0.5,0.5);
//dss=1;
//dss*=1.5;

v*=dss*dss;
//v=0.002;
float2 MX=float2 (v,0.0);
float2 MU=float2 (0.0,v);
dirr.y*=1.0;
 vec4 cl=float4 (0.0);
/*
for (int t=0;t<7;t++) { 
float ff=float (t)/(20.0f);
//    cl+=0.11*(1-ff)*tex2D (texture2d_1,ff*dirr+(1-ff)*oUV.st);
cl+=0.19*(1-ff)*tex2D (texture2d_1,ff*dirr+(1-ff)*oUV.st);

}

0.05,0.1,0.15,0.2,0.25,0.3
*/
//float cl=0.3;
float4 sti=tex2D (texture2d_1,oUV.st);
cl+=0.191*tex2D (texture2d_1,oUV.st);

cl+=0.191*(0.95)*tex2D (texture2d_1,0.93*0.075*dirr+(1.0-0.93*0.075)*oUV.st);
cl+=0.191*(0.9)*tex2D (texture2d_1,0.93*0.15*dirr+(1.0-0.93*0.015)*oUV.st);
cl+=0.191*(0.85)*tex2D (texture2d_1,0.93*0.225*dirr+(1.0-0.93*0.225)*oUV.st);

cl+=0.191*(0.8)*tex2D (texture2d_1,0.93*0.300*dirr+(1.0-0.93*0.300)*oUV.st);
cl+=0.191*(0.75)*tex2D (texture2d_1,0.93*0.375*dirr+(1.0-0.93*0.375)*oUV.st);
cl+=0.191*(0.7)*tex2D (texture2d_1,0.93*0.450*dirr+(1.0-0.93*0.450)*oUV.st);

cl=tex2D(texture2d_1,oUV.st)+cl*0.7*float4 (0.4,0.6,1.0,0.0);
//cl*=dss*sti;











//float4 cl;
//cl=tex2D (texture2d_1,oUV.xy)*1.0*vec4 (0.0,0.0,1.0,0.0);
cl=vec4 (0.0);
int count=0;
float2 DirV=oUV.xy-float2 (0.5,0.5);
//DirV=-normalize (DirV);
DirV*=-1.0;
	for (int x=0;x<18;x++){
count++;

float2 Dist=DirV*float(x)*0.05;
float4 res=tex2D(texture2d_1,oUV.xy+Dist);
res*=res*1.5;
//res*=1.0+0.5*sin(oUV.x*12.0);
//res*=1.0+0.7*sin(oUV.y*24.0);
cl+=res;
}

cl/=float (count);
cl=tex2D(texture2d_1,oUV.st)*0.9+cl*3.5;
//*float4 (0.4,0.6,1.0,0.0);
//cl*=float4 (1.0,1.0,1.0,1.0);



/* 
//Depth blurring

float dpth=0;
int count=0;
for (int y=-4;y<5;y++) {
	for (int x=-4;x<5;x++){
count++;
float2 dd=float2 (x,y)*0.7;

//dd=ray.xy*5.0;

float2 Dist=dd*0.0015;
dpth+=tex2D(texture2d_2,UV.xy+Dist).x;
}
}
dpth/=float (count);
//dpth=sin(dpth*3.14159);
dpth+=0.1;
dpth=pow(dpth,6.0);
//dpth+=Limit;
dpth=1.1-dpth;
//if (dpth<0.7) dpth=0.0;
count=0;
cl=vec4(0.0);
dpth=clamp(dpth,0.0,1.0);

dpth=Limit+((1.0-Limit)-(Limit))*dpth;

if (dpth>0.02) { 
for (int y=-4;y<5;y++) {
	for (int x=-4;x<5;x++){
count++;

float2 dd=float2 (x,y);

//if ( (x)==(y)) {	dd*=120.0;}

float2 Dist=dd*0.0015*dpth;
cl+=tex2D(texture2d_1,UV.xy+Dist);
}
}

cl/=float (count);
}
else
cl=tex2D(texture2d_1,UV.xy);



*/









gl_FragData[0]=cl;
	gl_FragData[1]=cl;

/*
float3 currentuv;



if (Projection_mode==0)  {
currentuv.xy=oUV.st;
currentuv.z=0.0+InVolumePosition.z;
}
else
if (Projection_mode==1)  {
currentuv.xz=oUV.st;
currentuv.y=0.0+InVolumePosition.y;
}
else
if (Projection_mode==2)  {
currentuv.zy=oUV.ts;
currentuv.x=0.0+InVolumePosition.x;
}
float4 original= tex3D (texture3d,currentuv);
float divisor=1.0/(Windowing_Levels.y-Windowing_Levels.x);




original-=Windowing_Levels.x*float4 (1,1,1,1);
original*=divisor;


//original.x=max (0.0,min (1.0,original.x));


gl_FragColor=currentuv.xyzz;
//gl_FragColor=float4 (1.0,0.0,0.0,1.0);
//gl_FragColor*=Sharpening;
*/
}














