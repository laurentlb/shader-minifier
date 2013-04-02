#version 110
#define float4 vec4
#define float3 vec3
#define float2 vec2
#define oUV gl_TexCoord[0]
#define tex3D texture3D
#define tex2D texture2D
#define tex1D texture1D

				uniform sampler2D texture2d_1;
				uniform sampler2D texture2d_s;
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
uniform float ttime;
uniform float fttime;
uniform float DSP;
uniform float DSP2;





void main (void) 
{




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
//Direction*=0.01;
//Direction=vec3 (0.0,0.01,0.0);
if (alpha>0.01)
{
	for (int t=0;t<10;t++) { 
UV.xy-=Direction.xy*0.01;
	cl+=tex2D (texture2d_1,UV.xy);
};

	cl*=1.0/10.0;
//Direction=tex2D (texture2d_velocities,UV.xy).xyz;
//cl=Direction.xyzz*1.0+vec4 (0.5);
//cl.x+=0.15*alpha;
//=float4 (1.0,0.0,0.0,1.0)*alpha;
}
else cl=tex2D (texture2d_1,UV.xy);

//float4 clnow=tex2D( texture2d_1,oUV.xy);
//cl=cl+(clnow-cl)*0.5;

UV=oUV.xy;
if (fttime>186.0) if (fttime<242.0) if (UV.x>0.5) UV.x=1.0-UV.x;

float fx=tex2D (texture2d_1,UV.xy).x;


if (ttime>169.0)  
if (ttime<184.0){

	if (UV.y>0.5) UV.y=1.0-UV.y;
}

if (ttime<197.0) 
if (ttime>184.0){
if (UV.x>0.5) UV.x=1.0-UV.x;
}




if (ttime<86.0)
if (ttime>26.5)
if (UV.x>0.5) UV.x=1.0-UV.x;





if (ttime>98.0)
if (ttime<106.0) { 

float fft=((UV.x-0.5) / (UV.y-0.5));
fft=1.0*atan (fft);

//fft=mod (fft,1.4);


float dist=length (UV.xy-vec2 (0.5));

vec2 UVn;
UVn.x=dist * sin(fft)+0.5;
UVn.y=dist * cos(fft)+0.5;

UV=UVn;
}









//1.0-UV.x;


//if (UV.y>0.5) UV.y=1.0-UV.y;
//if (UV.x>fx) UV.x=1.0-UV.x;
//+(fx-0.5);
cl=1.0*tex2D (texture2d_1,UV.xy);
cl+=1.0*tex2D (texture2d_s,UV.xy);

vec4 cln=cl;

if ((ttime>86.0) && (ttime<104.0)){ 


float cll2=cl.y;
//	if (UV.y>0.8) UV.y=1.0-UV.y+0.6;

cl=1.0*tex2D (texture2d_1,UV.xy);
cl+=1.0*tex2D (texture2d_s,UV.xy);

vec4 cln=vec4(tex2D(texture2d_1,UV.xy).x)+vec4(tex2D(texture2d_s,UV.xy).x);
if (cll2>0.9)  cl*=100.0;
//vec4 (1.7,0.8,1.0,0.0);
float fl=(ttime-92.0)*0.125;
fl=clamp (fl,0.0,1.0);
cl=cl+(cln-cl)*fl;

}






if (ttime>104.0) { 


float cll2=cl.y;
float cll3=cl.z;

float fl2=(ttime-123.0)*0.25;

fl2=clamp (fl2,0.0,1.0);
fl2=0.0;
float ff=UV.x;

if (fttime<186.0)
if (UV.x>0.5+fl2) UV.x=1.0-UV.x;

//UV.x=UV.x+(ff-UV.x)*fl2;




cl=vec4(tex2D(texture2d_1,UV.xy).x)+vec4(tex2D(texture2d_s,UV.xy).x);
if (cll2>0.9) 
cl*=1.7*vec4 (0.7,0.8,1.0,0.0);

fl2=(ttime-131.0)*0.3;

if (fttime>186.0);
fl2=(ttime-129.0)*0.25;

fl2=clamp (fl2,0.0,1.0);
cl=cl+(cln-cl)*fl2;
//if (cll3>0.6) cl*=1.7*vec4 (0.7,0.9,1.0,0.0);
}




cl+=vec4(DSP*DSP*oUV.y)*0.25;


float flx=1.9;
if (ttime>140.0) flx=1.2;


if (cl.z>flx)
{
float ures=pow(sin(UV.x*5.0),5.0);
if (ures>0.5) ures=-1.0; else ures=1.0;
	float uu=pow(1.1*sin(5.0*ttime+2000.0*UV.x)*sin(2000.0*UV.y),8.0);
uu*=pow(1.1*sin(ures*-15.0*ttime+200.0*UV.x)*sin(ures*-10.0*ttime+200.0*UV.y),8.0);

if (uu>0.0)
cl*=0.0;


}




if (ttime<250.0)
if (length (cl)<0.1) { 
float ures=pow(sin(UV.x*15.0),5.0);
if (ures>0.5) ures=-1.0; else ures=1.0;
	float uu=pow(1.1*sin(5.0*ttime+2000.0*UV.x)*sin(2000.0*UV.y),8.0);
uu*=pow(1.1*sin(ures*-5.0*ttime+400.0*UV.x)*sin(ures*-10.0*ttime+400.0*UV.y),8.0);


float fl2=(ttime-32.0)*0.125;
fl2=clamp (fl2,0.0,1.0);

if (uu>0.04)
cl+=fl2*vec4 (0.43)*vec4 (0.7,1.0,1.0,1.0)*pow((1.0-oUV.y),2.0);


}

/*
if (ttime>230.0) 
if (sin(oUV.y*100.0)>0.0)
cl=vec4 (cl.x);
*/


float ffade=ttime-230.0;
ffade=clamp (ffade,0.0,1.0);
if (fttime>230.0)
cl*=ffade;





//cl*=0.5+DSP2;


float fls=
min (length (vec2 (2.0,0.5)*(oUV.xy-vec2 (0.5,0.4))),
length (vec2 (0.5,2.0)*(oUV.xy-vec2 (0.5,0.4))));
fls=1.0-fls;
fls=clamp (fls,0.0,1.0);
fls=pow (fls,2.0);
//float fls2=length (oUV.xy-vec2 (0.5));

fls=
length (vec2 (2.0,0.5)*(oUV.xy-vec2 (0.5,0.4)));
fls=1.0-fls;
fls=clamp (fls,0.0,1.0);
fls=pow (fls,3.0);
//fls*=clamp (10.0*sin(oUV.y*1200.0),0.0,1.0);


if (fttime<130.0)
cl*=1.0+2.0*DSP2*fls*vec4 (0.5,0.7,1.0,1.0);

if (fttime>190.0)
cl*=1.0+2.0*DSP2*fls*vec4 (0.5,0.7,1.0,1.0);



//fls=(oUV.x-0.5)+(oUV.y-0.5);
//fls=1.0-fls;
//cl*=1.0+1.5*vec4 (0.6,0.8,0.9,1.0)*5.0*vec4 (pow(fls,8.0));
//cl=tex2D (texture2d_1,vec2 (sin(oUV.x),sin(oUV.y)));


vec4 clnn=vec4 (pow (cl.x*1.1,3.0),pow (cl.y*1.1,3.0),pow (cl.z*1.1,3.0),pow (cl.z*1.1,2.0));



if (ttime>86.0)
cl=vec4 (cl.x)+(clnn-vec4 (cl.x))*0.3;
else
cl=vec4 (cl.y)+(0.8*clnn-vec4 (cl.y))*0.25;








//cl=clnn;




//*vec4 (0.2,0.5,1.0,1.0);


//cl=vec4(tex2D(texture2d_1,UV.xy).x)+vec4(tex2D(texture2d_s,UV.xy).x);

/*


// blur effect
float lgn2=length (float2 (1.0,2.0)*(UV.xy-float2 (0.5)));
lgn2*=lgn2;
//pow(lgn2,1.8);
int count=0;
for (int y=-5;y<6;y++) {
	for (int x=-5;x<6;x++){
count++;

float2 dd=float2 (x,y);
if ( (x)==(y)) {
	dd*=2.0;
}

float2 Dist=float2 (x,y)*0.0025*lgn2;
Dist=dd*0.0025*lgn2;
cl+=tex2D(texture2d_1,UV.xy+Dist);

}
}
cl/=float (count)*(lgn2+(1.0-lgn2)*0.85);


vec4 cll=cl;

cl=tex2D (texture2d_1,oUV.xy);







 
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
*/

//cl=vec4 (tex2D(texture2d_1,UV.xy));
//cl=vec4 (tex2D(texture2d_velocities,UV.xy));





//cl=cll;



gl_FragData[0]=cl;
	gl_FragData[1]=cl;

}














