#version 110
#define float4 vec4
#define float3 vec3
#define float2 vec2
#define oUV gl_TexCoord[0]
#define tex3D texture3D
#define tex2D texture2D
#define tex1D texture1D

varying vec3 normal;
varying vec4 position;
varying vec4 thecolor;

varying vec2 texture_coordinate_1;



void main (void) 
{

	
	float4 TheNormal=float4 (1.0,0.0,0.0,0.0);
//TheNormal.w=shadow;
gl_FragData[0]=thecolor;
}














