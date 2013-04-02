#define float4 vec4
#define float3 vec3
#define float2 vec2




varying vec3 normal;
varying vec2 texture_coordinate_1; 
varying vec4 position;
varying vec4 thecolor;

uniform mat4 ProjectionMatrix;


void main()
{
mat4 Billboard;

vec4 glVertex=gl_Vertex;

vec4 Next_Position;
	
//gl_Position = gl_TextureMatrix[0] * gl_Vertex;


 float4 MotionBlurPos_current=gl_TextureMatrix[0]*gl_Vertex;
 float4 MotionBlurPos_previous=gl_TextureMatrix[1]*gl_Vertex;



float3 MotionBlurmotionVector=MotionBlurPos_current.xyz-MotionBlurPos_previous.xyz;


float4 MotionBlurNormalDirection=float4 (0.0,0.0,0.0,0.0)-gl_Vertex.xyzz;
MotionBlurNormalDirection= -gl_Normal.xyzz*1.0;

  ///MotionBlurNormalDirection=gl_TextureMatrix[0]*MotionBlurNormalDirection;
 // normalize (MotionBlurNormalDirection);



float MotionBlurflag=dot (MotionBlurmotionVector.xyz,MotionBlurNormalDirection.xyz);
float4 MotionBlurstretch;

float smm=0.0;
if (MotionBlurflag>-0.0)
{
	MotionBlurstretch = MotionBlurPos_current+(MotionBlurPos_previous-MotionBlurPos_current)*1.0; 
smm=1.0;
}
else
{
smm=0.3;
	MotionBlurstretch= MotionBlurPos_current;
}



	//MotionBlurstretch = MotionBlurPos_current+(MotionBlurPos_previous-MotionBlurPos_current)*1.0; 




//MotionBlurPos_current.xyz=MotionBlurPos_current.xyz/MotionBlurPos_current.w;
//MotionBlurstretch.xyz=MotionBlurstretch.xyz/MotionBlurstretch.w;
//MotionBlurstretch=MotionBlurPos_previous;
gl_Position = gl_ModelViewProjectionMatrix* MotionBlurstretch;

float3 MotionBlur_dP=MotionBlurstretch.xyz-MotionBlurPos_current.xyz;

float4 Mot=MotionBlur_dP.xyzz;
Mot=gl_ModelViewProjectionMatrix*Mot;
MotionBlur_dP.xyz=Mot.xyz;

MotionBlur_dP+=float3 (1.0);
MotionBlur_dP*=0.5;


thecolor =MotionBlur_dP.xyzz;
//thecolor =MotionBlurNormalDirection.xyzz;
//thecolor=float4 (smm);
//thecolor=(MotionBlurPos_previous-MotionBlurPos_current).xyzz+float4 (0.5,0.5,0.5,0.5);
//thecolor=gl_TextureMatrix[0]*gl_Normal.xyzz;
//gl_Position= gl_ModelViewProjectionMatrix*gl_TextureMatrix[0]*gl_Vertex;

//thecolor=MotionBlurstretch.xyzz;


// Transforming The Normal To ModelView-Space
//	normal = gl_NormalMatrix * gl_Normal;
	//normal = gl_Normal; 
//texture_coordinate_1 = vec2(gl_MultiTexCoord0);

//position=(glVertex).xyzw;
//ShadowPosZ= ProjectionMatrix * gl_TextureMatrix[0]*gl_Vertex;
//position=g
// Transforming The Vertex Position To ModelView-Space
//	vec4 vertex_in_modelview_space = gl_ModelViewMatrx * gl_Vertex;

	// Calculating The Vector From The Vertex Position To The Light Position
//	vertex_to_light_vector = vec3(gl_LightSource[0].position – vertex_in_modelview_space);
}











