uniform vec3 light1Pos;
uniform vec3 light2Pos;
uniform vec3 light3Pos;
uniform mat4 invCamMat;

varying vec3 N;
varying vec3 pos;

varying vec3 l1_pos;
varying vec3 l2_pos;
varying vec3 l3_pos;

void main()
{	

	N = gl_NormalMatrix * gl_Normal;
	vec4 epos = gl_ModelViewMatrix * gl_Vertex;
	
	pos = epos.xyz;
	l1_pos = (gl_ModelViewMatrix * vec4(light1Pos,1.0)).xyz;
	l2_pos = (gl_ModelViewMatrix * vec4(light2Pos,1.0)).xyz;
	l3_pos = (gl_ModelViewMatrix * vec4(light3Pos,1.0)).xyz;
	
	gl_TexCoord[0] = gl_MultiTexCoord0;
	gl_FrontColor = gl_Color;
	gl_Position = ftransform();
}