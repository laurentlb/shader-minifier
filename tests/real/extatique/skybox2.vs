varying vec3 wpos;

uniform vec3 eyePos;

void main()
{	
	wpos = gl_Vertex.xyz;
	/* pos = (gl_ModelViewMatrix * gl_Vertex).xyz; */
	gl_FrontColor = gl_Color;
	/* gl_TexCoord[0] = gl_MultiTexCoord0; */
	
	gl_Position = ftransform();
}