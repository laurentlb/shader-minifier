
varying vec3 wpos;
varying vec3 N;
uniform vec3 eyePos;

void main()
{	
	wpos = gl_Vertex.xyz;
	gl_Position = ftransform();
	gl_FrontColor = gl_Color;
    N = gl_Normal;    
    
}