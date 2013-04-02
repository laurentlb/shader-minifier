varying vec3 pos;
varying vec3 N;

void main()
{	
	pos = (gl_ModelViewMatrix * gl_Vertex).xyz;
	gl_Position = ftransform();
	gl_FrontColor = gl_Color;
    N = gl_NormalMatrix * gl_Normal;    
    
}