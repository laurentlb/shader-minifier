varying vec3 pos;

void main()
{	
	pos = (gl_ModelViewMatrix * gl_Vertex).xyz;
	gl_Position = ftransform();
}