varying vec3 dir;

void main()
{
	vec4 pos = ftransform();
	gl_Position = pos;	
	dir = pos.xyz;
	
}
