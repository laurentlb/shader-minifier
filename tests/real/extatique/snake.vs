varying vec3 normal;
varying float profondeur;

void main()
{	
	gl_FrontColor = gl_Color;
	
	normal = normalize(gl_Normal);
	gl_Position = ftransform();
	
	profondeur = gl_Position.z;
}