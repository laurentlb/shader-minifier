varying vec2 st;
varying float prof;


void main()
{	
	st = gl_MultiTexCoord0.xy;
	
	vec4 p = ftransform();
	prof = gl_Color.x * 50.0;
	
	gl_Position = p;
}