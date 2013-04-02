uniform float minDist;
uniform float maxDist;
uniform float minSize;
uniform float maxSize;

void main()
{	
	gl_TexCoord[0] = gl_MultiTexCoord0;
	
	vec4 pos = ftransform();
	gl_Position = pos;
	
	float f = (pos.z - minDist) / (maxDist - minDist);
	
	gl_FrontColor = gl_Color * vec4(vec3(1), f);
	gl_PointSize = minSize + maxSize * f;
}