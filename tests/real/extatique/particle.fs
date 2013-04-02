uniform sampler2D tex;
uniform float intensity;

void main()
{	
	vec2 p = gl_TexCoord[0].xy;	
	gl_FragColor = vec4(vec3(intensity), 1.0) * gl_Color * texture2D(tex, p);
}