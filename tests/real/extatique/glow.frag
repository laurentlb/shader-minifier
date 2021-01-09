uniform sampler1D tex;


void main()
{	
	vec2 p = gl_TexCoord[0].xy;	
	float a = length(p) / 2.04;
	gl_FragColor = gl_Color * vec4(1.0,1.0,1.0, texture1D(tex, a));
}