uniform sampler2D tex;

void main()
{	
	vec2 p = gl_TexCoord[0].xy;
	vec4 sample0 = texture2D(tex, p);
	gl_FragColor = gl_Color * sample0;
}