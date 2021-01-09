uniform sampler2D tex;

void main()
{	
	vec2 p = gl_TexCoord[0].xy;	
	gl_FragColor = gl_Color * texture2D(tex, p);
}