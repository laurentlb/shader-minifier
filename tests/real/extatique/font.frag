uniform sampler2D tex;
uniform sampler2D fill;

void main()
{	
	vec2 p = gl_TexCoord[0].xy;
	vec4 letter = texture2D(tex, p);
	vec4 fillColor = texture2D(fill, p * 3.0);

	gl_FragColor = letter * fillColor * gl_Color;
}