uniform sampler2D tex;
uniform float amount;
uniform float colorSep;
uniform vec4 glow;


void main()
{	
	vec2 p = (gl_TexCoord[0].xy - vec2(0.5,0.5));	
	vec2 p2 = p + vec2(colorSep, 0.0);
	vec2 p3 = p - vec2(colorSep, 0.0);
	
	float distortion = amount / (1.0 + length(p * 5.0));
	
	float cosd = cos(distortion);
	float sind = sin(distortion);
	
	mat2 tranfo = mat2(cosd, -sind, sind, cosd);
	
	vec4 color1 = vec4(1.0,0.5,0.0,0.5) * texture2D(tex,  (vec2(0.5) + tranfo * p2));
	vec4 color2 = vec4(0.0,0.5,1.0,0.5) * texture2D(tex,  (vec2(0.5) + tranfo * p3));
	gl_FragColor = gl_Color * (color1 + color2) + glow;
}