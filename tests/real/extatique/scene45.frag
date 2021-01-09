uniform sampler2D tex;
uniform float time;


void main()
{	
	vec2 p = gl_TexCoord[0].xy;	
	float u = atan(p.y, p.x + 0.0001) * 1.0 / 3.1415926;
	float v = 0.002 / (0.01 + length(p)) - time * 0.1;
	vec4 space = texture2D(tex, vec2(u,v)) 
	           + vec4(0.5) * texture2D(tex, vec2(u,v * 4.0)) 
	           + vec4(0.25) * texture2D(tex, vec2(u,v * 16.0)); 
	gl_FragColor = gl_Color * space;
}