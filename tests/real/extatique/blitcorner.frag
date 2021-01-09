uniform sampler2D tex; 
uniform float ratio;
uniform float factor;

void main()
{	
	vec2 p = gl_TexCoord[0].xy;
		
	vec2 dist = (p - vec2(0.5, 0.5)) * vec2(1.0,ratio); 
	vec4 darkF = vec4(max(0.0, 1.0 - factor * dot(dist,dist)));
	
	gl_FragColor = gl_Color * darkF *  texture2D(tex, p);
}
