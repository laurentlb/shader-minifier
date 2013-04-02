uniform sampler2D tex;
uniform vec2 size;
uniform float border;


void main()
{	
	vec2 p = gl_TexCoord[0].xy;	
	
	vec2 realP = p * size;
	
	float alphaX = smoothstep(0.0, border, realP.x) - smoothstep(size.x - border, size.x, realP.x);
	float alphaY = smoothstep(0.0, border, realP.y) - smoothstep(size.y - border, size.y, realP.y);
	
	float alpha = alphaX * alphaY;
	
	gl_FragColor =  vec4(gl_Color.rgb, alpha) * texture2D(tex, p);
}