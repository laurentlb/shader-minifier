uniform sampler2D tex;
uniform sampler2D noise;
uniform vec2 size;
uniform float border;


void main()
{	
	vec2 p = gl_TexCoord[0].xy;	
	
	vec2 realP = p * size;
	
	float alphaX = smoothstep(0.0, border, realP.x) - smoothstep(size.x - border, size.x, realP.x);
	float alphaY = smoothstep(0.0, border, realP.y) - smoothstep(size.y - border, size.y, realP.y);
	
	float border2 = border * 3.0;
	float alphaX2 = smoothstep(0.0, border2, realP.x) - smoothstep(size.x - border2, size.x, realP.x);
	float alphaY2 = smoothstep(0.0, border2, realP.y) - smoothstep(size.y - border2, size.y, realP.y);
	
	
	float noise = texture2D(noise, realP).r;
	
	float alpha = max(0.0, alphaX * alphaY - (noise * (1.0 - alphaX2 * alphaY2)));
	
	gl_FragColor =  vec4(gl_Color.rgb, alpha) * texture2D(tex, p);
}