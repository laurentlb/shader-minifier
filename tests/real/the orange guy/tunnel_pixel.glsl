
varying vec2 st;
uniform float alpha;
uniform sampler2D tex;
uniform float time;
uniform vec4 color;
varying float prof;

void main()
{
	
	vec3 texcolor = texture2D(tex, st).rgb;
	
	float opacity = alpha < 0.9 ? 1.0 : 1.0 - 0.7 * dot(vec3(0.33,0.33,0.33), texcolor);
	
	opacity *= exp(-0.10 * abs(prof));
	
	gl_FragData[0] = color * vec4(texcolor * 1.6,  opacity);	
	
}