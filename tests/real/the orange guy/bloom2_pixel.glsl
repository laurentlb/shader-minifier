uniform sampler2D tex;
uniform float invTexWidth;


void main()
{	
	vec2 p = gl_TexCoord[0].xy;
	vec2 dp = vec2(invTexWidth, 0.0);
	
	/*
	vec3 a = texture2D(tex, p - 3.0 * dp).rgb;
	vec3 b = texture2D(tex, p - 2.0 * dp).rgb;
	vec3 c = texture2D(tex, p - 1.0 * dp).rgb;
	vec3 d = texture2D(tex, p ).rgb;
	vec3 e = texture2D(tex, p + 1.0 * dp).rgb;
	vec3 f = texture2D(tex, p + 2.0 * dp).rgb;
	vec3 g = texture2D(tex, p + 3.0 * dp).rgb;
	vec3 final = 0.015625 * (a + g)
	           + 0.09375  * (b + f)
	           + 0.234375 * (c + e)
	           + 0.3125   * d;
	
	gl_FragColor = vec4( final, 1.0 );
	*/
	
	
	vec3 a = texture2D(tex, p - 2.1428571 * dp).rgb;
	vec3 b = texture2D(tex, p - 0.6 * dp).rgb;
	vec3 c = texture2D(tex, p + 0.6 * dp).rgb;
	vec3 d = texture2D(tex, p + 2.1428571 * dp).rgb;
	vec3 final = (2.0 * 0.21875)  * (a + d)
	           + (2.0 * 0.78125)  * (b + c);	
	gl_FragColor = vec4( final, 1.0 );
	
}