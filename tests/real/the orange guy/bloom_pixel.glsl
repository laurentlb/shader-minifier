uniform sampler2D tex;
uniform float invTexHeight;


void main()
{	
	vec2 p = gl_TexCoord[0].xy;
	vec2 dp = vec2(0.0, invTexHeight);
	
	const vec3 limit = vec3(0.5,0.5,0.5);
	const vec3 mini = vec3(0.0,0.0,0.0);
	
	/* first version (7 samples) */
	/*
	vec3 a = max(mini, texture2D(tex, p - 3.0 * dp).rgb - limit);
	vec3 b = max(mini, texture2D(tex, p - 2.0 * dp).rgb - limit);
	vec3 c = max(mini, texture2D(tex, p - 1.0 * dp).rgb - limit);
	vec3 d = max(mini, texture2D(tex, p ).rgb - limit);
	vec3 e = max(mini, texture2D(tex, p + 1.0 * dp).rgb - limit);
	vec3 f = max(mini, texture2D(tex, p + 2.0 * dp).rgb - limit);
	vec3 g = max(mini, texture2D(tex, p + 3.0 * dp).rgb - limit);
	vec3 final = 0.015625 * (a + g)
	           + 0.09375  * (b + f)
	           + 0.234375 * (c + e)
	           + 0.3125   * d;
	
	gl_FragColor = vec4( final * 2.0, 1.0 );
	*/
	
	/* second version (4 samples) */
	
	vec3 a = max(mini, texture2D(tex, p - 2.1428571 * dp).rgb - limit);
	vec3 b = max(mini, texture2D(tex, p - 0.6 * dp).rgb - limit);
	vec3 c = max(mini, texture2D(tex, p + 0.6 * dp).rgb - limit);
	vec3 d = max(mini, texture2D(tex, p + 2.1428571 * dp).rgb - limit);
	vec3 final = (2.0 * 0.21875)  * (a + d)
	           + (2.0 * 0.78125)  * (b + c);	
	gl_FragColor = vec4( final, 1.0 );
	
}