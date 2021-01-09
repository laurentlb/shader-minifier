varying vec3 normal;
varying float profondeur;

vec4 desaturate(vec4 color, float t)
{
	vec3 grey = vec3(dot(vec3(0.33), color.rgb));
	return vec4(mix(color.rgb, grey, t), color.a);
}

void main()
{	
	vec3 N = normal;
	
	vec3 e = vec3(0.0,0.0,-1.0);
	float diffuse = max(0.0, dot(N, e));
	vec4 C = gl_Color * vec4(0.1 + 0.9 * vec3(diffuse), diffuse);  
	gl_FragColor = C * exp( -profondeur * 0.1  );
}