uniform sampler2D tex;
uniform float angle;
uniform float time;
varying vec3 pos;



vec4 getEnvColor(vec3 p)
{
	float az = atan(p.x, p.y + 0.01);
	float ax = -pos.z;
	
	vec4 sample = vec4(0.0);
	
	for (int i = 0; i < 8; ++i)
	{
		float fi = float(i);
		float s = ax * 1.0 / (1.0 + fi) + time * 0.01 * (2.0 + fi);
		float t = angle + (2.5) * az / 3.14159;
		
		sample += texture2D(tex, vec2(s,t)) / (1.0 + 0.3 * fi);
	}
	return sample;
}



void main()
{	
	vec4 env = getEnvColor(pos);
	gl_FragColor = vec4(env.xyz, 1.0);
}