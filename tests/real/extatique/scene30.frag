uniform sampler2D tex;
uniform float angle;
uniform float time;
varying vec3 pos;
varying vec3 N;


vec4 getEnvColor(vec3 p)
{
	float az = atan(p.x, p.y + 0.01);
	float ax = -pos.z;
	
	vec4 sample = vec4(0.0);
	
	for (int i = 0; i < 8; ++i)
	{
		float fi = float(i);
		float s = ax * 2.0 / (1.0 + fi) + time * 0.01 * (2.0 + fi);
		float t = angle + (2.5) * az / 3.14159;
		
		sample += texture2D(tex, vec2(s,t)) / (1.0 + 0.3 * fi);
	}
	return sample;
}

vec3 diffuseLighting(vec3 pos, vec3 nml)
{
	float df1 = 0.8 * max(0.0, dot(nml, vec3(1.0,0.2, 0.0)));
	float df2 = 0.2 * max(0.0, dot(nml, vec3(-1.0,-0.1, 0.0)));
	vec3 diffuse = vec3(df1) * vec3(0.8,0.7,0.6) + vec3(df2) * vec3(0.6,0.7,0.8);
	return diffuse;
}

void main()
{	
	vec3 nml = normalize(N);
	vec3 R = reflect(-pos, normalize(nml));
	vec3 envColor = getEnvColor(R).xyz * mix(vec3(1.0), gl_Color.xyz, 0.4);
	vec3 diffuseColor = getEnvColor(pos).xyz * gl_Color.xyz;	
	gl_FragColor = vec4( diffuseColor + envColor, 1.0);
}