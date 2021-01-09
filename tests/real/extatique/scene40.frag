uniform sampler2D tex;
varying vec3 wpos;
varying vec3 N;
uniform vec3 eyePos;
uniform float radius;
uniform float medium;
uniform vec3 center;
uniform vec3 light1;
uniform vec3 light2;
uniform vec3 light3;
uniform vec3 light4;


vec3 getEnvColor(vec3 pos, vec2 coord)
{
	vec3 color = texture2D(tex, coord * 0.05).xyz;
	
	vec3 light1_dist = pos * 0.1 - vec3(-0.5,-0.5,-0.5);
	vec3 light2_dist = pos * 0.1 - vec3(+0.5,+0.5,-0.5);
	vec3 light3_dist = pos * 0.1 - vec3(-0.5,+0.5,+0.5);
	vec3 light4_dist = pos * 0.1 - vec3(+0.5,-0.5,+0.5);
	
	vec3 clight1 = light1 * vec3(max(0.0, 1.25 - 1.0 * dot(light1_dist, light1_dist)));
	vec3 clight2 = light2 * vec3(max(0.0, 1.25 - 1.0 * dot(light2_dist, light2_dist)));
	vec3 clight3 = light3 * vec3(max(0.0, 1.25 - 1.0 * dot(light3_dist, light3_dist)));
	vec3 clight4 = light4 * vec3(max(0.0, 1.25 - 1.0 * dot(light4_dist, light4_dist)));
	
	
	return color + clight1 + clight2 + clight3 + clight4;
}

float exp3(float x)
{
	float y = max(-1.15365, x);
	return 1.0 + y * (1.0 + y * (0.5 + y * 0.33333333));
}

vec3 rayColor(vec3 startPos, vec3 startDir)
{
	vec3 total = vec3(0.0);
	vec3 p = startPos;
	vec3 dir = startDir;
	vec3 blend = vec3(0.5);
	vec3 dpos;
	vec3 color = vec3(0.0);
	
	vec3 ray;
	vec3 nml; 
	vec2 coord;
	vec3 newdir;
	vec3 sample;
	
	for (int i = 0; i < 1; ++i)
	{
		ray = 40.0 * dir;
		nml = vec3(0.0);
		coord = vec2(0.0);
		dpos = p + ray;
		
		if (abs(dpos.x) > 10.0)
		{
			if (dpos.x > 10.0)
			{
				ray *= ((9.99 - p.x) / ray.x);
				nml = vec3(-1.0,0.0,0.0);
				dpos = ray + p;
				coord = dpos.yz;
				color = getEnvColor(dpos, coord);				
			} 
			else 
			{
				ray *= ((-9.99 - p.x) / ray.x);
				nml = vec3(1.0,0.0,0.0);
				
				
				dpos = ray + p;
				coord = dpos.yz * vec2(-1.0,1.0);
				color = getEnvColor(dpos, coord);				
			}
		}
		
		if (abs(dpos.y) > 10.0)
		{
			if (dpos.y > 10.0)
			{
				ray *= ((9.99 - p.y) / ray.y);
				nml = vec3(0.0,-1.0,0.0);
				
				
				dpos = ray + p;
				coord = dpos.xz * vec2(-1.0,1.0);
				color = getEnvColor(dpos, coord);
			} 
			else 
			{
				ray *= ((-9.99 - p.y) / ray.y);
				nml = vec3(0.0,1.0,0.0);
				
				dpos = ray + p;
				coord = dpos.xz;
				color = getEnvColor(dpos, coord);
			}
		}
		
		if (abs(dpos.z) > 10.0)
		{
			if (dpos.z > 10.0)
			{
				ray *= ((9.99 - p.z) / ray.z);
				nml = vec3(0.0,0.0,-1.0);
				dpos = ray + p;
				coord = dpos.xy * vec2(-1.0,1.0);
				color = getEnvColor(dpos, coord);
			} 
			else
			{
				ray *= ((-9.99 - p.z) / ray.z);			
				nml = vec3(0.0,0.0,1.0);
				dpos = ray + p;
				coord = dpos.xy;
				color = getEnvColor(dpos, coord);
			}
		}
		
		sample = color * vec3(exp3(-length(ray) * 0.04));
		
		total += sample * blend;
		blend *= sample;
		
		newdir = reflect(dir, nml);
		
		p = dpos;
		dir = newdir;	
	}
	return total;
}



void snell(vec3 dir, vec3 normal, float n1, float n2, out vec3 reflectionDir, out vec3 refractionDir, out float cost2)
{
	float n1n2 = n1 / n2;
	float cost1 = dot(-dir, normal);
	cost2 = sqrt(1.0 - n1n2 * n1n2 * (1.0 - cost1 * cost1));
	
	reflectionDir = dir + normal * (2.0 * cost1);
	refractionDir = dir * n1n2 + normal * (cost2 + n1n2 * cost1);
}

void main()
{	
	vec3 nml = normalize(N);
	vec3 dir = normalize(wpos - eyePos);
	
	
	vec3 R;
	vec3 RE;
	float cost2, fdummy;
	
	
	snell(dir, nml, 1.0, medium + 0.1, R, RE, cost2);
	
	float dist = 2.0 * cost2 * radius;
	vec3 p2 = wpos + RE * vec3(dist);
	vec3 nml2 = -(p2 - center) / radius;
	vec3 Rdummy, Rout;
	snell(RE, nml2, medium + 0.3, 1.0, Rdummy, Rout, fdummy);
	float fact = 2.0 * exp3(-dot(dist,dist) * 0.2);
	vec3 diffracted = rayColor(p2, Rout) * vec3(fact);
	
	
	vec3 specular = rayColor(wpos, R) * gl_Color.xyz;	

	
	       
	gl_FragColor = gl_Color * vec4( specular + diffracted, 1.0);
}