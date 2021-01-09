varying vec3 dir;


float flr(vec3 p, float f)
{
	return abs(f - p.y);
}

float sph(vec3 p, vec4 spr)
{
	return length(spr.xyz - p) - spr.w;
}

float iso_tore(vec3 p, vec3 center, float r1, float r2) 
{
	p -= center;
	float xx = sqrt(p.x * p.x + p.z * p.z) - r1;
	float dist = sqrt(xx * xx + p.y * p.y) - r2;
	return mix(dist, dist, 0.9);
}

float scene(vec3 p)
{
	float d = iso_tore(p, vec3(0,0,15), 2.0, 0.5);
	return d;
}

vec3 getN(vec3 p)
{
	float eps = 0.01;
	return normalize(vec3(
		scene(p+vec3(eps,0,0))-scene(p-vec3(eps,0,0)),
		scene(p+vec3(0,eps,0))-scene(p-vec3(0,eps,0)),
		scene(p+vec3(0,0,eps))-scene(p-vec3(0,0,eps))
	));
}


void main()
{
	float g,d = 0.0;
	vec3 p = vec3(0);
	vec3 ndir = normalize(dir);
	
	for(int i = 0; i < 64; i++)
	{
		d = scene(p);
		p = p + d * ndir;
	}
	if(d > 1.0)
	{
		gl_FragColor = vec4(1.0,0.0,0.0,1.0);
	
		return;
	}
	
	vec3 n = getN(p);
	gl_FragColor = vec4(n, 1.0);
}
