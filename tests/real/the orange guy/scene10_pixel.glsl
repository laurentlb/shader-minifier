const int N_SPIRALES = 12; /* le nombre de points spirales */

uniform vec4 pos[N_SPIRALES]; /* position des points spirales, rayon, intensite */
uniform float musicVolume;

static const vec3 WHITE = vec3(1.0,1.0,1.0);

vec3 coul(vec2 p)
{
	float f = 0.0;
	for (int i = 0; i < N_SPIRALES; ++i)
	{
		vec2 diff = pos[i].xy - p;
		float dist = pow(dot(diff, diff), 1.0 / 4.0);
		float s = abs(dist - pos[i].z); 
		
		f = f + pos[i].w * exp(-s * (50.0 - musicVolume * 20.0));
	}
	
	return WHITE * max(0.0, 1.2 - f); 
}

void main()
{	
	vec2 p = gl_TexCoord[0].xy;
	
	vec3 couleur = coul(p);
	
	gl_FragColor = vec4( couleur * (1.0 + musicVolume), 1.0 );
}