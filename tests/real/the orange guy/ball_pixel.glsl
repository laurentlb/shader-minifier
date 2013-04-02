const int N_BALLS = 8; /* le nombre de points spirales */

uniform vec4 col[N_BALLS]; /* couleurs des points spirales */
uniform vec3 pos[N_BALLS]; /* position des points spirales */
uniform float intensity[N_BALLS]; /* "taille" des points spirales */


uniform float localTime;
uniform float progression;

/* renvoie la couleur de l'image en ce point.
   On l'appelle plusieurs fois pour faire des symétries (bourrin !) */
vec3 coul(vec2 p)
{
	float prog = progression;
	float p2 = prog * prog;
	float time = localTime;
		
	vec3 couleur = vec3(0.0,0.0,0.0); 
	
	
	float l = min(0.0, 1.5 * progression - 0.5);
	
	float dist_factor = (6.0 + 3.0 * sin(time));

	
	for (int i = 0; i < N_BALLS; ++i)
	{
		vec2 diff = (pos[i].xy - p);
		float d = dot(diff, diff);  /* d = distance entre le pixel et le point spirale i */
		
		float angle = atan(diff.y, diff.x);
		
		/* on accumule la contribution de chaque spirale */		
		
		float s = cos(5.0 * (angle - (1.0 + float(i) * 0.05 - prog * 1.5 ) * time) + d * dist_factor ); 
		
		s *= min(1.0, abs(0.7 + prog * 0.3 - s) * 15.0);
		
		float spiral_factor = max(0.0, s  * (1.0 - p2) );
		float contrib = spiral_factor * intensity[i] * exp(-0.02 * d) + l;
		
		 couleur += col[i].rgb * contrib; 
		
	}
	return couleur;
}

void main()
{	
	vec2 p = gl_TexCoord[0].xy * 0.7;
	
	vec3 couleur = coul(p);
	
	gl_FragColor = vec4( couleur * 1.1, 1.0 );
}