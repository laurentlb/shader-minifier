uniform sampler2D tex;
uniform sampler2D grain;

const int NPOINTS = 5;

uniform float env;

uniform vec3 pos[NPOINTS];
uniform float intensity[NPOINTS];
uniform float localTime;
uniform float zoom;


const float PI = 3.14159265;

void main()
{
	float x = gl_TexCoord[0].s * zoom;
	float y = gl_TexCoord[0].t * zoom;
	float u = localTime * 0.25; 
	
	vec4 color = vec4(0.0);
	
	for (int i = 0; i < 5; i++)
	{
		vec3 diff = vec3(x, y, 0.0) - pos[i];
		float angle = atan(diff.y, diff.x);
		float d = env * 1.0; /* length(pos[i]);*/
		float t = (d + localTime) * 0.03 ;
		float s = u + angle / PI + (float(i) * 0.1);
		float dist = length(diff);
		vec4 grain = texture2D(grain, vec2(0.5, 0.3 * dist));		
		
		color += (texture2D(tex, vec2(s,t)) * grain) * (intensity[i] / (1.0 + 0.2 * dist));	
	}
	
	gl_FragColor = vec4(color.rgb * 0.6 + 0.4 * env, 1.0); 
}
