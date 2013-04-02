uniform sampler2D tex;
uniform float invGamma;

const vec3 LUMINANCECONV = vec3(0.11,0.6,0.29);
void main()
{	
	vec2 p = gl_TexCoord[0].xy;	
	
	
	
	vec3 sample = texture2D(tex, p).xyz;
	
	
	sample = log2(sample + vec3(1.0));
	float luminance = dot(LUMINANCECONV, sample);
	
	vec3 sampleC = sample  * vec3(pow(luminance,invGamma)); 
	
	gl_FragColor = gl_Color * vec4(sampleC,1.0);
}