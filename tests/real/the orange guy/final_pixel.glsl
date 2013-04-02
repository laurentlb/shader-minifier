uniform sampler2D tonemappedTexture;
uniform sampler2D bloomTexture;
uniform sampler2D paperTexture;
uniform int paper;
uniform float paperInvSize;


void main()
{	
	vec2 p = gl_TexCoord[0].xy;
	
	vec4 main = texture2D(tonemappedTexture, p);
	vec4 bloom = texture2D(bloomTexture, p);
	
	vec4 paper = float(paper) * 1.8 *  texture2D(paperTexture, gl_FragCoord.xy * paperInvSize) + vec4(1.0,1.0,1.0,1.0) * (1.0 - float(paper));
	
	gl_FragColor = paper * (main * 0.8 + 0.2 * bloom); 	
}