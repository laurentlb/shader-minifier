uniform sampler2D tex1;
uniform sampler2D tex2;

uniform float tex1Amount;
uniform float tex2Amount;

void main()
{	
	vec2 p = gl_TexCoord[0].xy;	
	
	gl_FragColor = vec4(tex1Amount) * texture2D(tex1, p) + vec4(tex2Amount) * texture2D(tex2, p); 
}