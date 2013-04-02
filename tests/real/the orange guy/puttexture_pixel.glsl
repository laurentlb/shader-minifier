uniform sampler2D tex0;
uniform sampler2D tex1;
uniform sampler2D tex2;
uniform sampler2D tex3;

uniform float amountTex0;
uniform float amountTex1;
uniform float amountTex2;
uniform float amountTex3;

void main()
{	
	vec2 p = gl_TexCoord[0].xy;
	vec4 sample0 = texture2D(tex0, p);
	vec4 sample1 = texture2D(tex1, p);
	vec4 sample2 = texture2D(tex2, p);
	vec4 sample3 = texture2D(tex3, p);
	gl_FragColor = gl_Color * (amountTex0 * sample0 + sample1 * amountTex1 
	                         + amountTex2 * sample2 + sample3 * amountTex3 );
}