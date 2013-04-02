uniform sampler2D tex;

uniform float intensity;
uniform float alpha;

void main()
{
	vec4 color = texture2D(tex, gl_TexCoord[0].xy);	
	float opacity = dot(color.rgb, vec3(0.33,0.33,0.33)) * color.a * alpha;
	
	
	if (opacity < 0.05) discard; 
		
	gl_FragData[0] = vec4(color.rgb * intensity, opacity);	
}