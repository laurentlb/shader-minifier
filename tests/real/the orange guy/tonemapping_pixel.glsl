uniform sampler2D tex;

void main()
{	
	vec3 color = texture2D(tex, gl_TexCoord[0].xy).rgb;
	float luminance = dot( vec3(0.3, 0.59, 0.11), color);
	
	/*float luminance = dot( vec3(0.33, 0.33, 0.33), color);*/
	
	 /* [0 .. infinity] is mapped to [0 .. 1] */
	 
	float factor = luminance / (1.0 + luminance);
	
	/* interesting bug : make black area */
	/* float factor = sin((luminance * 3.141596 * 0.5) / (1.0 + luminance));*/
	
	gl_FragColor = vec4( factor * color, 1.0 );
}