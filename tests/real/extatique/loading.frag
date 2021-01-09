uniform sampler2D tex;
uniform sampler3D gammaRamp;

uniform float blurType;
uniform float blurAmount;


void main()
{	
	vec2 p = gl_TexCoord[0].xy;
	
	vec4 blur = ( texture2DLod(tex, p, 7.0)
	            + texture2DLod(tex, p, 6.0)
	            + texture2DLod(tex, p, 5.0)
	            + texture2DLod(tex, p, 4.0)
	            + texture2DLod(tex, p, 3.0)
	            + texture2DLod(tex, p, 2.0) 
	            + texture2DLod(tex, p, 1.0)) * vec4(0.142857); /* 1/7 */
	
	vec4 center = texture2D(tex, p);

	
	vec4 blurred = (vec4(1.0) - (vec4(1.0) - blur) * (vec4(1.0) - center));
	vec4 blurred2 =  blurred * mix( blurred, vec4(1.0), blurType);
	
	vec4 uncorrected = mix(center, blurred2, blurAmount); 	
	
	/* gamma correction */
	
	gl_FragColor = texture3D(gammaRamp, uncorrected.xyz);
}
