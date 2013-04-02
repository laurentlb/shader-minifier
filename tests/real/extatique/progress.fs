uniform sampler2D tex;
uniform float progression;

vec3 desaturate(vec3 c, float s)
{
	return mix( c, vec3(dot(vec3(0.33), c)), s);
}

void main()
{
	float A = 3.1415 * 0.5;
	float TOUR = 13.0;
	
	float t = gl_TexCoord[0].y;	
	float s = gl_TexCoord[0].x + 0.15 * sin(t * 2.0 * 3.1415);
	
	float v = smoothstep(0.0,0.1,t) - smoothstep(0.9,1.0,t);
	
	float center1 = 0.5 + 0.5 * sin(TOUR * t);	
	float distu1 = abs(center1 - s);
	float value1 = sqrt(max(0.0, 2.5 - (9.0 - 3.0 * abs(cos(TOUR * t))) * distu1));
	float alpha1 = value1 *max(0.0,-cos(TOUR * t));
	
	float center2 = 0.5 + 0.5 * sin(TOUR * t + A);	
	float distu2 = abs(center2 - s);
	float value2 = sqrt(max(0.0, 2.5 - (9.0 - 3.0 * abs(cos(TOUR * t + A))) * distu2));
	float alpha2 = value2 *max(0.0,-cos(TOUR * t + A));	
	
	float center3 = 0.5 + 0.5 * sin(TOUR * t + A * 2.0);
	float distu3 = abs(center3 - s);
	float value3 = sqrt(max(0.0, 2.5 - (9.0 - 3.0 * abs(cos(TOUR * t + A * 2.0))) * distu3));
	float alpha3 = value3 *max(0.0,-cos(TOUR * t + A * 2.0));	
	
	float center4 = 0.5 + 0.5 * sin(TOUR * t + A * 3.0);	
	float distu4 = abs(center4 - s);
	float value4 = sqrt(max(0.0, 2.5 - (9.0 - 3.0 * abs(cos(TOUR * t + A * 3.0))) * distu4));
	float alpha4 = value4 * max(0.0, -cos(TOUR * t + A * 3.0));	
	
	const float desat = 0.3;
	float refs1 = (gl_TexCoord[0].x + 0.51) / 2.0;
	float refs2 = (gl_TexCoord[0].x + 0.44) / 2.0;
	float refs3 = (gl_TexCoord[0].x + 0.55) / 2.0;
	float refs4 = (gl_TexCoord[0].x + 0.48) / 2.0;
	vec3 c1 = (refs1 < progression) ? desaturate(vec3(1.0, 0.59, 1.0), desat) : vec3(0.3,0.3,0.3);
	vec3 c2 = (refs2 < progression) ? desaturate(vec3(0.6,0.18,0.19), desat) : vec3(0.4,0.4,0.4);
	vec3 c3 = (refs3 < progression) ? desaturate(vec3(0.97,0.62,118.0/255.0), desat) : vec3(0.6,0.6,0.6);
	vec3 c4 = (refs4 < progression) ? desaturate(vec3(254.0/255.0,79.0/255.0,138.0/255.0), desat) : vec3(0.5,0.5,0.5);
	
	vec4 final_color = vec4(alpha1 * c1
	                      + alpha2 * c2
	                      + alpha3 * c3
	                      + alpha4 * c4, v * (alpha1 + alpha2 + alpha3 + alpha4));
	
	vec2 p = gl_TexCoord[0].xy;
	gl_FragColor = gl_Color * final_color * mix(texture2D(tex, p * vec2(1.5, 4.5)), vec4(1.0,1.0,1.0,1.0), 0.5);
}


