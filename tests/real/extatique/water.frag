uniform sampler2D tex; 
uniform sampler2D texu;
uniform sampler2D texv;
uniform float spacing;
uniform float ratio;

void main()
{	
	vec2 p0 = gl_TexCoord[0].xy;
		
	vec4 total = texture2D(tex, p0 * 2.5);	
	vec2 v0 = vec2(spacing) * (vec2(texture2D(texu, p0).x, texture2D(texv, p0).x));
	
	vec2 p1 = p0 - v0;
	total = texture2D(tex, p1) * 0.9755297;	
	vec2 v1 = vec2(spacing) *(vec2(texture2D(texu, p1).x, texture2D(texv, p1).x));
	
	vec2 p2 = p1 - v1;
	total += texture2D(tex, p2) * 0.9045139;	
	vec2 v2 = vec2(spacing) *(vec2(texture2D(texu, p2).x, texture2D(texv, p2).x));
	
	vec2 p3 = p2 - v2;
	total += texture2D(tex, p1) * 0.7939039;	
	vec2 v3 = vec2(spacing) *(vec2(texture2D(texu, p3).x, texture2D(texv, p3).x));
	
	vec2 p4 = p3 - v3;
	total += texture2D(tex, p2) * 0.6545261;		
	vec2 v4 = vec2(spacing) *(vec2(texture2D(texu, p4).x, texture2D(texv, p4).x));
	
	vec2 p5 = p4 - v4;
	total += texture2D(tex, p1) * 0.5000232;
	vec2 v5 = vec2(spacing) *(vec2(texture2D(texu, p5).x, texture2D(texv, p5).x));
	
	vec2 p6 = p5 - v5;
	total += texture2D(tex, p2) * 0.3455179;	
	vec2 v6 = vec2(spacing) *(vec2(texture2D(texu, p6).x, texture2D(texv, p6).x));
	
	vec2 p7 = p6 - v6;
	total += texture2D(tex, p1) * 0.2061336;	
	vec2 v7 = vec2(spacing) *(vec2(texture2D(texu, p7).x, texture2D(texv, p7).x));
	
	vec2 p8 = p7 - v7;
	total += texture2D(tex, p2) *  0.0955133;		
	
	
	
	vec2 p12 = p0 + v0;
	total += texture2D(tex, p12) * 0.9755297;	
	vec2 v12 = vec2(spacing) *(vec2(texture2D(texu, p12).x, texture2D(texv, p12).x));
	
	vec2 p22 = p12 + v12;
	total += texture2D(tex, p22) * 0.9045139;	
	vec2 v22 = vec2(spacing) *(vec2(texture2D(texu, p22).x, texture2D(texv, p22).x));
	
	vec2 p32 = p22 + v22;
	total += texture2D(tex, p12) * 0.7939039;	
	vec2 v32 = vec2(spacing) *(vec2(texture2D(texu, p32).x, texture2D(texv, p32).x));
	
	vec2 p42 = p32 + v32;
	total += texture2D(tex, p22) * 0.6545261;		
	vec2 v42 = vec2(spacing) *(vec2(texture2D(texu, p42).x, texture2D(texv, p42).x));
	
	vec2 p52 = p42 + v42;
	total += texture2D(tex, p12) * 0.5000232;
	vec2 v52 = vec2(spacing) *(vec2(texture2D(texu, p52).x, texture2D(texv, p52).x));
	
	vec2 p62 = p52 + v52;
	total += texture2D(tex, p22) * 0.3455179;	
	vec2 v62 = vec2(spacing) *(vec2(texture2D(texu, p62).x, texture2D(texv, p62).x));
	
	vec2 p72 = p62 + v62;
	total += texture2D(tex, p12) * 0.2061336;	
	vec2 v72 = vec2(spacing) *(vec2(texture2D(texu, p72).x, texture2D(texv, p72).x));
	
	vec2 p82 = p72 + v72;
	total += texture2D(tex, p22) *  0.0955133;		
	
	vec2 dist = (p0 - vec2(0.5, 0.5)) * vec2(1.0,ratio);
	vec4 darkF = vec4(max(0.0, 1.0 - 2.5 * dot(dist,dist)));
	
	vec4 finalColor = gl_Color * total * vec4(0.1) * darkF;
	
	gl_FragColor = finalColor;
}
