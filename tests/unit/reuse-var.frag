float simple_var_decl_reuse(float x)
{
	float a = 1.+x;
	int b = 3+int(x);
	vec3 sep = vec3(0.); // prevents squeezeConsecutiveDeclarations
	int b2 = 5+int(a);
	float c = a;
	sep += vec3(0.); // prevents squeezeConsecutiveDeclarations
	float c2 = 9.+a;
	int d = 3+b+b2;
	sep += vec3(0.); // prevents squeezeConsecutiveDeclarations
	int d2 = d+b2-b;
	float e = c*c2;
	sep += vec3(0.); // prevents squeezeConsecutiveDeclarations
	float e2 = 4.-c-c2;
	int f = 3*d*d2;
	sep += vec3(0.); // prevents squeezeConsecutiveDeclarations
	int f2 = 4/d2-d;
	float g = e-float(f)+e2;
	int g2 = int(4.-g+e2+e);
	int h = 3*f-f2;
	sep += vec3(0.); // prevents squeezeConsecutiveDeclarations
	int h2 = 7*f2-f;
	return length(sep)+float(h*h2)*g*float(g2)*x + float(g2/h2-h);
}
float multidecl_var_decl_reuse(float x)
{
	float a = 1.+x;
	int b = 3+int(x), b2 = 5+int(a);
	float c = a, c2 = 9.+a;
	int d = 3+b+b2, d2 = d+b2-b;
	float e = c*c2, e2 = 4.-c-c2;
	int f = 3*d*d2, f2 = 4/d2-d;
	float g = e-float(f)+e2, g2 = 4.-g+e2+e;
	int h = 3*f-f2, h2 = 7*f2-f;
	return float(h*h2)*g*g2*x + g2/float(h2-h);
}
vec2 map (in vec3 p)
{
    vec2 tun = p.xy;
	tun.x++;
    vec3 q = vec3(tun,p.z);
    vec3 fs = p - vec3(2.85,0,0);
    vec2 center = floor(fs.xz) + .5;
    float height = .0465;
    height = smoothstep(.001,1.,height);
    float me =   dot(fs-vec3(0,0,center.y), vec3(.05,.150+height,.25));
    float next = dot(fs-vec3(0,0,center.y), vec3(.05,.001+height,.25));
    float dlt = min(me, next);
    return vec2(dlt*dlt);
}
