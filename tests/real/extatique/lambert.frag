varying vec3 N;
varying vec3 pos;
varying vec3 wpos;

varying vec3 l1_pos;
varying vec3 l2_pos;
varying vec3 l3_pos;

uniform vec3 light1Color;
uniform vec3 light2Color;
uniform vec3 light3Color;
uniform vec3 ambientColor;


uniform float light1Specular;
uniform float light2Specular;
uniform float light3Specular;
uniform float light1Diffuse;
uniform float light2Diffuse;
uniform float light3Diffuse;
uniform float thresholdZ;

const float linAtt = 0.20;
const float quadAtt = 0.1;
const float fogNear = 1.0;
const float fogFar = 10.0;
	
uniform sampler2D noise;


float AO(vec3 p)
{
	return 1.0 / (1.0 + 0.012 * dot(p,p));
}

vec3 desaturate(vec3 c, float s)
{
	return mix( c, vec3(dot(vec3(0.3), c)), s);
}

void main()
{	
	
	vec2 p = gl_TexCoord[0].xy;	
	
	vec3 perturb = vec3(texture2D(noise, p + vec2(0.13,0.46)).x,  texture2D(noise, p).x, texture2D(noise, p + vec2(-0.33,0.14)).x);
	
	perturb = (perturb - vec3(0.5)) * 5.0;
	
	vec3 nml = normalize(N + perturb);
	
	if (nml.z < thresholdZ) discard;
	
	vec3 l1dist = l1_pos - pos;
	vec3 l2dist = l2_pos - pos;
	vec3 l3dist = l3_pos - pos;
	
	float dist1 = length(l1dist);
	float dist2 = length(l2dist);
	float dist3 = length(l3dist);
	
	vec3 lv1 = l1dist / dist1;
	vec3 lv2 = l2dist / dist2;
	vec3 lv3 = l3dist / dist3;
	
	vec3 eye = normalize(-pos);
	
	vec3 re1 = reflect(lv1, N);
	vec3 re2 = reflect(lv2, N);
	vec3 re3 = reflect(lv3, N);
	
	float diffl1 = max(0.0, dot(nml,lv1) );
	float diffl2 = max(0.0, dot(nml,lv2) ); 
	float diffl3 = max(0.0, dot(nml,lv3) );
	
	float specl1 = light1Specular * pow(max(0.0, dot(eye, re1)), 10.0);
	float specl2 = light2Specular * pow(max(0.0, dot(eye, re2)), 10.0);
	float specl3 = light3Specular * pow(max(0.0, dot(eye, re3)), 10.0);
	
	
	float ql1 = (diffl1 + specl1) / (1.0 + dist1 * (linAtt + quadAtt * dist1));
	float ql2 = (diffl2 + specl2) / (1.0 + dist2 * (linAtt + quadAtt * dist2));
	float ql3 = (diffl3 + specl3) / (1.0 + dist3 * (linAtt + quadAtt * dist3));
	
	vec3 light = ambientColor + light1Color * vec3(ql1) + light2Color * vec3(ql2) + light3Color * vec3(ql3);
	

	float fogAmount =  clamp( (pos.z - fogNear) / (fogFar - fogNear) , 0.0, 1.0);
	vec3 lightAO = desaturate(light * vec3(AO(wpos)), fogAmount);
	
	
	gl_FragColor = gl_Color * vec4(lightAO, 1.0);
}
