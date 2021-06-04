// Multifile test, part 2

#version 330

uniform samplerCube texture0;

uniform float mediumDensity;
uniform vec3 ambientLight;
uniform vec3 diffuseColor;
uniform vec3 emissiveColor;
uniform vec3 specularColor;

in vec3 normal;
in vec3 viewVec;

out vec4 fragmentColor;

vec3 Schlick(vec3 f0, vec3 E, vec3 H)
{
  float x = 1. - clamp(dot(E, H), 0., 1.);
  return x*x*x*x*x * (1. - f0) + f0;
}

void main()
{
  vec3 E = normalize(viewVec);
  vec3 N = normalize(normal);
  vec3 albedo = diffuseColor;
  vec3 f0 = specularColor;
  float schlick = 0.5;

  vec3 color = emissiveColor + mix(albedo * ambientLight, ambientLight, schlick);
  fragmentColor = vec4(color, 1.);
}

vec3 BlinnPhong(vec3 E, vec3 L, vec3 N, vec3 albedo, vec3 f0, float roughness)
{
  vec3 H = normalize(E + L);
  float alpha = 1. + 2048. * (1. - roughness)*(1. - roughness);

  vec3 diffuse = albedo;
  vec3 specular = vec3(pow(clamp(dot(H, N), 0., 1.), alpha) * (alpha + 4.) / 8.);
  vec3 fresnel = Schlick(f0, E, H);

  return mix(diffuse, specular, fresnel);
}
