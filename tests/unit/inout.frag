// Multifile test, part 1

// The test is to ensure that exported variables are renamed in the same way
// across multiple files.

#version 330

// These 4 variables should have the same name as in the other file.
uniform samplerCube texture0;

in vec3 normal;
in vec3 viewVec;

out vec4 fragmentColor;

void main()
{
  vec3 E = normalize(viewVec);
  vec3 N = normalize(normal);
  vec3 albedo = vec3(0.1, 0.2, 0.3);
  vec3 f0 = vec3(0.5, 0.5, 0.5);

  float IoR = 1.5;
  vec3 reflection = texture(texture0, reflect(-E, N)).rgb;
  vec3 refraction = texture(texture0, refract(-E, N, 1./IoR)).rgb;

  vec3 color = mix(albedo * refraction, reflection, 0.1);
  fragmentColor = vec4(color, 1.);
}

// These functions are same as in the other file. Ideally (for better compression),
// the local variables and arguments should remain the same.
vec3 Schlick(vec3 f0, vec3 E, vec3 H)
{
  float x = 1. - clamp(dot(E, H), 0., 1.);
  return x*x*x*x*x * (1. - f0) + f0;
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
