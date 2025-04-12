out vec4 fragColor;
uniform sampler2D samp;
vec3 dof(sampler2D tex, float f) {
    vec3 a = texture(tex,vec2(0)).rgb;
   return a.zyx+a.xyz*(f*=f);
}
void main(){
  fragColor.xyz = dof(samp, 10.0);
}
