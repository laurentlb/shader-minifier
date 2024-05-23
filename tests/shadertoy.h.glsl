precision highp float;

uniform vec3      iResolution;
uniform float     iTime;
uniform float     iTimeDelta;
uniform int       iFrame;
uniform float     iChannelTime[4];
uniform vec3      iChannelResolution[4];
uniform vec4      iMouse;
uniform vec4      iDate;
uniform float     iSampleRate;

uniform sampler2D iChannel0;
uniform sampler2D iChannel1;
uniform sampler2D iChannel2;
uniform sampler2D iChannel3;

// hack: make all samplers sampler2D
// ( real shadertoy.com iChanneln declarations depend on channel settings )
#define samplerCube sampler2D
vec4 _shadertoy_h_texture(sampler2D s,vec2 v){return vec4(0);}
vec4 _shadertoy_h_texture(sampler2D s,vec2 v,float b){return vec4(0);}
vec4 _shadertoy_h_texture(sampler2D s,vec3 v){return vec4(0);}
#define texture _shadertoy_h_texture

void mainImage (out vec4 O, vec2 f);
out vec4 shadertoy_out_color;
void main () {
  mainImage(shadertoy_out_color,gl_FragCoord.xy);
}
