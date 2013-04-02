uniform vec2 resolution;
uniform float time;
uniform sampler2D tex0;
uniform sampler2D tex1;
uniform sampler2D tex2;
uniform sampler2D tex3;

void main(void)
{
    vec2 p = -1.0 + 2.0 * gl_FragCoord.xy / resolution.xy;
    vec2 uv;

    float a = atan(p.y,p.x);
    float r = sqrt(dot(p,p));

    uv.x = cos(0.6+time) + cos(cos(1.2+time)+a)/r;
    uv.y = cos(0.3+time) + sin(cos(2.0+time)+a)/r;

    vec3 col =  texture2D(tex0,uv*.25).xyz;

    gl_FragColor = vec4(col*r*r,1.0);
}
