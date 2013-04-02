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

    uv.x = .75*time+.1/r;
    uv.y = a/3.1416;

    vec3 col =  texture2D(tex0,uv).xyz;

    gl_FragColor = vec4(col*r,1.0);
}
