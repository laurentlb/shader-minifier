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

    float r = pow( pow(p.x*p.x,16.0) + pow(p.y*p.y,16.0), 1.0/32.0 );
    uv.x = .5*time + 0.5/r;
    uv.y = 1.0*atan(p.y,p.x)/3.1416;

    vec3 col =  texture2D(tex0,uv).xyz;

    gl_FragColor = vec4(col*r*r*r,1.0);
}
