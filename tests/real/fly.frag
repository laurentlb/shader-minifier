uniform vec2 resolution;
uniform float time;
uniform sampler2D tex0;
uniform sampler2D tex1;
uniform sampler2D tex2;
void main(void)
{
    vec2 p = -1.0 + 2.0 * gl_FragCoord.xy / resolution.xy;
    vec2 uv;

    float an = time*.25;

    float x = p.x*cos(an)-p.y*sin(an);
    float y = p.x*sin(an)+p.y*cos(an);
     
    uv.x = .25*x/abs(y);
    uv.y = .20*time + .25/abs(y);

    gl_FragColor = vec4(texture2D(tex0,uv).xyz * y*y, 1.0);
}
