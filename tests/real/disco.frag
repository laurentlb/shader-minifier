uniform vec2 resolution;
uniform float time;
uniform sampler2D tex0;
uniform sampler2D tex1;
uniform sampler2D tex2;
uniform sampler2D tex3;

vec4 s(vec2 px,float z)
{
    float l=3.1415;
    float k=time*sign(z);
    float x = px.x*320.0*.0065*z;
    float y = px.y*240.0*.0060*z;
    float c=sqrt(x*x+y*y);
    if(c>1.0)
    {
        return vec4(0.0);
    }
    else
    {
        float u=-.4*sign(z)+sin(k*.05);
        float v=sqrt(1.0-x*x-y*y);
        float q=y*sin(u)-v*cos(u);
        y=y*cos(u)+v*sin(u);
        v=acos(y);
        u=acos(x/sin(v))/(2.0*l)*120.0*sign(q)-k;
        v=v*60.0/l;
        q=cos(floor(v/l));
        c=pow(abs(cos(u)*sin(v)),.2)*.1/(q+sin(float(int((u+l/2.0)/l))+k*.6+cos(q*25.0)))*pow(1.0-c,.9);

        vec4 res;
        if(c<0.0)
           res = vec4(-c/2.0*abs(cos(k*.1)),0.0,-c*2.0*abs(sin(k*.04)),1.0);
        else
           res = vec4(c,c*2.0,c*2.0,1.0);
        return res;
    }
}

void main(void)
{
    vec2 p = -1.0 + 2.0 * gl_FragCoord.xy / resolution.xy;
    vec4 c = vec4(0.0);
    for(int i=80;i>0;i--)
        c+=s(p,1.0-float(i)/80.0)*(.008-float(i)*.00005);
    vec4 d=s(p,1.0);
    gl_FragColor = (d.a==0.0?s(p,-.2)*.02:d)+sqrt(c);
}
