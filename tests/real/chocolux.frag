uniform vec2 resolution;
uniform float time;

float h(vec3 q)
{
    float f=1.*distance(q,vec3(cos(time)+sin(time*.2),.3,2.+cos(time*.5)*.5));
    f*=distance(q,vec3(-cos(time*.7),.3,2.+sin(time*.5)));
    f*=distance(q,vec3(-sin(time*.2)*.5,sin(time),2.));
    f*=cos(q.y)*cos(q.x)-.1-cos(q.z*7.+time*7.)*cos(q.x*3.)*cos(q.y*4.)*.1;
    return f;
}

void main()
{
    vec2 p = -1.0 + 2.0 * gl_FragCoord.xy / resolution.xy;
    vec3 o=vec3(p.x,p.y*1.25-0.3,0.);
    vec3 d=vec3(p.x+cos(time)*0.3,p.y,1.)/64.;
    vec4 c=vec4(0.);
    float t=0.;
    for(int i=0;i<75;i++)
    {
        if(h(o+d*t)<.4)
        {
            t-=5.;
            for(int j=0;j<5;j++)
            {
                if(h(o+d*t)<.4)
                    break;
                t+=1.;
            }
            vec3 e=vec3(.01,.0,.0);
            vec3 n=vec3(.0);
            n.x=h(o+d*t)-h(vec3(o+d*t+e.xyy));
            n.y=h(o+d*t)-h(vec3(o+d*t+e.yxy));
            n.z=h(o+d*t)-h(vec3(o+d*t+e.yyx));
            n=normalize(n);
            c+=max(dot(vec3(.0,.0,-.5),n),.0)+max(dot(vec3(.0,-.5,.5),n),.0)*.5;
            break;
        }
        t+=5.;
    }
    gl_FragColor=c+vec4(.1,.2,.5,1.)*(t*.025);
}
