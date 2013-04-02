uniform vec2 resolution;
uniform float time;
uniform sampler2D tex0;
uniform sampler2D tex1;
uniform sampler2D tex2;
uniform sampler2D tex3;

float jinteresct(in vec3 rO, in vec3 rD, in vec4 c, out float ao)
{
    float mz2,md2,dist,t;
    float res=1000.0;
    vec4 z,nz;

    ao = 0.0;
    for(t=0.0;t<6.0;t+=dist)
    {
        ao += 1.0;
        vec3 p=rO+t*rD;

        // calc distance
        z=vec4(p,(c.y+c.x)*.3);
        md2=1.0;
        mz2=dot(z,z);

        for(int i=0;i<9;i++)
        {
             // |dz|^2 -> 4*|dz|^2
             md2*=4.0*mz2;
             // z -> z2 + c
             nz.x=z.x*z.x-dot(z.yzw,z.yzw);
             nz.yzw=2.0*z.x*z.yzw;
             z=nz+c;

             mz2=dot(z,z);
             if(mz2>4.0)
                 break;
         }

         dist=0.25*sqrt(mz2/md2)*log(mz2);

         if(dist<0.0005)
         {
             res=t;
             break;
         }

     }

    return res;
}


vec3 calcNormal(in vec3 p, in vec4 c)
{
    vec4 nz,ndz,dz[4];

    vec4 z=vec4(p,(c.y+c.x)*.3);

    dz[0]=vec4(1.0,0.0,0.0,0.0);
    dz[1]=vec4(0.0,1.0,0.0,0.0);
    dz[2]=vec4(0.0,0.0,1.0,0.0);
  //dz[3]=vec4(0.0,0.0,0.0,1.0);

    for(int i=0;i<9;i++)
    {
        vec4 mz = vec4(z.x,-z.y,-z.z,-z.w);
        // derivative
        dz[0]=vec4(dot(mz,dz[0]),z.x*dz[0].yzw+dz[0].x*z.yzw);
        dz[1]=vec4(dot(mz,dz[1]),z.x*dz[1].yzw+dz[1].x*z.yzw);
        dz[2]=vec4(dot(mz,dz[2]),z.x*dz[2].yzw+dz[2].x*z.yzw);
        //dz[3]=vec4(dot(mz,dz[3]),z.x*dz[3].yzw+dz[3].x*z.yzw);

        // z = z2 + c
        nz.x=dot(z, mz);
        nz.yzw=2.0*z.x*z.yzw;
        z=nz+c;

        if(dot(z,z)>4.0)
            break;
        }

    return normalize(vec3(dot(z,dz[0]),dot(z,dz[1]),dot(z,dz[2])));
}


void main(void)
{
    vec2 p = -1.0 + 2.0 * gl_FragCoord.xy / resolution.xy;
    vec3 color = vec3(0.0);
    vec4 cccc = vec4( .7*cos(.5*time), .7*sin(.3*time), .7*cos(1.0*time), 0.0 );
    vec3 edir = normalize(vec3(p,1.0));
    vec3 wori = vec3(0.0,0.0,-2.0);

    float ao;
    float t = jinteresct(wori,edir,cccc,ao);
    if(t<100.0)
    {
        vec3 inter = wori + t*edir;
        vec3 nor = calcNormal(inter,cccc);

        float dif = .5 + .5*dot( nor, vec3(0.57703) );
        ao = max( 1.0-ao*0.005, 0.0 );

        color = vec3(1.0,.9,.5)*dif*ao +  .5*vec3(.6,.7,.8)*ao;
    }
    else
    {
        color = vec3(0.5,0.51,0.52)+vec3(0.5,0.47,0.45)*p.y;
    }

    gl_FragColor = vec4(color,1.0);
}
