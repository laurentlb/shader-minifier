uniform vec2 resolution;
uniform float time;

bool isphere( in vec4 sph, in vec3 ro, in vec3 rd, out vec2 t )
{
    vec3 oc = ro - sph.xyz;
    float b = dot(oc,rd);
    float c = dot(oc,oc) - sph.w*sph.w;

    float h = b*b - c;
    if( h<0.0 )
        return false;

    float g = sqrt( h );
    t.x = - b - g;
    t.y = - b + g;

    return true;
}

const int NumIte = 7;
const float Bailout = 100.0;

bool iterate( in vec3 q, out float resPot, out vec4 resColor )
{
    vec4 trap = vec4(100.0);
    vec3 zz = q;
    float m = dot(zz,zz);
    if( m > Bailout )
    {
        resPot = 0.5*log(m)/pow(8.0,0.0);
        resColor = vec4(1.0);
        return false;
    }

    for( int i=1; i<NumIte; i++ )
    {
    #if 0
        float zr = sqrt( dot(zz,zz) );
        float zo = acos( zz.y/zr );
        float zi = atan( zz.x, zz.z );

        zr = pow( zr, 8.0 );
        zo = zo * 8.0;
        zi = zi * 8.0;

        zz = q + zr*vec3( sin(zo)*sin(zi), cos(zo), sin(zo)*cos(zi) );
    #else
        float x = zz.x; float x2 = x*x; float x4 = x2*x2;
        float y = zz.y; float y2 = y*y; float y4 = y2*y2;
        float z = zz.z; float z2 = z*z; float z4 = z2*z2;

        float k3 = x2 + z2;
        float k2 = inversesqrt( k3*k3*k3*k3*k3*k3*k3 );
        float k1 = x4 + y4 + z4 - 6.0*y2*z2 - 6.0*x2*y2 + 2.0*z2*x2;
        float k4 = x2 - y2 + z2;

        zz.x = q.x +  64.0*x*y*z*(x2-z2)*k4*(x4-6.0*x2*z2+z4)*k1*k2;
        zz.y = q.y + -16.0*y2*k3*k4*k4 + k1*k1;
        zz.z = q.z +  -8.0*y*k4*(x4*x4 - 28.0*x4*x2*z2 + 70.0*x4*z4 - 28.0*x2*z2*z4 + z4*z4)*k1*k2;
    #endif

        m = dot(zz,zz);

        trap = min( trap, vec4(zz.xyz*zz.xyz,m) );

        if( m > Bailout )
        {
            resColor = trap;
            resPot = 0.5*log(m)/pow(8.0,float(i));
            return false;
        }
    }

    resColor = trap;
    resPot = 0.0;
    return true;
}

bool ifractal( in vec3 ro, in vec3 rd, out float rest, in float maxt, out vec3 resnor, out vec4 rescol, float fov )
{
    vec4 sph = vec4( 0.0, 0.0, 0.0, 1.25 );
    vec2 dis;

    // bounding sphere
    if( !isphere(sph,ro,rd,dis) )
        return false;

    // early skip
    if( dis.y<0.001 ) return false;
    // clip to near!
    if( dis.x<0.001 )dis.x = 0.001;

    if( dis.y>maxt) dis.y = maxt;

    float dt;
    vec3 gra;
    vec4 color;

    float fovfactor = 1.0/sqrt(1.0+fov*fov);

    // raymarch!
    for( float t=dis.x; t<dis.y;  )
    {
        vec3 p = ro + rd*t;

        float Surface = clamp( 0.001*t*fovfactor, 0.000001, 0.005 );

        float eps = Surface*0.1;
        vec4 col2;
        float pot1; if( iterate(p,pot1,color) ) { rest = t; resnor=normalize(gra); rescol = color; return true; }
        float pot2; iterate(p+vec3(eps,0.0,0.0),pot2,col2);
        float pot3; iterate(p+vec3(0.0,eps,0.0),pot3,col2);
        float pot4; iterate(p+vec3(0.0,0.0,eps),pot4,col2);

        gra = vec3( pot2-pot1, pot3-pot1, pot4-pot1 );
        dt = 0.5*pot1*eps/length(gra);

        if( dt<Surface )
        {
            rescol = color;
            resnor = normalize( gra );
            rest = t;
            return true;
        }

        t+=dt;
    }

    return false;
}

void main(void)
{
    vec2 p = -1.0 + 2.0 * gl_FragCoord.xy / resolution.xy;
    vec2 s = p*vec2(1.33,1.0);

    vec3 light1 = vec3(  0.577, 0.577, 0.577 );
    vec3 light2 = vec3( -0.707, 0.000, 0.707 );

    float fov = 1.0;
    float r = 1.4+0.2*cos(6.28318*time/20.0);
    vec3 campos = vec3( r*sin(6.28318*time/20.0), 0.3-0.4*sin(6.28318*time/20.0), r*cos(6.28318*time/20.0) );
    vec3 camtar = vec3(0.0,0.1,0.0);
    //camera matrix
    vec3 cw = normalize(camtar-campos);
    vec3 cp = vec3(0.0,1.0,0.0);
    vec3 cu = normalize(cross(cw,cp));
    vec3 cv = normalize(cross(cu,cw));
    // ray dir
    vec3 rd = normalize( s.x*cu + s.y*cv + 1.5*cw );


    vec3 nor, rgb;
    vec4 col;
    float t;
    if( !ifractal(campos,rd,t,1e20,nor,col,fov) )
    {
     	rgb = 1.3*vec3(1.0,.98,0.9)*(0.7+0.3*rd.y);
    }
    else
    {
        vec3 xyz = campos + t*rd;

        // sun light
        float dif1 = clamp( 0.2 + 0.8*dot( light1, nor ), 0.0, 1.0 ); dif1=dif1*dif1;
        // back light
        float dif2 = clamp( 0.3 + 0.7*dot( light2, nor ), 0.0, 1.0 );
        // ambient occlusion
        float ao = clamp(1.25*col.w-.4,0.0,1.0); ao=ao*ao*0.5+0.5*ao;
        // shadow
        float lt1; vec3 ln; vec4 lc;
        if( dif1>0.001 ) if( ifractal(xyz,light1,lt1,1e20,ln,lc,fov) ) dif1 = 0.1;

        // material color
        rgb = vec3(1.0,1.0,1.0);
        rgb = mix( rgb, vec3(0.8,0.6,0.2), sqrt(col.x)*1.25 );
        rgb = mix( rgb, vec3(0.8,0.3,0.3), sqrt(col.y)*1.25 );
        rgb = mix( rgb, vec3(0.7,0.4,0.3), sqrt(col.z)*1.25 );

        // lighting
        rgb *= (0.5+0.5*nor.y)*vec3(.14,.15,.16)*0.8 +  dif1*vec3(1.0,.85,.4) + 0.5*dif2*vec3(.08,.10,.14);
        rgb *= vec3( pow(ao,0.8), pow(ao,1.00), pow(ao,1.1) );

        // gamma
        rgb = 1.5*(rgb*0.15 + 0.85*sqrt(rgb));
    }

    vec2 uv = p*0.5+0.5;
    rgb *= 0.7 + 0.3*16.0*uv.x*uv.y*(1.0-uv.x)*(1.0-uv.y);

    rgb = clamp( rgb, 0.0, 1.0 );
    gl_FragColor = vec4(rgb,1.0);
}
