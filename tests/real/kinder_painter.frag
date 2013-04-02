uniform vec2 resolution;
uniform float time;
uniform vec4 mouse;
uniform sampler2D tex0;
uniform sampler2D tex1;
uniform sampler2D tex2;
uniform sampler2D tex3;

vec4 fpar00[6];
vec4 fpar01[6];


float cylinder( in vec4 sph, in vec3 ro, in vec3 rd )
{
    vec3  d = ro - sph.xyz;
    float a = dot( rd.xz, rd.xz );
    float b = dot( rd.xz, d.xz );
    float c = dot( d.xz, d.xz ) - sph.w*sph.w;
    float t;

    t = b*b - a*c;
    if( t>0.0 )
    {
        t = -(b+sqrt( t ))/a;
    }

    return t-.001;

}


float esfera( in vec4 sph, in vec3 ro, in vec3 rd )
{
    vec3  d = ro - sph.xyz;
    float b = dot( rd, d );
    float c = dot(  d, d ) - sph.w*sph.w;
    float t = b*b - c;

    if( t>0.0 )
    {
        t = -b - sqrt( t );
    }

    return t-.001;
}


bool esfera2( in vec4 sph, in vec3 ro, in vec3 rd, in float tmin )
{
    vec3  d = ro - sph.xyz;
    float b = dot( rd, d );
    float c = dot(  d, d ) - sph.w*sph.w;

    float t = b*b - c;
    bool r = false;

    if( t>0.0 )
    {
        t = -b - sqrt( t );
        r = (t>0.0) && (t<tmin);
    }

    return r;
}


bool cylinder2( in vec4 sph, in vec3 ro, in vec3 rd, in float tmin )
{
    vec3  d = ro - sph.xyz;
    float a = dot( rd.xz, rd.xz );
    float b = dot( rd.xz, d.xz );
    float c = dot( d.xz, d.xz ) - sph.w*sph.w;
    float t = b*b - a*c;
    bool r = false;
    if( t>0.0 )
    {
        t = -(b+sqrt(t));
        r = (t>0.0) && (t<(tmin*a));
    }
    return r;
}


float plane( in vec4 pla, in vec3 ro, in vec3 rd )
{
    float de = dot(pla.xyz, rd);
    de = sign(de)*max( abs(de), 0.001);
    float t = -(dot(pla.xyz, ro) + pla.w)/de;
    return t-.001;
}


vec3 calcnor( in vec4 obj, in vec4 col, in vec3 inter, out vec2 uv )
{
    vec3 nor;
    if( col.w>2.5 )
    {
        nor.xz = inter.xz - obj.xz;
        nor.y = 0.0;
        nor = nor/obj.w;
        //uv = vec2( atan(nor.x,nor.z)/3.14159, inter.y );
        uv = vec2( nor.x, inter.y );
    }
    else if( col.w>1.5 )
    {
        nor = obj.xyz;
        uv = inter.xz*.2;
    }
    else
    {
        nor = inter - obj.xyz;
        nor = nor/obj.w;
        uv = nor.xy;
    }

    return nor;
}

vec4 cmov( in vec4 a, in vec4 b, in bool cond )
{
    return cond?b:a;
}

float cmov( in float a, in float b, in bool cond )
{
    return cond?b:a;
}

int cmov( in int a, in int b, in bool cond )
{
    return cond?b:a;
}

float intersect( in vec3 ro, in vec3 rd, out vec4 obj, out vec4 col )
{
    float tmin = 10000.0;
    float t;

    col.w = -1.0;

    bool isok;

    t = esfera( fpar00[0], ro, rd );
    isok = (t>0.0) && (t<tmin);
    obj  = cmov( obj, fpar00[0], isok );
    col  = cmov( col, fpar01[0], isok );
    tmin = cmov( tmin, t, isok );

    t = esfera( fpar00[1], ro, rd );
    isok = (t>0.0) && (t<tmin);
    obj  = cmov( obj, fpar00[1], isok );
    col  = cmov( col, fpar01[1], isok );
    tmin = cmov( tmin, t, isok );

    t = cylinder( fpar00[2], ro, rd );
    isok = ( t>0.0 && t<tmin );
    obj  = cmov( obj, fpar00[2], isok );
    col  = cmov( col, fpar01[2], isok );
    tmin = cmov( tmin, t, isok );

    t = cylinder( fpar00[3], ro, rd );
    isok = ( t>0.0 && t<tmin );
    obj  = cmov( obj, fpar00[3], isok );
    col  = cmov( col, fpar01[3], isok );
    tmin = cmov( tmin, t, isok );

    t = plane( fpar00[4], ro, rd );
    isok = ( t>0.0 && t<tmin );
    obj  = cmov( obj, fpar00[4], isok );
    col  = cmov( col, fpar01[4], isok );
    tmin = cmov( tmin, t, isok );

    t = plane( fpar00[5], ro, rd );
    isok = ( t>0.0 && t<tmin );
    obj  = cmov( obj, fpar00[5], isok );
    col  = cmov( col, fpar01[5], isok );
    tmin = cmov( tmin, t, isok );

    return tmin;
}






bool intersectShadow( in vec3 ro, in vec3 rd, in float l )
{
    float t;

    bvec4 sss;

    sss.x = esfera2(   fpar00[0], ro, rd, l );
    sss.y = esfera2(   fpar00[1], ro, rd, l );
    sss.z = cylinder2( fpar00[2], ro, rd, l );
    sss.w = cylinder2( fpar00[3], ro, rd, l );

    return any(sss);
}


vec4 basicShade( in vec3 inter, in vec4 obj, in vec4 col, in vec3 rd, in vec4 luz, out vec4 ref )
{
    vec3 nor;
    float dif, spe;
    vec2 uv;


    nor = calcnor( obj, col, inter, uv );


    dif = dot( nor, luz.xyz );
    ref.xyz = reflect( rd, nor );
    spe = dot( ref.xyz, luz.xyz );
    spe = max( spe, 0.0 );
    spe = spe*spe;
    spe = spe*spe;

    if( intersectShadow( inter, luz.xyz, luz.w ) )
    {
        dif = 0.0;
    }


    col *= texture2D( tex0, uv );

    // amb + dif + spec
    dif = max(dif,0.0);
    col = col*( vec4(.3,.34,.38,1) + .5*vec4(1.0,0.95,0.8,1.0)*dif ) + .5*spe;

    // fresnel
    dif = dot( nor, -rd );
    ref.w = dif;
    dif = 1.0 - dif*dif;
    dif = dif*dif;
    col = col + .35*vec4( dif );

    return( col );
}

void main( void )
{
    vec4  luz;
    vec4  obj, col;
    vec3  nor;
    vec4  ref;

    vec2 p = -1.0 + 2.0 * gl_FragCoord.xy / resolution.xy;
    p *= vec2(resolution.x/resolution.y,1.0);

    fpar00[0] = vec4( 1.2*sin( 6.2831*.33*time + 0.0 ), 0.0,  1.8*sin( 6.2831*.39*time + 1.0 ), 1 );
    fpar00[1] = vec4( 1.5*sin( 6.2831*.31*time + 4.0 ), 1.0*sin( 6.2831*.29*time + 1.9),  1.8*sin( 6.2831*.29*time + 0.0 ), 1 );
    fpar00[2] = vec4( -1.2, 0.0, -0.0, 0.4 );
    fpar00[3] = vec4(  1.2, 0.0, -0.0, 0.4 );
    fpar00[4] = vec4(  0.0, 1.0, 0.0, 2.0 );
    fpar00[5] = vec4(  0.0, -1.0, 0.0, 2.0 );

    fpar01[0] = vec4( 0.9, 0.8, 0.6, 1.0 );
    fpar01[1] = vec4( 1.0, 0.6, 0.4, 1.0 );
    fpar01[2] = vec4( 0.8, 0.6, 0.5, 3.0 );
    fpar01[3] = vec4( 0.5, 0.5, 0.7, 3.0 );
    fpar01[4] = vec4( 1.0, 0.9, 0.9, 2.0 );
    fpar01[5] = vec4( 1.0, 0.9, 0.9, 2.0 );

    float an = .15*time - 6.2831*mouse.x/resolution.x;
    float di = 2.0+3.0*mouse.y/resolution.y;
    vec2 sc = vec2(cos(an),sin(an));
    vec3 rd = normalize(vec3(p.x*sc.x-sc.y,p.y,sc.x+p.x*sc.y));
    vec3 ro = vec3(di*sc.y,0.0,-di*sc.x);

    float tmin = intersect( ro, rd, obj, col );

    vec3 inter = ro + rd*tmin;

    luz.xyz = vec3(0.0,1.5,-3.0)-inter;
    luz.w = length( luz.xyz );
    luz.xyz = luz.xyz/luz.w;

    col = basicShade( inter, obj, col, rd, luz, ref );

    vec4 col2;
    vec4 ref2;
    tmin = intersect( inter, ref.xyz, obj, col2 );
    inter = inter + ref.xyz*tmin;
    luz.xyz = vec3(0.0,1.5,-1.0)-inter;
    luz.w = length( luz.xyz );
    luz.xyz = luz.xyz/luz.w;
    col2 = basicShade( inter, obj, col2, ref.xyz, luz, ref2 );

    col = mix( col, col2, .5-.5*ref.w );

    gl_FragColor = col;
}
