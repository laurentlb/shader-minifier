#extension GL_EXT_gpu_shader4: enable

uniform vec2 resolution;
uniform float time;
uniform sampler2D tex0;
uniform sampler2D tex1;
uniform sampler2D tex2;
uniform sampler2D tex3;

float coolfFunc3d2( int n )
{
    n = (n << 13) ^ n;
    n = (n * (n * n * 15731 + 789221) + 1376312589) & 0x7fffffff;
    return float(n);
}

//-----------------------------------------------------------------------------

float noise3f( in vec3 x, int sem )
{
    ivec3 ip = ivec3(floor(x));
    vec3 f = fract(x);

    f = f*f*(3.0-2.0*f);
   //f = f*f*f*(f*(f*6.0-15.0)+10.0);

    int n = ip.x + ip.y * 57 + 113*ip.z + sem;

    float res = mix(mix(mix( coolfFunc3d2(n+(0+57*0+113*0)), coolfFunc3d2(n+(1+57*0+113*0)),f.x),
                        mix( coolfFunc3d2(n+(0+57*1+113*0)), coolfFunc3d2(n+(1+57*1+113*0)),f.x),f.y),
                    mix(mix( coolfFunc3d2(n+(0+57*0+113*1)), coolfFunc3d2(n+(1+57*0+113*1)),f.x),
                        mix( coolfFunc3d2(n+(0+57*1+113*1)), coolfFunc3d2(n+(1+57*1+113*1)),f.x),f.y),f.z);
    return 1.0 - res*(1.0/1073741824.0);
}

//----------------------------------------------------------------------------

vec2 celular( in vec3 x )
{
    ivec3 ip = ivec3(floor(x));
    vec3 f = fract(x);

    vec2 dmin = vec2( 1.0, 1.0 );

    for( int k=-1; k<=1; k++ )
    for( int j=-1; j<=1; j++ )
    for( int i=-1; i<=1; i++ )
    {
        int nn = (ip.x+i) + 57*(ip.y+j) + 113*(ip.z+k);

        vec3 di = vec3(float(i),float(j),float(k)) - f + vec3(coolfFunc3d2(nn), coolfFunc3d2(nn+1217), coolfFunc3d2(nn+2513))/2147483647.0;

        float d2 = dot(di,di);

        if( d2<dmin.x )
        {
            dmin.y = dmin.x;
            dmin.x = d2;
        }
        else if( d2<dmin.y )
        {
            dmin.y = d2;
        }
    }
    return 0.25*sqrt(dmin);
}

//----------------------------------------------------------------------------

float fbm( in vec3 x )
{
    return 0.5000*noise3f( x*1.0, 0 ) +
           0.2500*noise3f( x*2.0, 0 ) +
           0.1250*noise3f( x*4.0, 0 ) +
           0.0625*noise3f( x*8.0, 0 );
}

//============================================================================
float map( in vec3 x, out float ao )
{
    vec3 d = mod( vec3(1024.0)+x, 1.0 ) - 0.5;

    float dis = sqrt( dot(d,d) ) - 0.3*0.3;

    float disp = noise3f( 4.0*x, 0 );
    dis += 0.8*disp;
    ao = clamp(-1.5*disp, 0.0, 1.0);

    vec2 cel = celular( 16.0*x );
    float disp2 = clamp(cel[1] - cel[0], 0.0, 1.0);
    dis -= disp2;
    ao *= clamp(disp2*12.0, 0.0, 1.0);

    return dis;
}


vec3 calcNormal( in vec3 pos )
{
    float kk;
    float eps = 0.0002;
    vec3 nor = vec3( map( pos+vec3(eps,0.0,0.0), kk ) - map( pos-vec3(eps,0.0,0.0), kk ),
                     map( pos+vec3(0.0,eps,0.0), kk ) - map( pos-vec3(0.0,eps,0.0), kk ),
                     map( pos+vec3(0.0,0.0,eps), kk ) - map( pos-vec3(0.0,0.0,eps), kk ) );
    return normalize( nor );
}



void generateRay( out vec3 rayDir, out vec3 rayPos, in vec2 p, float ftime )
{
    vec2 s = p;

    float r2 = s.x*s.x*0.32 + s.y*s.y;
    vec2 d = s*(7.0-sqrt(37.5-11.5*r2))/(r2+1.0);

    vec3 rayTar = vec3( 0.0, 1.5, 2.0 );

    rayPos = rayTar + vec3( -sin(6.2831853*ftime/20.0), 0.75*cos(6.2831853*ftime/20.0+0.5), -cos(6.2831853*ftime/20.0) );

    rayTar += 0.075*vec3( noise3f( vec3(2.0*ftime,0.0,0.5),0), noise3f( vec3(2.0*ftime,0.1,0.4),7), noise3f( vec3(2.0*ftime,0.2,0.3),9) );

    float roll = 0.1*noise3f( vec3(2.0*ftime,0.0,0.0), 13 );

    vec3 up = vec3( 0.0, cos(roll), sin(roll) );

    vec3 dd = normalize( rayTar - rayPos );
    vec3 rr = normalize( cross( dd, up ) );
    vec3 uu = normalize( cross( rr, dd ) );

    rayDir = normalize( d.x*rr + d.y*uu + dd );
}

vec3 addbump( in vec3 nor, float bumpa, in vec3 x )
{
    float ke = 0.0005;
    float kk = fbm( 256.0*x );
    vec3 xnor = vec3( fbm(256.0*(x+vec3(ke,0.0,0.0)))-kk,
                      fbm(256.0*(x+vec3(0.0,ke,0.0)))-kk,
                      fbm(256.0*(x+vec3(0.0,0.0,ke)))-kk );
    return normalize( nor + bumpa*xnor );
}

void main(void)
{
    vec2 p = -1.0 + 2.0 * gl_FragCoord.xy / resolution.xy;

    // genearete ray
    vec3 ro, rd;
    generateRay( rd, ro, p, time );

    // ray march scene
    float t, ao; vec3 nor, pos;
    for( t=0.1; t<5.0;  )
    {
        pos = ro + t*rd;
        float h = map( pos, ao );
        if( h<0.001 ) break;
        t += h*0.12;
    }
    nor = calcNormal( pos );

    // shade
    vec3 col = vec3( 1.0 + 0.5*fbm( 96.0*pos ) );
    vec3 xnor = addbump( nor, 1.0, 0.25*pos );
    vec3 lig = vec3( 0.80, 0.50, -0.10 );
    float dif = clamp(dot( xnor, lig ), 0.0, 1.0);
    vec3 ref = vec3(0.50,0.55,0.60) + vec3(0.6,0.5,0.3)*dif*3.0;
    col = col*ref*ao;

    // fog
    col = col/(1.0+t) + vec3(1.06, 1.14, 1.0) * (1.0-exp2(-0.25*t));

    // contrast
    col = (col*col+col)*.5;
    // tint
    col = clamp( col*vec3(1.0,1.2,1.0), 0.0, 1.0 );
    // vigneting
    col *= 0.5 + 0.5*(1.0-p.x)*(1.0+p.x);

    gl_FragColor = vec4(col,1.0);
}
