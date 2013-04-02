#extension GL_EXT_gpu_shader4: enable

uniform vec2 resolution;
uniform float time;
uniform sampler2D tex0;

int icoolfFunc3d2( in int n )
{
    n=(n<<13)^n;
    return (n*(n*n*15731+789221)+1376312589) & 0x7fffffff;
}

float coolfFunc3d2( in int n )
{
    return float(icoolfFunc3d2(n));
}

float noise3f( in vec3 p )
{
    ivec3 ip = ivec3(floor(p));
    vec3 u = fract(p);
    u = u*u*(3.0-2.0*u);

    int n = ip.x + ip.y*57 + ip.z*113;

    float res = mix(mix(mix(coolfFunc3d2(n+(0+57*0+113*0)),
                            coolfFunc3d2(n+(1+57*0+113*0)),u.x),
                        mix(coolfFunc3d2(n+(0+57*1+113*0)),
                            coolfFunc3d2(n+(1+57*1+113*0)),u.x),u.y),
                    mix(mix(coolfFunc3d2(n+(0+57*0+113*1)),
                            coolfFunc3d2(n+(1+57*0+113*1)),u.x),
                        mix(coolfFunc3d2(n+(0+57*1+113*1)),
                            coolfFunc3d2(n+(1+57*1+113*1)),u.x),u.y),u.z);

    return 1.0 - res*(1.0/1073741824.0);
}

float fbm( in vec3 p )
{
    return 0.5000*noise3f(p*1.0) +
           0.2500*noise3f(p*2.0) +
           0.1250*noise3f(p*4.0) +
           0.0625*noise3f(p*8.0);
}


float techo( in float x, in float y )
{
    y = 1.0 - y;
    if( x<0.1 || x>0.9 ) return y;
    x = x - 0.5;
    return -(sqrt(x*x+y*y)-0.4);
}


float distToBox( in vec3 p, in vec3 abc )
{
    vec3 di=max(abs(p)-abc,0.0);
    return dot(di,di);
}


float columna( in float x, in float y, in float z, in float mindist, in float offx )
{
    vec3 p=vec3(x,y,z);
    float di0=distToBox( p, vec3(0.14,1.0,0.14) );
    if( di0 > (mindist*mindist) ) return mindist + 1.0;

    float y2=y-0.40;
    float y3=y-0.35;
    float y4=y-1.00;

    float di1=distToBox( p, vec3(0.10,1.00,0.10) );
    float di2=distToBox( p, vec3(0.12,0.40,0.12) );
    float di3=distToBox( p, vec3(0.05,0.35,0.14) );
    float di4=distToBox( p, vec3(0.14,0.35,0.05) );
    float di9=distToBox( vec3(x,y4,z), vec3(0.14,0.02,0.14) );

    float di5=distToBox( vec3((x-y2)*0.7071, (y2+x)*0.7071, z), vec3(0.10*0.7071,  0.10*0.7071, 0.12) );
    float di6=distToBox( vec3(x, (y2+z)*0.7071, (z-y2)*0.7071), vec3(0.12,  0.10*0.7071, 0.10*0.7071) );
    float di7=distToBox( vec3((x-y3)*0.7071, (y3+x)*0.7071, z), vec3(0.10*0.7071,  0.10*0.7071, 0.14) );
    float di8=distToBox( vec3(x, (y3+z)*0.7071, (z-y3)*0.7071), vec3(0.14,  0.10*0.7071, 0.10*0.7071) );

    float di=min(min(min(di1,di2),min(di3,di4)),min(min(di5,di6),min(di7,di8)));
    di=min(di,di9);

  //  di += 0.00000003*max( fbm(10.1*p), 0.0);

    return di;
}



float bicho( vec3 x, in float mindist )
{
//    float ramo = noise3f( vec3(2.0*time, 2.3*time, 0.0) );

    x -= vec3(0.64,0.5,1.5);

    float r2 = dot(x,x);

    float sa = smoothstep(0.0,0.5,r2);
    float fax = 0.75 + 0.25*sa;
    float fay = 0.80 + 0.20*sa;

    x.x *= fax;
    x.y *= fay;
    x.z *= fax;

    r2 = dot(x,x);

    float r = sqrt(r2);

    float a1 = 1.0-smoothstep( 0.0, 0.75, r );
    a1 *= 0.40;
    float si1 = sin(a1);
    float co1 = cos(a1);
    x.xy = mat2(co1,si1,-si1,co1)*x.xy;


    float mindist2 = 100000.0;

    float rr = 0.05+sqrt(dot(x.xz,x.xz));
    float ca = (0.5-0.045*0.75) -6.0*rr*exp2(-10.0*rr);
    for( int j=1; j<7; j++ )
    {
        float an = (6.2831/7.0) * float(j);
        float aa = an + 0.40*rr*noise3f( vec3(4.0*rr, 2.5, an) ) + 0.29;
        float rc = cos(aa);
        float rs = sin(aa);
        vec3 q = vec3( x.x*rc-x.z*rs, x.y+ca, x.x*rs+x.z*rc );
        float dd = dot(q.yz,q.yz);
        if( q.x>0.0 && q.x<1.5 && dd<mindist2 ) mindist2=dd;
    }

    float c = sqrt(mindist2) - 0.045;
    float d = r-0.30;
    float a = clamp( r*3.0, 0.0, 1.0 );
    return c*a + d*(1.0-a);
}


float map( in vec3 pos, out int sid, out int submat )
{
    submat = 0;
    float dis, mindist;

    //-----------------------
    // suelo
    //-----------------------
    dis = pos.y;
    vec2 axz = vec2(128.0) + 6.0*vec2(pos.x+pos.z,pos.x-pos.z);
    ivec2 ixz = ivec2( floor(axz) );
    submat = icoolfFunc3d2(ixz.x+53*ixz.y);
    vec2 peldxz = fract( axz );
    float peld = smoothstep( 0.975, 1.0, max( peldxz.x, peldxz.y ) );
    if( (((submat>>10)&7)>6) ) { peld = 1.0; }
    dis += 0.005*peld;
    mindist = dis;
    sid = 0;
    if( peld>0.0000001 ) sid = 2;

    //-----------------------
    // techo
    //-----------------------
    float fx = fract( pos.x+128.0 );
    float fz = fract( pos.z+128.0 );
    if( pos.y>1.0 )
    {
        dis = max(techo(fx,pos.y),techo(fz,pos.y));
        if( dis<mindist )
        {
            mindist = dis;
            sid = 5;
        }
    }
    fx = fract( pos.x+128.0+.5 );
    fz = fract( pos.z+128.0+.5 );

    //-----------------------
    // columnas
    dis = columna( fx-.5, pos.y, fz-.5, mindist, 13.1*floor(pos.x)+17.7*floor(pos.z) );
    if( dis<(mindist*mindist) )
    {
        mindist = sqrt(dis);
        sid = 1;
    }

    //-----------------------
    // bicho
    //-----------------------

    dis = bicho( pos, mindist );
    if( dis<mindist )
    {
        mindist = dis;
        sid = 4;
    }
    //-----------------------

    return mindist;
}

vec3 calcNormal( in vec3 pos )
{
    float eps = 0.0002;
    vec3 nor;
    int kk, kk2;
    nor.x = map( vec3(pos.x+eps, pos.y, pos.z), kk, kk2 ) - map( vec3(pos.x-eps, pos.y, pos.z), kk, kk2 );
    nor.y = map( vec3(pos.x, pos.y+eps, pos.z), kk, kk2 ) - map( vec3(pos.x, pos.y-eps, pos.z), kk, kk2 );
    nor.z = map( vec3(pos.x, pos.y, pos.z+eps), kk, kk2 ) - map( vec3(pos.x, pos.y, pos.z-eps), kk, kk2 );
    return normalize( nor );
}



void main(void)
{
    vec2 pixel = -1.0 + 2.0 * gl_FragCoord.xy / resolution.xy;
    float an = time*0.15;
    vec2 sc = vec2(cos(an),sin(an));

    float r2 = pixel.x*pixel.x*0.32 + pixel.y*pixel.y;
    float tt = (7.0-sqrt(37.5-11.5*r2))/(r2+1.0);
    pixel *= tt;

    float asp = resolution.x/resolution.y;
    vec3 rd = normalize(vec3(asp*pixel.x*sc.x-sc.y,pixel.y,sc.x+asp*pixel.x*sc.y));
    vec3 ro = vec3(0.5+1.4*sc.y,0.5,1.5-1.4*sc.x);

    float t;
    int matID=666;
    int subMatID;
    vec3 pos;
    vec3 rgb = vec3(0.0);

    // cast ray
    #if 1
    for( t=0.5; t<12.0; )
    {
        pos = ro + t*rd;
        float h = map( pos, matID, subMatID );
        if( h<0.01 ) break;
        t += h;
        //if( t>30.0 ) break;
    }
    #else
    t = 0.5;
    for( int i=0; i<50; i++ )
    {
        pos = ro + t*rd;
        float h = map( pos, matID, sumMatID );
        if( h<0.001 ) break;
        t += h;
        //if( t>30.0 ) break;
    }
    #endif

    // shade
    if( matID!=666 )
    {
        // calc normal
        vec3 nor = calcNormal(pos);


        // bump mapping
        float kke=0.0001;
        float bumpa=0.0075;
        if( matID!=5 ) bumpa*=0.75;
        if( matID==4 ) bumpa*=0.50;
        bumpa /= kke;
        float kk = fbm(32.0*pos);
        nor.x += bumpa*(fbm(32.0*vec3(pos.x+kke, pos.y, pos.z))-kk);
        nor.y += bumpa*(fbm(32.0*vec3(pos.x, pos.y+kke, pos.z))-kk);
        nor.z += bumpa*(fbm(32.0*vec3(pos.x, pos.y, pos.z+kke))-kk);
        nor = normalize(nor);

        // light
        float spe = 0.0;
        vec3 lig = vec3( 0.5-pos.x, 0.8-pos.y, 1.5-pos.z );
        float llig = dot(lig,lig);
        float im = inversesqrt(llig);
        lig = lig * im;
        float dif = dot(nor,lig);
        if( matID==4 )
            {dif=0.5+0.5*dif;}
        else
            {dif=0.1+0.9*dif;}
        //if( dif<0.0 ) dif=0.0;
    	//dif=max(dif,0.0);
    	dif = clamp(dif,0.0,1.0);
    	dif *= 2.5*exp2(-1.75*llig);
    	float dif2=(nor[0]+nor[1])*0.075;


        // materials
        if( matID==0 )
        {
            float xoff = 13.1*float(subMatID&255);
            float fb = fbm(16.0*vec3(pos.x+xoff,pos.y,pos.z));
            rgb = vec3(0.7) + fb*vec3(0.20,0.22,0.25);

            float baldscale = float((subMatID>>9)&15)/14.0;
            baldscale = 0.51 + 0.34*baldscale;
            rgb *= baldscale;
            float fx = 1.0;
            if( (subMatID&256)!=0 ) fx = -1.0;
            float m = sin( 64.0*pos.z*fx +  64.0*pos.x + 4.0*fb );
            m = smoothstep( 0.25, 0.5, m ) - smoothstep( 0.5, 0.75, m );
            rgb += m*vec3(0.15);
        }
        else if( matID==2 )
        {
            rgb = vec3(0.0);
        }
        else if( matID==1 )
        {
            float fb = fbm( 16.0*pos );
            float m = sin( 64.0*pos.z +  64.0*pos.x + 4.0*fb );
            m = smoothstep( 0.30, 0.5, m ) - smoothstep( 0.5, 0.70, m );
            rgb = vec3(0.59) + fb*vec3(0.17,0.18,0.21) + m*vec3(0.15) + vec3(dif2);
        }
        else if( matID==4 )
        {
            float ft = fbm( 16.0*pos );
            rgb = vec3(0.82,0.73,0.65) + ft*vec3(0.1);

            float fs = 0.9+0.1*fbm( 32.0*pos );
            rgb *= fs;

            float fre = max( -dot( nor, rd ), 0.0);
            rgb -= vec3(fre*fre*0.45);
            spe = clamp((nor.y-nor.z)*0.707,0.0,1.0);
            spe = 0.20*pow(spe,32.0);
        }
        // techo
        else //if( matID==5 )
        {
            float fb = fbm( 16.0*pos );
            rgb = vec3(0.64,0.61,0.59) + fb*vec3(0.21,0.19,0.19) + dif2;
        }

        // AO
        float ao;
        float totao = 0.0;
        float sca = 10.0;
        for( int aoi=0; aoi<5; aoi++ )
        {
            float hr = 0.01 + 0.015*float(aoi*aoi);
            vec3 aopos =  nor * hr + pos;
            int kk, kk2;
            float dd = map( aopos, kk, kk2 );
            ao = -(dd-hr);
            totao += ao*sca;
            sca *= 0.5;
        }
        ao = 1.0 - clamp( totao, 0.0, 1.0 );

        // shadow
        float so = 0.0;
        for( int i=0; i<6; i++ )
        {
            float h = float(i)/6.0;
            float hr = 0.01 + h;
            vec3 aopos = lig * hr + pos;
            int kk, kk2;
            float dd = map( aopos, kk, kk2 );
            so += (1.0-h)*dd*2.0 * (10.0/6.0);
        }
        dif *= clamp( (so-0.40)*1.5, 0.0, 1.0 );

        // lighting
        rgb = vec3(spe) + rgb * (ao*vec3(0.25,0.30,0.35) + dif*vec3(1.95,1.65,1.05));
        // fog
        rgb = rgb * exp2( -0.4*t );
    }

    // color correct
    rgb =(sqrt(rgb)*0.7+0.3*rgb)*vec3(0.83,1.0,0.83)*1.2;

    // vigneting
    rgb *= 0.25+0.75*clamp(0.60*abs(pixel.x-1.0)*abs(pixel.x+1.0),0.0,1.0);

    gl_FragColor=vec4(rgb,1.0);
}
