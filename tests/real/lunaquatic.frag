// Lunaquatic
// This source is released exclusively for ShaderToy by rgba, for educational purposes only.
// Feel free to be inspired by this code and play around with it. If you make a production with the help
// of this code, it would be polite to greet our group in the intro or in the NFO file.
// Have fun! - xTr1m / BluFlame

#extension GL_EXT_gpu_shader4: enable

// .xy = pixel position
// .z  = time
vec4 Y;
uniform vec2 resolution;
uniform float time;


// All data of our world
vec4 artifactPos;
vec3 lightPos, lightDir, ro, rd;
float FAR, EXPLOSIONTIME, pi, eps=0.0001;

float saturate(float x) { return clamp(x,0.0,1.0); }
float ftime(float t, float s, float e) { return (t-s)/(e-s); }

vec3 rotateY(vec3 v, float x)
{
    return vec3(
        cos(x)*v.x - sin(x)*v.z,
        v.y,
        sin(x)*v.x + cos(x)*v.z
    );
}

vec3 rotateX(vec3 v, float x)
{
    return vec3(
        v.x,
        v.y*cos(x) - v.z*sin(x),
        v.y*sin(x) + v.z*cos(x)
    );
}

// Pseudo random number base generator (credits go to iq/rgba)
float rnd(vec2 x)
{
    int n = int(x.x * 40.0 + x.y * 6400.0);
    n = (n << 13) ^ n;
    return 1.0 - float( (n * (n * n * 15731 + 789221) + 1376312589) & 0x7fffffff) / 1073741824.0;
}

// Convert the cipher range from [-1,1] to [0,1]
float norm(float x)
{
    return x * 0.5 + 0.5;
}

// Generate animated (t) caustic values
float caustic(float u, float v, float t)
{
    return pow((
    norm(sin(pi * 2.0 * (u + v + Y.z*t))) +
    norm(sin(pi       * (v - u - Y.z*t))) +
    norm(sin(pi       * (v     + Y.z*t))) +
    norm(sin(pi * 3.0 * (u     - Y.z*t)))) * 0.3, 2.0);
}

// Generate cubic interpolated random values
float smoothrnd(vec2 x)
{
    x = mod(x,1000.0);
    vec2 a = fract(x);
    x -= a;
    vec2 u = a*a*(3.0-2.0*a);
    return mix(
    mix(rnd(x+vec2(0.0)),rnd(x+vec2(1.0,0.0)), u.x),
    mix(rnd(x+vec2(0.0,1.0)),rnd(x+vec2(1.0)), u.x), u.y);
}

float height(vec2 x)
{
    float maxV = Y.z - EXPLOSIONTIME;
    float l = mix(1., max(0., artifactPos.w - artifactPos.y), 1.0 - ((maxV > 0.0 && length(x - artifactPos.xz) < maxV) ? 1.0 : 0.0)) /
            pow(1./max(0., 1.0-length(artifactPos.xz-x)*0.8), 2.0);
    x += length(x-ro.xy);
    x *= min(length(x-ro.xy)*5.0, 4.0);

    return    caustic(x.x+Y.z*0.75, x.y*0.5, 0.3) * 0.006 +
            caustic(x.x*0.1+Y.z*0.2, x.y*0.1, 0.02) * 0.125 -
            0.15 - l*2.0;
}

// Calculates the water "waves". To reduce the bumpiness, increment the y-axis
vec3 getWaterNormal(vec3 p)
{
    return normalize(vec3(
        caustic(p.x * 160.0 - 12.0 * cos(10.0 * p.z), p.z * 140.0, 4.0),
        8.0,
        caustic(p.z * 160.0 - 12.0 * sin(10.0 * p.x), p.x * 140.0, 4.0)) * 2.0 - 1.0);
}

// Raymarch the terrain function, returns the distance from the ray origin to the terrain voxel
// This function was originally adopted from an implementation by iq/rgba
int traceTerrain(vec3 ro, vec3 rd, float maxt, out float depth)
{
    float lh, ly, delt=0.0;
    // advance our sample position from our nearplane to our farplane
    for (float t = 0.1; t < maxt; t += delt)
    {
        // advance our ray
        ro += rd * delt;

        // get the height at the given sample 2d (!) position (we could enhance this by sampling a voxel and returning only the distance to the voxel)
        depth = height(ro.xz);

        if (ro.y <= depth)
        {
            // we need to know our improved (more accuracy here) real terrainposition and the old sampleposition
            // also we precalculate the traveled ray distance (its not a ray anymore if we use stuff like refraction, eg but hey lets stick to this word)
            depth = t - delt + delt*(lh-ly)/(ro.y-depth+lh-ly);
            return 1;
        }

        // store our last height and last sampleposition on the y-axis
        // we need this to calculate the improved terrainposition which will give us a smoother transition between our samplesteps (rd*delt)
        lh = depth;
        ly = ro.y;

        // advance our steplength the more we travel the bigger our stepsize should be
        // with this we are able to sample finer details near to our camera
        delt = 0.002 + (t/(40.0 * clamp(rd.y+1.0,0.0,1.0))); //detail level
    }

    // we hit nothing
    return 0;
}


vec3 calculateSkySub(vec3 rd)
{
    return  norm(smoothrnd(abs(rd.xy*rd.z+rd.y*2.0))) * vec3(0.15) +
            norm(smoothrnd(1.5*abs(rd.xy*rd.z+rd.y+10.0)))* vec3(0.15) +
            norm(smoothrnd(2.5*abs(rd.xy*rd.z+rd.y+20.0)))* vec3(0.15);
}


vec4 calcPlanet(vec3 ro, vec3 rd)
{
    vec4 color = vec4(0.0);

    vec3 planetPos = vec3(70.0, 20.0, 100.0);
    float dist = dot(rd, normalize(planetPos-ro))-0.95;
    if (dist>0.0)
    {
        dist = length(planetPos-ro)-dist*800.0;

        vec3 p = ro+rd*dist;
        vec3 n = normalize(planetPos-p);
        vec2 uv = 0.5 + 0.5 * vec2(atan(n.z, n.x), acos(n.y)) / pi * vec2(5.0, 50.0);
        color.rgb = max(0., 0.2+dot(normalize(p-lightPos), n)) *
                    (caustic(uv.x*0.5+Y.z*0.1, uv.y*0.5,0.)+0.5)*.15 * vec3(1.0,0.0,1.0);
        color.a = 1.0;
    }
    else dist = FAR*99.0;

    // hit with plane
    vec3 pN = vec3(-0.96,0.96,-0.2);
    float t = dot(pN, planetPos-ro) / dot(pN, rd);
    if (t > 0.0 && t < dist)
    {
        float d = length(planetPos - (ro+rd*t));
        if (d > 52.0 && d < 80.0)
            color.rgb = mix(color.rgb, vec3(0.8, 0.64, 0.4), t / 200.0 * norm(sin((d-50.0)/30.0 * smoothrnd(vec2(d, 3.0)))));
        color.a = color.a < 1.0 ? 3.0 * length(color) : color.a;
    }

    return vec4(max(vec3(0.0), color.xyz*0.3) * clamp(dot(rd, vec3(0.0,1.0,0.0))*8.0, 0., 1.0), color.a);
}

vec3 calculateSky(vec3 ro, vec3 rd, int addPlanet)
{
    // atmospheric scattering+sun
    vec3 color = max(vec3(0.0), (max(vec3(0.0), pow(dot(lightDir, rd), 6.0)) * .7 - rd.y) * mix(vec3(1.0,0.5,0.0), vec3(1.0), min(1.0, lightDir.y*1.5)) + lightDir.y * 3.0);

    float phi = atan(rd.x, rd.z);
    float theta = acos(rd.y / length(rd));
    float coeff = smoothstep(0.0, 0.5, norm(0.5 * smoothrnd(300.0 * vec2(phi, theta))) + norm(0.75 * smoothrnd(500.0 * vec2(phi, theta))) - 1.25) * saturate(1.0-lightDir.y*5.0);
    if (addPlanet>0)
    {
        // a planet
        vec4 p = calcPlanet(ro, rd);
        color += coeff*saturate(1.0-p.a) + p.rgb;
    }

    // the clouds
    rd.xy += ro.xy*eps;
    color += (calculateSkySub(normalize(rd + vec3(sin(Y.z*0.1),0.0,cos(Y.z*0.1)) * 0.1)*3.0) +
              calculateSkySub(normalize(rd + vec3(sin(Y.z*0.1),0.0,cos(Y.z*0.1)) * 0.2)*5.0)*0.1 +
              calculateSkySub(normalize(rd + vec3(sin(Y.z*0.1),0.0,cos(Y.z*0.1)) * 0.4)*7.0)*0.1 -
              calculateSkySub(normalize(rd + vec3(sin(Y.z*0.2),0.0,0) * 0.5))*1.5) * saturate(rd.y+0.5);

    return color;
}

float isoSurface(vec3 p)
{
    float b = Y.z>80.0&&Y.z<112.0?1.0:0.0;
    p = rotateX(rotateY(rotateX(rotateY(p - artifactPos.xyz, 3.0*Y.z), 3.0*Y.z), b*sin(3.0*Y.z+3.0*p.y)), b*sin(3.0*Y.z+3.0*p.x));
    p *= 4.0 + 10.0 * max(0., Y.z - EXPLOSIONTIME);

    return -0.4 +
        p.x*p.x*p.x*p.x*p.x*p.x*p.x*p.x +
        p.y*p.y*p.y*p.y*p.y*p.y*p.y*p.y +
        p.z*p.z*p.z*p.z*p.z*p.z*p.z*p.z;

}

float traceIso(vec3 ro, vec3 rd, float mint, float maxt, float s)
{
    float lt, liso, exact, delt = (maxt-mint)/s;

    for (float t = mint; t < maxt; t += delt)
    {
        vec3 p = ro + t * rd;
        float iso = isoSurface(p);
        if (iso <= 0.0)
        {
            for(int i = 0; i < 9; i++)
            {
                exact = (lt + t) / 2.0;
                if (isoSurface(ro + exact * rd) < 0.0) t = exact;
                else lt = exact;
            }
            return exact;
        }

        lt = t;
        liso = iso;
    }

    return FAR;
}

void calcBurn(vec2 x, vec3 normal, inout vec3 color)
{
    float gd = length(x - artifactPos.xz);
    float maxV = Y.z - EXPLOSIONTIME;
    if (maxV > 0.0 && gd < maxV)
    {
        float minV = maxV*0.9;
        if (gd < maxV-(maxV-minV))
        {
            float strength = saturate((gd-minV) / ((maxV-(maxV-minV)*2.0)-minV));
            color *= (1.0-strength*1.5);
            color += (1.0-strength*0.8) *
                     pow(norm(normal.x) + norm(normal.y),
                         2.0*norm(smoothrnd(0.4*Y.z+x*20.0))*
                             norm(smoothrnd(10.0   +x*5.0 ))+
                           1.
                        ) * vec3(1.5, 0.75, 0.5);
        }
        if(gd > maxV-(maxV-minV)*2.0)
            color += cos( (gd-minV) / (maxV-minV) * pi*0.5 ) * vec3(1.5, 0.75, 0.5) ;
    }
}


vec3 calcScene(vec3 ro, vec3 rd)
{
    float upperPlane = (0.1-ro.y) / rd.y;
    float finalDepth = 200.0;
    vec3 color = calculateSky(ro, rd, 1);
    if (rd.y < -0.01 && traceTerrain(ro+rd*upperPlane, rd, finalDepth, finalDepth)>0) // prevent endless stuff and other funny shit
    {
        finalDepth += upperPlane;
        vec3 pos = ro+rd*finalDepth;
        vec3 normal = normalize(normalize(
                                        vec3(
                                            height(pos.xz - vec2(eps, 0.0)) - height(pos.xz + vec2(eps, 0.0)),
                                            eps*2.0,
                                            height(pos.xz - vec2(0.0, eps)) - height(pos.xz + vec2(0.0, eps))
                                            )
                                        ) +
                                        (getWaterNormal(pos*0.2)*1.5+getWaterNormal(pos*0.1)) * max(0., 1.0-finalDepth/FAR*7.0)
                            );

        color = max(0., dot(normal, lightDir)    // diffuse
                + pow(max(0., dot(normal,  normalize(lightDir-rd))), 2.0) // specular
                ) *
                calculateSky(pos, reflect(rd, normal), 1);

        // burn
        calcBurn(pos.xz, normal, color);

        // depth fog
        color = mix(    color,
                        calculateSky(ro, rd, 0),
                        saturate(finalDepth/FAR*1.6 + saturate(dot(normalize(normal+rd*.2), rd)))
                    ); // sky refl and fog
    }

    return color;
}

void main()
{
    FAR = 9.0;
    EXPLOSIONTIME = 127.0;
    pi = 3.1416;


    Y.xy = -1.0 + 2.0 * gl_FragCoord.xy / resolution.xy;
    Y.z = time;

    rd = normalize(vec3(Y.xy - 0.5, 1.0));
    artifactPos = vec4(10.0, norm(sin(Y.z*1.0+4.0)), 13.0, 0.0);

    ro = vec3(.0, .25, 0.);
    float t = Y.z;
    if (t < 16.0) // 1 intro
    {
        t = ftime(t,0.0,16.0);
        t = 1.0-pow(1.0-t, 2.);
        lightPos = vec3(-400.0, 100.0 - t*450.0, 1000.0);

        t=(t-1.0)*0.7;
        rd = rotateX(rd, -t);
    }
    else if (t < 32.0) // 1 intro
    {
        t=ftime(t,16.0,32.0);
        t = 1.0-pow(1.0-t, 2.);
        lightPos = vec3(-400.0, -350.0+t*600.0, 1000.0);
        ro.y -= t*0.1;
    }
    else if (t < 40.0) // 2. wonderful flyby over the scenery
    {
        lightPos = vec3(-400.0, 250.0, 1000.0);
        ro.y -= 0.1;
    }
    else if (t < 64.0) //3. approaching the artifact
    {
        // Determine the scene we're in
        int scene = int((t-40.0)/6.0);
        if (scene >= 3)
            artifactPos.w = 1.;
        lightPos = vec3(-400.0, min(250., norm(rnd(vec2(scene, 2))) * 400.0), 1000.0);

        // Get a random initial position for our camera
        ro.xz += vec2(0.1, 20.0) * vec2(rnd(vec2(scene, 5.0)), rnd(vec2(scene, 7.0)));

        // Basing on the initial position, choose some "random" start and end points nearby
        ro = mix(
                ro+vec3(0.008)*abs(vec3(rnd(vec2(scene, 8.0)), 0.0, rnd(vec2(scene, 10.0)))),
                ro+vec3(.75)  *abs(vec3(rnd(vec2(scene, 11.0)), 0.0, rnd(vec2(scene, 13.0)))),
                t-40.0-float(scene));
        ro.y = 0.2;
        rd = rotateY(rd, rnd(vec2(scene, 14.0))*pi);
    }
    else // 4. artifact is in the middle and 5. BOOM
    {
        artifactPos.w = 1.0;
        lightPos = vec3(-400.0, norm(sin(t+10.0)) * 250.0, 1000.0);
        float i = smoothstep(118.,120.,t);
        ro.xz = artifactPos.xz +
                mix(vec2(sin(t*0.6), cos(t*0.6))*mix(sin(t*1.5+10.0)+2.0,2.,i),
                    vec2(0.0,2.0+0.75*(t - EXPLOSIONTIME)),
                    smoothstep(-1.0, 1.0, t - EXPLOSIONTIME)
                    );
        ro.y = mix(norm(sin(t*3.0)*(1.0-i))*0.3,
                   0.0,
                   smoothstep(-1.0, 1.0, t - EXPLOSIONTIME)
                   ) + 0.2;
        vec2 dir = normalize(artifactPos.xz-ro.xz);
        rd = rotateY(rd, atan(dir.y, dir.x)-pi/2.0);
    }

    lightDir = normalize(lightPos-ro);

    float dist = length(ro-artifactPos.xyz)-1.0;
    float isoDistance = artifactPos.w==1.0?traceIso(ro, rd, dist, dist+2.0, 99.0):FAR;
    if (isoDistance < FAR)
    {
        ro += isoDistance * rd;
        vec3 n = normalize(vec3(
                isoSurface(vec3(ro.x-eps, ro.y, ro.z))-isoSurface(vec3(ro.x+eps, ro.y, ro.z)),
                isoSurface(vec3(ro.x, ro.y-eps, ro.z))-isoSurface(vec3(ro.x, ro.y+eps, ro.z)),
                isoSurface(vec3(ro.x, ro.y, ro.z-eps))-isoSurface(vec3(ro.x, ro.y, ro.z+eps))));
        rd = reflect(rd, n);
    }

    gl_FragColor = vec4(calcScene(ro, rd) * saturate(sin((Y.z/150.0)*pi)*10.0),1.0);
}
