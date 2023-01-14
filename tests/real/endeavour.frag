/*
 * Copied from https://www.shadertoy.com/view/3d2XWt
 * Endeavor by Team210 - 64k intro by Team210 at Revision 2k19
 * Copyright (C) 2019  Alexander Kraus <nr4@z10.info>
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

// Global constants
const float pi = acos(-1.);
const vec3 c = vec3(1.0, 0.0, -1.0);
float a = 1.0;

// Hash function
void rand(in vec2 x, out float num)
{
    x += 400.;
    num = fract(sin(dot(sign(x)*abs(x) ,vec2(12.9898,78.233)))*43758.5453);
}

// Arbitrary-frequency 2D noise
void lfnoise(in vec2 t, out float num)
{
    vec2 i = floor(t);
    t = fract(t);
    //t = ((6.*t-15.)*t+10.)*t*t*t;  // TODO: add this for slower perlin noise
    t = smoothstep(c.yy, c.xx, t); // TODO: add this for faster value noise
    vec2 v1, v2;
    rand(i, v1.x);
    rand(i+c.xy, v1.y);
    rand(i+c.yx, v2.x);
    rand(i+c.xx, v2.y);
    v1 = c.zz+2.*mix(v1, v2, t.y);
    num = mix(v1.x, v1.y, t.x);
}

// Multi-frequency 2D noise
void mfnoise(in vec2 x, in float fmin, in float fmax, in float alpha, out float num)
{
    num = 0.;
    float a = 1., nf = 0., buf;
    for(float f = fmin; f<fmax; f = f*2.)
    {
        lfnoise(f*x, buf);
        num += a*buf;
        a *= alpha;
        nf += 1.;
    }
    num *= (1.-alpha)/(1.-pow(alpha, nf));
}

// Stroke
void stroke(in float d0, in float s, out float d)
{
    d = abs(d0)-s;
}

// Extrusion
void zextrude(in float z, in float d2d, in float h, out float d)
{
    vec2 w = vec2(-d2d, abs(z)-0.5*h);
    d = length(max(w,0.0));
}

// Add distance functions with materials
void add(in vec2 sda, in vec2 sdb, out vec2 dst)
{
    dst = mix(sda, sdb, step(sdb.x, sda.x));
}

// Distance to hexagon pattern
// Taken from iqs 'Hexagons - Distance' - see https://www.shadertoy.com/view/Xd2GR3
// Created by inigo quilez - iq/2014
// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.
void dhexagonpattern(in vec2 p, out float d, out vec2 ind) 
{
    vec2 q = vec2( p.x*1.2, p.y + p.x*0.6 );
    
    vec2 pi = floor(q);
    vec2 pf = fract(q);

    float v = mod(pi.x + pi.y, 3.0);

    float ca = step(1.,v);
    float cb = step(2.,v);
    vec2  ma = step(pf.xy,pf.yx);
    
    d = dot( ma, 1.0-pf.yx + ca*(pf.x+pf.y-1.0) + cb*(pf.yx-2.0*pf.xy) );
    ind = pi + ca - cb*ma;
    ind = vec2(ind.x/1.2, ind.y);
    ind = vec2(ind.x, ind.y-ind.x*.6);
}
// End of iqs 'Hexagon - Distance'

void scene(in vec3 x, out vec2 sdf)
{
    sdf.y = 1.;
    mfnoise(x.xy, 1., 400., .45, sdf.x);
    stroke(sdf.x, .5, sdf.x);
    sdf.x = x.z+.05*smoothstep(.35,.45,.5+.5*sdf.x)+.1*sdf.x;
    
    if(x.z>-.05)
    {
        float n2;
        mfnoise(x.xy, 30., 500., .47, n2);
        vec2 sda = vec2(x.z+.05+.01*n2, 1.);
        add(sdf, sda, sdf);
    }
    
    float R = .07+.1*sdf.x, dis;
    lfnoise(.5*x.y*c.xx, dis);
    vec2 sdb;
    vec2 ya = abs(x.xz-.4*dis*c.xy)+.045*c.yx - .065*c.xy;
    zextrude(x.y, -length(ya)+R, 1.e4, sdb.x);
    float da;
    stroke(sdb.x, .003, da);
    sdb.y = 2.;
    
    vec2 ind;
    float phi = atan(ya.y, ya.x);
    dhexagonpattern(vec2(56.,12.)*vec2(x.y, phi), dis, ind);
    stroke(dis, .2, dis);
    stroke(sdb.x, .003*step(dis,0.), sdb.x);
	sdf.x = max(sdf.x,-da);
    add(sdf, sdb, sdf);
    
    // Add guard objects for debugging
    float dr = .1;
    vec3 y = mod(x,dr)-.5*dr;
    float guard = -length(max(abs(y)-vec3(.5*dr*c.xx, .6),0.));
    guard = abs(guard)+dr*.1;
    sdf.x = min(sdf.x, guard);
}

void normal(in vec3 x, out vec3 n)
{
    const float dx = 5.e-4;
    vec2 s, na;
    
    scene(x,s);
    scene(x+dx*c.xyy, na);
    n.x = na.x;
    scene(x+dx*c.yxy, na);
    n.y = na.x;
    scene(x+dx*c.yyx, na);
    n.z = na.x;
    n = normalize(n-s.x);
}

void planet_texture(in vec2 x, out vec3 col)
{
    vec3 light_orange = vec3(1.00,0.69,0.05),
        orange = vec3(0.95,0.45,0.01),
        dark_orange = vec3(0.98,0.73,0.01);
    
    //rock like appearance
    float d;
    mfnoise(x, 1.,400.,.65,d);
	col = mix(vec3(0.19,0.02,0.00), vec3(0.91,0.45,0.02), .5+.5*d);
    
    // big structures
    stroke(d,.01, d);
    col = mix(mix(.8*vec3(0.99,0.49,0.02),c.yyy,d*clamp(.2-.5*x.y/12.,0.,1.)), col, smoothstep(-.05,.05,d));
    col = mix(col, vec3(0.15,0.05,0.00), clamp(.2-.5*x.y/12.,0.,1.));
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    a = iResolution.x/iResolution.y;
    vec2 uv = fragCoord/iResolution.yy-0.5*vec2(a, 1.0),
        s;
    vec3 col = c.yyy, 
        o = .5*c.yyx + .3*iTime*c.yxy, 
        t = vec3(uv,0.) + .3*iTime*c.yxy + c.yxy,
        dir = normalize(t-o),
        x;
    float d = (.04-o.z)/dir.z;
    
    int N = 450, i;
    for(i=0; i<N; ++i)
    {
        x = o + d * dir;
        scene(x,s);
        if(s.x < 1.e-4) break;
        d += s.x;
    }
    
    if(i<N)
    {
        vec3 n,
            l = normalize(x+c.yyx);
        normal(x,n);
        vec3 c1 = vec3(0.99,0.64,0.02);
        if(s.y == 1.)
        {
            planet_texture(x.xy, c1);
            col = .3*c1
                + (.3*c1)*abs(dot(l,n))
                + (1.3*c1+.1*c.xyy)*pow(abs(dot(reflect(-l,n),dir)),3.);
            
            // fake pipe shadow
            if(x.z < .0)
            {
                float R = .07, dis;
                lfnoise(.5*x.y*c.xx, dis);
                float ya = abs(x.x-.4*dis) - .15;
                col = mix(col, .3*col, smoothstep(-.01,.01,-ya));
            }
        }
        else if(s.y == 2.)
        {
            planet_texture(x.xy, c1);
            col = .3*c1
                + (.3*c1)*abs(dot(l,n))
                + (1.3*c1+.1*c.xyy)*pow(abs(dot(reflect(-l,n),dir)),3.);
            col = mix(col,.4*length(col)*c.xyy,.7);
        }
    }

    col = clamp(col, 0.,1.);
    
    fragColor = vec4(col,1.0);
}