// The Sheep and the Biker, by Ctrl-Alt-Test
// Source: https://github.com/ctrl-alt-test/moutard/
// Apache-2.0 license

#version 150

const int MAX_RAY_MARCH_STEPS = 250;
const float MAX_RAY_MARCH_DIST = 500.;
const float INF = 1e6;
// Include begin: shared.h
// --------------------------------------------------------------------

const int XRES = 1280;
const int YRES = 720;

// --------------------------------------------------------------------
// Include end: shared.h


const float lampHeight = 7.;


uniform float iTime;

const int SCENE_SHEEP = 0;
const int SCENE_MOTO = 1;
const int SCENE_BLOOD = 2;
const int SCENE_MOUTARD = 3;
int sceneID = 0;

float camProjectionRatio = 1.;
float wheelie = 0.;
float globalFade = 1.;
float shouldDrawLogo = 0.;
float blink = 0.;
float squintEyes = 0.;
float sheepTears = -1.;
float headDist = 0.; 
float sheepPos = INF;
float lightFalloff = 10000.;
float pupilSize = 0.1;

vec2 headRot = vec2(0., -0.4);

vec3 eyeDir = vec3(0.,-0.2,1.);
vec3 animationSpeed = vec3(1.5);
vec3 camPos;
vec3 camTa;
vec3 panelWarningPos = vec3(6., 0., 0.);
vec3 motoPos;
vec3 headLightOffsetFromMotoRoot = vec3(0.53, 0.98, 0.0);
vec3 breakLightOffsetFromMotoRoot = vec3(-0.8, 0.75, 0.0);



out vec4 fragColor;

// Include begin: common.frag
// --------------------------------------------------------------------

float hash11(float x) { return fract(sin(x) * 43758.5453); }
float hash21(vec2 xy) { return fract(sin(dot(xy, vec2(12.9898, 78.233))) * 43758.5453); }
float hash31(vec3 xyz) { return hash21(vec2(hash21(xyz.xy), xyz.z)); }
vec2 hash22(vec2 xy) { return fract(sin(vec2(dot(xy, vec2(127.1,311.7)), dot(xy, vec2(269.5,183.3)))) * 43758.5453); }
vec2 hash12(float x) { float h = hash11(x); return vec2(h, hash11(h)); }


float noise(vec3 x) {

    vec3 i = floor(x);
    vec3 f = fract(x);
    f = f*f*f*(f*(f*6.0-15.0)+10.0);
    return mix(mix(mix( hash31(i+vec3(0,0,0)), 
                        hash31(i+vec3(1,0,0)),f.x),
                   mix( hash31(i+vec3(0,1,0)), 
                        hash31(i+vec3(1,1,0)),f.x),f.y),
               mix(mix( hash31(i+vec3(0,0,1)), 
                        hash31(i+vec3(1,0,1)),f.x),
                   mix( hash31(i+vec3(0,1,1)), 
                        hash31(i+vec3(1,1,1)),f.x),f.y),f.z)*2.-1.;
}


vec2 valueNoise2(float p)
{
    float p0 = floor(p);
    float p1 = p0 + 1.;

    vec2 v0 = hash12(p0);
    vec2 v1 = hash12(p1);

    float fp = p - p0;
    fp = fp*fp * (3.0 - 2.0 * fp);

    return mix(v0, v1, fp);
}




float smin(float d1, float d2, float k)
{
    float h = clamp( 0.5 + 0.5*(d2-d1)/k, 0.0, 1.0 );
    return mix( d2, d1, h ) - k*h*(1.0-h);
}

float cappedCone(vec3 p, float h, float r1, float r2)
{
  vec2 q = vec2( length(p.xz), p.y );
  vec2 k1 = vec2(r2,h);
  vec2 k2 = vec2(r2-r1,2.0*h);
  vec2 ca = vec2(q.x-min(q.x,(q.y<0.0)?r1:r2), abs(q.y)-h);
  vec2 cb = q - k1 + k2*clamp( dot(k1-q,k2)/dot(k2,k2), 0.0, 1.0 );
  float s = (cb.x<0.0 && ca.y<0.0) ? -1.0 : 1.0;
  return s*sqrt( min(dot(ca,ca),dot(cb,cb)) );
}

float smax(float a, float b, float k)
{
    k *= 1.4;
    float h = max(k-abs(a-b),0.0);
    return max(a, b) + h*h*h/(6.0*k*k);
}

float Box3(vec3 p, vec3 size, float corner)
{
   p = abs(p) - size + corner;
   return length(max(p, 0.)) + min(max(max(p.x, p.y), p.z), 0.) - corner;
}

float Ellipsoid(in vec3 p, in vec3 r)
{
    float k0 = length(p / r);
    float k1 = length(p / (r*r));
    return k0 * (k0-1.0) / k1;
}

float Segment3(vec3 p, vec3 a, vec3 b, out float h)
{
	vec3 ap = p - a;
	vec3 ab = b - a;
	h = clamp(dot(ap, ab) / dot(ab, ab), 0., 1.);
	return length(ap - ab * h);
}


float capsule(vec3 p, vec3 a, vec3 b, float r)
{
  vec3 pa = p - a, ba = b - a;
  return length( pa - ba*clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 ) ) - r;
}

float Capsule(vec3 p, float h, float r)
{
    p.y += clamp(-p.y, 0., h);
    return length(p) - r;
}

float Torus(vec3 p, vec2 t)
{
    return length(vec2(length(p.xz) - t.x,p.y)) - t.y;
}

mat2 Rotation(float angle)
{
    float c = cos(angle);
    float s = sin(angle);
    return mat2(c, s, -s, c);
}

float Triangle(vec3 p, vec2 h, float r)
{
  return max(
        abs(p.z) - h.y,
        smax(smax(p.x*0.9 + p.y*0.5, -p.x*0.9 + p.y*0.5, r),-p.y,r) - h.x*0.5);
}

float UnevenCapsule2d( vec2 p, float r1, float r2, float h )
{
    p.x = abs(p.x);
    float b = (r1-r2)/h;
    float a = sqrt(1.0-b*b);
    float k = dot(p,vec2(-b,a));
    if( k < 0.0 ) return length(p) - r1;
    if( k > a*h ) return length(p-vec2(0.0,h)) - r2;
    return dot(p, vec2(a,b) ) - r1;
}

vec2 MinDist(vec2 d1, vec2 d2)
{
    return d1.x < d2.x ? d1 : d2;
}
// --------------------------------------------------------------------
// Include end: common.frag

// Include begin: ids.frag
// --------------------------------------------------------------------
const int NO_ID = -1;

const int MOTO_ID = 0;
const int MOTO_MOTOR_ID = MOTO_ID;
const int MOTO_BREAK_LIGHT_ID = MOTO_ID;
const int MOTO_HEAD_LIGHT_ID = MOTO_ID;
const int MOTO_DRIVER_HELMET_ID = MOTO_ID;

const int MOTO_WHEEL_ID = 1;
const int MOTO_DRIVER_ID = MOTO_WHEEL_ID;

const int MOTO_EXHAUST_ID = MOTO_ID;

const int TREE_ID = 2;

const int GROUND_ID = 3;

const int WOOL_ID = 4;
const int SKIN_ID = 5;
const int EYE_ID = 6;
const int CLOGS_ID = 7;
const int METAL_ID = 8;
const int BLOOD_ID = 9;
const int PANEL_ID = 10;
const int TEARS_ID = 11;
// --------------------------------------------------------------------
// Include end: ids.frag

// Include begin: backgroundContent.frag
// --------------------------------------------------------------------
// --------------------------------------------------------------------
// Include end: backgroundContent.frag

// Include begin: roadContent.frag
// --------------------------------------------------------------------
const float warningHeight = 3.;

vec2 panelWarning(vec3 p) {
    p -= panelWarningPos;
    float pan = Triangle(p - vec3(0., warningHeight,-5.), vec2(1.7, .1), .3);
    if (pan > 8.) {
        return vec2(INF, GROUND_ID);
    }

    pan = smax(pan, -Triangle(p - vec3(0., warningHeight, -5.1), vec2(1.6,.1), .3), .001);
    
    float tube = Box3(p-vec3(0., 2.,-5.1), vec3(.11, 2., .08), 0.);
    vec3 pp = p;
    pp.y = abs(pp.y - 3.65)-.3;
    tube = min(tube, Box3(pp-vec3(0.,0.,-5.05), vec3(.35,.1,.05), 0.));
    
    vec2 dmat = vec2(tube, METAL_ID);
    return MinDist(dmat, vec2(pan, PANEL_ID));
}

vec2 blood(vec3 p) {
    if (sceneID != SCENE_BLOOD) {
        return vec2(INF, GROUND_ID);
    }
    p -= vec3(0, 1.2, -2.5);

    float d = p.y + smoothstep(1.5,8.,length(p.xz)) + 1.;
    if (d < 0.4) {
        d -= pow((noise(p*.9+0.)*.5+noise(p*1.6)*.3+noise(p*2.7)*.1)*.5+.5, 3.) *.45
             ;
        return vec2(d, BLOOD_ID);
    }
    return vec2(d, GROUND_ID);
}

vec2 terrainShape(vec3 p)
{
    
    float isRoad = 1.0 - smoothstep(3.5, 5., abs(p.x));

    
    float height = mix(
        noise(p*5.)*0.1 + 0.5 * noise(vec3(p.xz,0) * .4),
        0.,
        isRoad*isRoad);

    if (isRoad > 0.0)
    {
        float x = clamp(abs(p.x / 3.5), 0., 1.);
        float roadBumpHeight = 0.2 * (1. - x * x * x);
        height += roadBumpHeight + pow(noise(mod(p*50, 100))*.5+.5, .01) * .1;
    }

    return vec2(p.y - height, GROUND_ID);
}

const float treeSpace = 10.;

float tree(vec3 localP, vec2 id) {
    float h1 = hash21(id);
    float h2 = hash11(h1);
    float terrainHeight = -1.;

    float d = treeSpace * 0.5 * 0.7;

    
    if (abs(id.x) < 14.) return d;

    float treeHeight = mix(7., 20., h1);
    float treeWidth = max(3.5, treeHeight * mix(0.3, 0.4, h2*h2));

    localP.y -= terrainHeight + 0.5 * treeHeight;
    localP.xz += (vec2(h1, h2) - 0.5) * 1.5; 

    d = min(d, Ellipsoid(localP, 0.5*vec3(treeWidth, treeHeight, treeWidth)));

    
    vec2 pNoise = vec2(2.*atan(localP.z, localP.x), localP.y) + id;
    d += 0.2*noise(2. * pNoise.xyy) + 0.5;

    return d;
}

vec2 treesShape(vec3 p)
{
    
    
    vec2 id = round(p.xz / treeSpace) * treeSpace;
    vec3 localP = p;
    localP.xz -= id;
    return vec2(tree(localP, id), TREE_ID);
}
// --------------------------------------------------------------------
// Include end: roadContent.frag

// Include begin: motoContent.frag
// --------------------------------------------------------------------




vec3 motoToWorldForCamera(vec3 v)
{
    v.xz *= Rotation(1.57);
    v += motoPos;
    return v;
}

vec3 motoToWorld(vec3 v, bool isPos)
{
    v.xy *= Rotation(-0.5 * wheelie);
    v.xz *= Rotation(1.57);
    if (isPos)
    {
        v += motoPos;
    }
    return v;
}

vec3 worldToMoto(vec3 v, bool isPos)
{
    if (isPos)
    {
        v -= motoPos;
    }
    v.xz *= Rotation(-1.57);
    v.xy *= Rotation(0.5 * wheelie);
    return v;
}

vec2 driverShape(vec3 p)
{

    if (sceneID >= SCENE_BLOOD) {
        return vec2(INF, MOTO_DRIVER_ID);
    }
    
    float wind = noise((p + iTime) * 12.);
    p = worldToMoto(p, true);
    
    p -= vec3(-0.35, 0.78, 0.0);

    float d = length(p);
    if (d > 1.2)
        return vec2(d, MOTO_DRIVER_ID);

    vec3 simP = p;
    simP.z = abs(simP.z);


    
    if (d < 0.8)
    {
        vec3 pBody = p;
        pBody.z = max(abs(pBody.z)-0.02,0);
        pBody.xy *= Rotation(3.1);
        pBody.yz *= Rotation(-0.1);
        d = smin(d, Capsule(pBody, 0.12, 0.12), 0.4);

        pBody.y += 0.2;
        pBody.xy *= Rotation(-0.6);
        d = smin(d, Capsule(pBody, 0.12, 0.11), 0.08);

        pBody.y += 0.2;
        pBody.xy *= Rotation(-0.3);
        pBody.yz *= Rotation(-0.2);
        d = smin(d, Capsule(pBody, 0.12, 0.12), 0.08);

        pBody.y += 0.1;
        pBody.yz *= Rotation(1.7);
        d = smin(d, Capsule(pBody, 0.12, 0.1), 0.06);

        pBody=p;
        pBody.y-=.48;
        pBody.x-=.25;
        pBody.xy *= Rotation(-.7);
        d = min(d, length(vec2(max(abs(pBody.y)-.07,0),abs(length(pBody.xz)-.05)))-.04);
    }
    
    
    vec3 pArm = simP;

    pArm -= vec3(0.23, 0.45, 0.18);
    pArm.yz *= Rotation(-0.6);
    pArm.xy *= Rotation(0.2);
    float arms = Capsule(pArm, 0.29, 0.06);
    d = smin(d, arms, 0.02);

    pArm.y += 0.32;
    pArm.xy *= Rotation(1.5);
    arms = Capsule(pArm, 0.28, 0.04);
    d = smin(d, arms, 0.02);
    d += 0.005 * wind;

    
    vec3 pLeg = simP;

    pLeg -= vec3(0.0, 0.0, 0.13);
    pLeg.xy *= Rotation(1.55);
    pLeg.yz *= Rotation(-0.4);
    float h2 = Capsule(pLeg, 0.35, 0.09);
    d = smin(d, h2, 0.04);

    pLeg.y += 0.4;
    pLeg.xy *= Rotation(-1.5);
    float legs = Capsule(pLeg, 0.4, 0.06);
    d = smin(d, legs, 0.04);

    pLeg.y += 0.45;
    pLeg.xy *= Rotation(1.75);
    pLeg.yz *= Rotation(0.25);
    float feet = Capsule(pLeg, 0.2, 0.03);
    d = smin(d, feet, 0.02);
    d += 0.002 * wind;

    
    vec3 pHead = p - vec3(0.39, 0.6, 0.0);
    float head = length(pHead*vec3(1.2,1.,1.3-pHead.y)) - 0.15;

    if (head < d) {
        return vec2(head, MOTO_DRIVER_HELMET_ID);
    }

    return vec2(d, MOTO_DRIVER_ID);
}

vec2 wheelShape(vec3 p, float wheelRadius, float tireRadius, vec3 innerPart)
{
    vec2 d = vec2(INF, MOTO_WHEEL_ID);
    float wheel = Torus(p.yzx, vec2(wheelRadius, tireRadius));

    if (wheel < 0.25)
    {
        p.z = abs(p.z);
        float h;
        float cyl = Segment3(p, vec3(0.0), vec3(0.0, 0.0, 1.0), h);
        wheel = -smin(-wheel, cyl - 0.22, 0.04);

         
        
        wheel = min(wheel, -min(min(min(0.15 - cyl, cyl - 0.08), p.z - 0.04), -p.z + 0.05));
         
        wheel = min(wheel, Ellipsoid(p, innerPart));
    }
    return vec2(wheel, MOTO_WHEEL_ID);
}

vec2 motoShape(vec3 p)
{
    p = worldToMoto(p, true);

    float boundingSphere = length(p);
    if (boundingSphere > 2.0)
        return vec2(boundingSphere - 1.5, MOTO_ID);

    vec2 d = vec2(INF, MOTO_ID);

    float h;
    float cyl;

    float frontWheelTireRadius = 0.14/2.0;
    float frontWheelRadius = 0.33 - frontWheelTireRadius;
    float rearWheelTireRadius = 0.3/2.0;
    float rearWheelRadius = 0.32 - rearWheelTireRadius;
    vec3 frontWheelPos = vec3(0.9, frontWheelRadius + frontWheelTireRadius, 0.0);

    
    d = MinDist(d, wheelShape(p - frontWheelPos, frontWheelRadius, frontWheelTireRadius, vec3(0.02, 0.02, 0.12)));

    
    d = MinDist(d, wheelShape(p - vec3(-0.85, rearWheelRadius + rearWheelTireRadius, 0.0), rearWheelRadius, rearWheelTireRadius, vec3(0.2, 0.2, 0.01)));

    
    float forkThickness = 0.025;
    vec3 pFork = p;
    vec3 pForkTop = vec3(-0.48, 0.66, 0.0);
    vec3 pForkAngle = pForkTop + vec3(-0.14, 0.04, 0.05);
    pFork.z = abs(pFork.z);
    pFork -= frontWheelPos + vec3(0.0, 0.0, frontWheelTireRadius + 2. * forkThickness);
    float fork = Segment3(pFork, pForkTop, vec3(0.0), h) - forkThickness;

    
    fork = min(fork, Segment3(pFork, pForkTop, pForkAngle, h) - forkThickness * 0.7);

    
    float handle = Segment3(pFork, pForkAngle, pForkAngle + vec3(-0.08, -0.07, 0.3), h);
    fork = min(fork, handle - mix(0.035, 0.02, smoothstep(0.25, 0.4, h)));

    
    vec3 pMirror = pFork - pForkAngle - vec3(0.0, 0.1, 0.15);
    pMirror.xz *= Rotation(0.2);
    pMirror.xy *= Rotation(-0.2);
    
    float mirror = pMirror.x - 0.02;
    pMirror.xz *= Rotation(0.25);

    mirror = -min(mirror, -Ellipsoid(pMirror, vec3(0.04, 0.05, 0.08)));
    pMirror.x-=.05;
    pMirror.yz *= Rotation(1);
    mirror = min(mirror,max(length(pMirror.xz)-.003,max(pMirror.y,-pMirror.y-.2)));
    fork = min(fork, mirror);

    d = MinDist(d, vec2(fork, MOTO_EXHAUST_ID));

    
    vec3 pHead = p - headLightOffsetFromMotoRoot;
    float headBlock = Ellipsoid(pHead, vec3(0.15, 0.2, 0.15));
    
    if (headBlock < 0.2)
    {
        vec3 pHeadTopBottom = pHead;

        
        pHeadTopBottom.xy *= Rotation(-0.15);
        headBlock = -min(-headBlock, -Ellipsoid(pHeadTopBottom - vec3(-0.2, -0.05, 0.0), vec3(0.35, 0.16, 0.25)));

        
        headBlock = -min(-headBlock, -Ellipsoid(pHead - vec3(-0.2, -0.08, 0.0), vec3(0.35, 0.25, 0.13)));

        
        headBlock = -min(-headBlock, -Ellipsoid(pHead - vec3(-0.1, -0.05, 0.0), vec3(0.2, 0.2, 0.3)));

        
        pHead.xy *= Rotation(-0.4);
        headBlock = -min(-headBlock, -Ellipsoid(pHead - vec3(0.1, 0.0, 0.0), vec3(0.2, 0.3, 0.4)));
    }

    d = MinDist(d, vec2(headBlock, MOTO_HEAD_LIGHT_ID));

    float joint = Box3(p - vec3(0.4, 0.82, 0.0), vec3(0.04, 0.1, 0.08), 0.02);
    d = MinDist(d, vec2(joint, MOTO_MOTOR_ID));

    
    vec3 pTank = p - vec3(0.1, 0.74, 0.0);
    vec3 pTankR = pTank;
    pTankR.xy *= Rotation(0.45);
    pTankR.x += 0.05;
    float tank = Ellipsoid(pTankR, vec3(0.35, 0.2, 0.42));

    if (tank < 0.1)
    {
        
        float i_tankCut = Ellipsoid(pTankR + vec3(0.0, 0.13, 0.0), vec3(0.5, 0.35, 0.22));
        tank = -min(-tank, -i_tankCut);
        

        
        float i_tankCut2 = Ellipsoid(pTank - vec3(0.0, 0.3, 0.0), vec3(0.6, 0.35, 0.4));
        tank = -min(-tank, -i_tankCut2);
        
    }
    d = MinDist(d, vec2(tank, MOTO_EXHAUST_ID));

    
    vec3 pMotor = p - vec3(-0.08, 0.44, 0.0);
    
    
    vec3 pMotorSkewd = pMotor;
    pMotorSkewd.x *= 1. - pMotorSkewd.y * 0.4;
    pMotorSkewd.x += pMotorSkewd.y * 0.1;
    float motorBlock = Box3(pMotorSkewd, vec3(0.44, 0.29, 0.11), 0.02);
    
    if (motorBlock < 0.5)
    {
        
        vec3 pMotor1 = pMotor - vec3(0.27, 0.12, 0.0);
        vec3 pMotor2 = pMotor - vec3(0.00, 0.12, 0.0);
        pMotor1.xy *= Rotation(-0.35);
        pMotor2.xy *= Rotation(0.35);
        motorBlock = min(motorBlock, Box3(pMotor1, vec3(0.1, 0.12, 0.20), 0.04));
        motorBlock = min(motorBlock, Box3(pMotor2, vec3(0.1, 0.12, 0.20), 0.04));

        
        vec3 pGearBox = pMotor - vec3(-0.15, -0.12, -0.125);
        pGearBox.xy *= Rotation(-0.15);
        float gearBox = Segment3(pGearBox, vec3(0.2, 0.0, 0.0), vec3(-0.15, 0.0, 0.0), h);
        gearBox -= mix(0.08, 0.15, h);
        
        pGearBox.x += 0.13;
        float gearBoxCut = -pGearBox.z - 0.05;
        gearBoxCut = min(gearBoxCut, Box3(pGearBox, vec3(0.16, 0.08, 0.1), 0.04));
        gearBox = -min(-gearBox, -gearBoxCut);

        motorBlock = min(motorBlock, gearBox);

        
        vec3 pPedals = pMotor - vec3(0.24, -0.13, 0.0);
        float pedals = Segment3(pPedals, vec3(0.0, 0.0, .4), vec3(0.0, 0.0, -.4), h) - 0.02;
        motorBlock = min(motorBlock, pedals);
    }
    d = MinDist(d, vec2(motorBlock, MOTO_MOTOR_ID));

    
    vec3 pExhaust = p;
    pExhaust -= vec3(0.0, 0.0, rearWheelTireRadius + 0.05);
    float exhaust = Segment3(pExhaust, vec3(0.24, 0.25, 0.0), vec3(-0.7, 0.3, 0.05), h);

    if (exhaust < 0.6)
    {
        exhaust -= mix(0.04, 0.08, mix(h, smoothstep(0.5, 0.7, h), 0.5));
        exhaust = -min(-exhaust, p.x - 0.7 * p.y + 0.9);
        exhaust = min(exhaust, Segment3(pExhaust, vec3(0.24, 0.25, 0.0), vec3(0.32, 0.55, -0.02), h) - 0.04);
        exhaust = min(exhaust, Segment3(pExhaust, vec3(0.22, 0.32, -0.02), vec3(-0.4, 0.37, 0.02), h) - 0.04);
    }
    d = MinDist(d, vec2(exhaust, MOTO_EXHAUST_ID));

    
    vec3 pSeat = p - vec3(-0.44, 0.44, 0.0);
    float seat = Ellipsoid(pSeat, vec3(0.8, 0.4, 0.2));
    float seatRearCut = length(p + vec3(1.05, -0.1, 0.0)) - 0.7;
    seat = max(seat, -seatRearCut);

    if (seat < 0.2)
    {
        vec3 pSaddle = pSeat - vec3(0.35, 0.57, 0.0);
        pSaddle.xy *= Rotation(0.4);
        float seatSaddleCut = Ellipsoid(pSaddle, vec3(0.5, 0.15, 0.6));
        seat = -min(-seat, seatSaddleCut);
        seat = -smin(-seat, seatSaddleCut, 0.08);

        vec3 pSeatBottom = pSeat + vec3(0.0, -0.55, 0.0);
        pSeatBottom.xy *= Rotation(0.5);
        float seatBottomCut = Ellipsoid(pSeatBottom, vec3(0.8, 0.4, 0.4));
        seat = -min(-seat, -seatBottomCut);
    }
    d = MinDist(d, vec2(seat, MOTO_EXHAUST_ID));

    return d;
}
// --------------------------------------------------------------------
// Include end: motoContent.frag

// Include begin: sheep.frag
// --------------------------------------------------------------------
const float animationAmpX = 1.;
const float animationAmpY = .2;
const float animationAmpZ = .25;

float sunglasses(vec3 p) {
  if (sceneID != SCENE_MOUTARD) {
    return INF;
  }
  
  p -= vec3(0, 0.3, -0.9);
  vec3 framePos = p;
  float h;
  float middle = Segment3(p - vec3(0, -0.1, -0.4), vec3(-0.3,0,0), vec3(.3,0,0), h) - 0.04;
  framePos.x = abs(framePos.x) - 0.5;

  float frame = Segment3(framePos, vec3(0.3, 0., -0.), vec3(0.2, -0.1, -0.4), h) - 0.04;
  frame = min(frame, middle);

  
  vec3 lensPos = p - vec3(0., -0.25, -0.4);
  lensPos.x = abs(lensPos.x) - 0.4;
  float lens = length(lensPos * vec3(0.3, 0.4, 1.)) - 0.1;

  float sunglasses = min(frame, lens);
  return sunglasses;
}

vec2 sheep(vec3 p, bool shiftPos) {
    const float SCALE = 0.15;

    if (shiftPos) {
      if (sceneID == SCENE_MOUTARD) { 
        p -= motoPos + vec3(0., 1.2, -0.3);
        p.yz *= Rotation(0.5);

        if (wheelie > 0.) { 
          p.yz *= Rotation(wheelie * 0.4);
          p.yz -= vec2(0.35, 0.2) * wheelie;
        }
      } else {
        p -= vec3(1, .46, sheepPos);
      }
    }
    p /= SCALE;

    float time = mod(iTime, 1.);
    time = smoothstep(0., 1., abs(time * 2. - 1.));
   
    
    float tb = iTime*animationSpeed.x*3.14;
    vec3 bodyMove = vec3(cos(tb),cos(tb*2.)*.1,0.)*.025*animationAmpX;
    float body = length(p*vec3(1.,1.,.825)-vec3(0.,1.5,2.55)-bodyMove)-2.;
    
    if (body >= 3.) {
        return vec2(body * SCALE, WOOL_ID);
    }

    float n = pow(noise((p-bodyMove+vec3(0,0,0.*10.)+vec3(0,0,0.5))*2.)*.5+.5, .75)*2.-1.;
    if (sceneID == SCENE_MOUTARD) {
      n += noise(p-bodyMove+vec3(0,0,-iTime*10.)*2.)*.2;
    }
    body = body + .05 - n*.2;

    
    float t = mod(iTime*animationSpeed.x,2.);
    float l = smoothstep(0.,.5,t) * smoothstep(1.,.5,t);
    float a = smoothstep(0.,.5,t);
    float b = smoothstep(.5,1.,t);
    float c = smoothstep(1.,1.5,t);
    float d = smoothstep(1.5,2.,t);
    vec4 legsRot = vec4(b * (1.-b), d * (1.-d), a * (1.-a), c * (1.-c));
      
    vec4 legsPos = t*.5 - vec4(b, d, a, c);
    legsPos *= animationAmpX;
    
    vec3 pl = p;
    pl.x -= .8;
    pl.z -= 2. + legsPos.x;
    pl.yz = Rotation(legsRot.x) * pl.yz;
    float legs = cappedCone(pl-vec3(0.,0.,0.), .7, .3, .2);
    float clogs = cappedCone(pl-vec3(0.,-0.8,0.), .2, .35, .3);

    pl = p;
    pl.x += 1.;
    pl.z -= 2. + legsPos.y;
    pl.yz = Rotation(legsRot.y) * pl.yz;
    legs = min(legs, cappedCone(pl-vec3(0.,0.,0.), .7, .3, .2));
    clogs = min(clogs, cappedCone(pl-vec3(0.,-0.8,0.), .2, .35, .3));

    pl = p;
    pl.x -= 1.;
    pl.z -= 4. + legsPos.z;
    pl.yz = Rotation(legsRot.z) * pl.yz;
    legs = min(legs, cappedCone(pl-vec3(0.,0.,0.), .7, .3, .2));
    clogs = min(clogs, cappedCone(pl-vec3(0.,-0.8,0.), .2, .35, .3));

    pl = p;
    pl.x += 1.;
    pl.z -= 4. + legsPos.w;
    pl.yz = Rotation(legsRot.w) * pl.yz;
    legs = min(legs, cappedCone(pl-vec3(0.,0.,0.), .7, .3, .2));
    clogs = min(clogs, cappedCone(pl-vec3(0.,-0.8,0.), .2, .35, .3));

    
    vec3 ph = p + vec3(0., -2., -1.2);
    ph.xz = Rotation((time*animationSpeed.y - 0.5)*0.25*animationAmpY+headRot.x) * ph.xz;
    ph.zy = Rotation(sin(iTime*animationSpeed.y)*0.25*animationAmpY-headRot.y) * ph.zy;

    float head = length(ph-vec3(0.,-1.3,-1.2)) - 1.;
    head = smin(head, length(ph-vec3(0.,0.,0.)) - .5, 1.8);

    float glasses = sunglasses(ph);

    
    vec3 pp = ph;
    pp *= vec3(.7,1.,.7);
    float hair = length(ph-vec3(0.,0.35,-0.1))-.55;
    hair -= (cos(ph.z*8.+ph.y*4.5+ph.x*4.)+cos(ph.z*4.+ph.y*6.5+ph.x*8.))*.05;
    
    hair = smin(hair, body, 0.1);
    
    
    pp = ph;
    pp.yz = Rotation(-.6) * pp.yz;
    pp.x = abs(p.x)-.8;
    pp *= vec3(.3,1.,.4);
    pp -= vec3(0.,-0.05 - pow(pp.x,2.)*5.,-.1);
    float ears = length(pp)-.15;
    ears = smax(ears, -(length(pp-vec3(0.,-.1,0.))-.12), .01);
    pp.y *= .3;
    pp.y -= -.11;
    float earsClip =  length(pp)-.16;
    
    
    pp = ph;
    pp.x = abs(ph.x)-.4;
    float eyes = length(pp*vec3(1.)-vec3(0.,0.,-1.)) - .3;

    float eyeCap = abs(eyes)-.02;

    float blink = mix(smoothstep(0.95,0.96,blink)*.3 + cos(iTime*10.)*.02, 0.1, squintEyes);
    eyeCap = smax(eyeCap, smin(-abs(ph.y+ph.z*(.025))+.25-blink, -ph.z-1., .2), .01);
    eyeCap = smin(eyeCap, head, .02);
    head = min(head, eyeCap);

    
    pp.x = abs(ph.x)-.2;
    pp.xz = Rotation(-.45) * pp.xz;
    head = smax(head, -length(pp-vec3(-0.7,-1.2,-2.05)) + .14, .1);
    head = smin(head, Torus(pp.xzy-vec3(-0.7,-1.94,-1.2), vec2(.14,.05)), .05);

    float tears;
    if (sheepTears < 0.) {
      tears = INF;
    } else {
      pp = ph;
      pp.x = abs(ph.x)-.25;
      float shift = sheepTears*.02;
      tears = length(pp-vec3(0.,-0.15-shift*0.5,-1.1-shift*1.)) - .01 - shift*.1;
      tears -= pow(noise(pp*10.)*.5+.5, 1.) *.1;
      tears = smin(tears, head+.01, 0.1);
    }

    
    float tail = capsule(p-vec3(0.,-.1,cos(p.y-.7)*.5),vec3(cos(iTime*animationSpeed.z)*animationAmpZ,.2,5.), vec3(0.,2.,4.9), .2);
    tail -= (cos(p.z*8.+p.y*4.5+p.x*4.)+cos(p.z*4.+p.y*6.5+p.x*3.))*.02;
    tail = smin(body, tail, .1);
    
    
    vec2 dmat = vec2( body, WOOL_ID);
    dmat = MinDist(dmat, vec2(tail, WOOL_ID));
    dmat = MinDist(dmat, vec2(hair, WOOL_ID));
    dmat.x = smax(dmat.x, -earsClip, .15);
    dmat = MinDist(dmat, vec2(legs, SKIN_ID));
    dmat = MinDist(dmat, vec2(head, SKIN_ID));
    dmat = MinDist(dmat, vec2(tears, TEARS_ID));
    dmat = MinDist(dmat, vec2(eyes, EYE_ID));
    dmat = MinDist(dmat, vec2(clogs, CLOGS_ID));
    dmat = MinDist(dmat, vec2(ears, SKIN_ID));
    dmat = MinDist(dmat, vec2(glasses, MOTO_DRIVER_HELMET_ID));
    
    headDist = head;
    dmat.x *= SCALE;
    return dmat;
}
// --------------------------------------------------------------------
// Include end: sheep.frag

// Include begin: rendering.frag
// --------------------------------------------------------------------
vec2 sceneSDF(vec3 p)
{
    return MinDist(MinDist(MinDist(MinDist(MinDist(MinDist(
        motoShape(p),
        driverShape(p)),
        terrainShape(p)),
        treesShape(p)),
        blood(p)),
        panelWarning(p)),
        sheep(p, true));
}

float fastAO(vec3 pos, vec3 nor, float maxDist, float falloff) {
    float i_occ1 = .5*maxDist - sceneSDF(pos + nor*maxDist *.5).x;
    float i_occ2 = .95*(maxDist - sceneSDF(pos + nor*maxDist).x);
    return clamp(1. - falloff*1.5*(i_occ1 + i_occ2), 0., 1.);
}

float shadow(vec3 ro, vec3 rd)
{
    float res = 1.0;
    float t = 0.08;
    for(int i = 0; i < 64; i++)
    {
        float h = sceneSDF(ro + rd*t).x;
        res = min(res, 10.*h / t);
        t += h;
        
        if(res < 0.0001 || t > 40.) break;
        
    }
    return clamp( res, 0.0, 1.0 );
}

vec3 sky(vec3 V, vec3 fogColor)
{
    float cloud = noise(V/(0.05 + V.y));
    cloud = pow(smoothstep(0.5, 0.51, cloud+1.), 0.2);
    cloud = mix(1., cloud, 0.2);

    return 
        mix(
            fogColor,
            mix(vec3(0.7, 0.7, 0.7), vec3(0.2, 0.2, 0.6), cloud),
            pow(smoothstep(0., 1., V.y), 0.4));
}

float trace(vec3 ro, vec3 rd) {
    float t = 0.1;
    for(int i = 0; i < MAX_RAY_MARCH_STEPS; i++) {
        float d = sceneSDF(ro + rd*t).x;
        t += d;
        if (t > MAX_RAY_MARCH_DIST || abs(d) < 0.001) break;
    }
    
    return t;
}


float specular(vec3 v, vec3 l, float size)
{
    float spe = max(dot(v, normalize(l + v)), 0.);
    float a = 2000./size;
    float b = 3./size;
    return (pow(spe, a)*(a+2.) + pow(spe, b)*(b+2.)*2.)*0.008;
}

vec3 rayMarchScene(vec3 ro, vec3 rd)
{
    
    float t = trace(ro, rd);
    vec3 p = ro + rd * t;

    vec2 dmat = sceneSDF(p);
    vec2 eps = vec2(0.0001,0.0);
    vec3 n = normalize(vec3(dmat.x - sceneSDF(p - eps.xyy).x, dmat.x - sceneSDF(p - eps.yxy).x, dmat.x - sceneSDF(p - eps.yyx).x));

    
    
    
    vec3 sunDir = normalize(vec3(3.5,3.,-1.));
    vec3 fogColor = vec3(0.3,0.5,0.6);
    vec3 skyColor = sky(rd, fogColor);

    float ao = fastAO(p, n, .15, 1.) * fastAO(p, n, 1., .1)*.5;
    float material = dmat.y;
    
    float shad =
        (material == SKIN_ID || material == TEARS_ID) ?
        1.0 : shadow(p, sunDir);
    float fre = 1.0+dot(rd,n);
    
    vec3 diff = vec3(1.,.8,.7) * max(dot(n,sunDir), 0.) * pow(vec3(shad), vec3(1.,1.2,1.5));
    vec3 bnc = vec3(1.,.8,.7)*.1 * max(dot(n,-sunDir), 0.) * ao;
    vec3 sss = vec3(.5) * mix(fastAO(p, rd, .3, .75), fastAO(p, sunDir, .3, .75), 0.5);
    vec3 spe = vec3(1.) * max(dot(reflect(rd,n), sunDir),0.);
    vec3 envm = vec3(0.);
    
    
    vec3 amb = vec3(.4,.45,.5)*1. * ao;
    vec3 emi = vec3(0.);
    
    vec3 albedo = vec3(0.);
    if (t >= MAX_RAY_MARCH_DIST) {
        return skyColor;
    }

    if (material-- == 0.) { 
        albedo *= 0.;
        spe *= pow(spe, vec3(80.))*fre*15.;
        sss *= 0.;
    } else if (material-- == 0.) { 
        albedo = vec3(.01);
        spe *= 0.02;
        sss *= 0.;
    } else if (material-- == 0.) { 
        albedo = vec3(.2, .3, .2);
        sss *= 0.2;
        spe *= 0.;
    } else if(material-- == 0.) { 
        const float laneWidth = 3.5;
        if (abs(p.x) < laneWidth) { 
            vec2 laneUV = p.xz / laneWidth;
            float tireTrails = sin((laneUV.x+0.2) * 7.85) * 0.5 + 0.5;
            tireTrails = mix(tireTrails, smoothstep(0., 1., tireTrails), 0.25);
            float highFreqNoise = noise(vec3(laneUV * vec2(50., 2),0));
            tireTrails = mix(tireTrails, highFreqNoise, 0.2) * .3;
            vec3 color = vec3(mix(vec3(0.2,0.2,0.3), vec3(0.3,0.4,0.5), tireTrails));

            sss *= 0.;
            albedo = color;
            spe *= mix(0., 0.1, tireTrails);
        } else { 
            sss *= 0.3;
            albedo = vec3(.2, .3, .2);
            spe *= 0.;
        }
    } else if (material-- == 0.) { 
        albedo = vec3(.4);
        sss *= fre*.5+.5;
        emi = vec3(.35);
        spe = pow(spe, vec3(4.))*fre*.25;
    } else if (material-- == 0.) { 
        albedo = vec3(1.,.7,.5)*1.;
        amb *= vec3(1.,.75,.75);
        sss = pow(sss, vec3(.5,2.5,4.0)+2.)*3.;
        spe = pow(spe, vec3(4.))*fre*.02;
    } else if (material-- == 0.) { 
        sss *= .5;
        vec3 dir = normalize(eyeDir + (noise(vec3(iTime,iTime*.5,iTime*1.5))*2.-1.)*.01);
        
        
        vec3 t = cross(dir, vec3(0.,1.,0.));
        vec3 b = cross(dir,t);
        t = cross(b, dir);
        
        vec3 ne = n.z * dir + n.x * t + n.y * b;
        
        
        vec3 v = rd.z * eyeDir + rd.x * t + rd.y * b;
        vec2 offset = v.xy / v.z * length(ne.xy) / length(ro-p) * .4;
        ne.xy -= offset * smoothstep(0.01,.0, dot(ne,rd));
        
        const float i_irisSize = .3;
        
        
        float er = length(ne.xy);
        float theta = atan(ne.x, ne.y);
        
        
        vec3 c = mix(vec3(.5,.3,.1) , vec3(.0,.8,1), smoothstep(0.16,i_irisSize,er)*.3+cos(theta*15.)*.04);
        float filaments = smoothstep(-.9,1.,noise(vec3(er*10.,theta*30.+cos(er*50.+noise(vec3(theta))*50.)*1.,0.)))
            + smoothstep(-.9,1.,noise(vec3(er*10.,theta*40.+cos(er*30.+noise(vec3(theta))*50.)*2.,0.)));
        float pupil = smoothstep(pupilSize,pupilSize+0.02, er);
        albedo = c * (filaments*.5+.5) * (smoothstep(i_irisSize,i_irisSize-.01, er)); 
        albedo *= vec3(1.,.8,.7) * pow(max(0.,dot(normalize(vec3(3.,1.,-1.)), ne)),8.)*300.+.5; 
        albedo *= pupil; 
        albedo += pow(spe,vec3(800.))*3; 
        albedo = mix(albedo, vec3(.8), smoothstep(i_irisSize-0.01,i_irisSize, er)); 
        albedo = mix(c*.3, albedo, smoothstep(0.0,0.05, abs(er-i_irisSize-0.0)+0.01)); 
        
        
        n = mix(normalize(n + (eyeDir + n)*4.), n, smoothstep(i_irisSize,i_irisSize+0.02, er));

        v = reflect(rd, n);
        vec3 l1 = normalize(vec3(1., 1.5, -1.));
        vec3 l2 = vec3(-l1.x, l1.y*.5, l1.z);
        float spot =
            + specular(v, l1, .1)
            + specular(v, l2, 2.) * .1
            + specular(v, normalize(l1 + vec3(0.2, 0., 0.)), .3)
            + specular(v, normalize(l1 + vec3(0.2, 0., 0.2)), .5)
            + specular(v, normalize(l2 + vec3(0.1, 0., 0.2)), 8.) * .5;

        envm = (mix(
            mix(vec3(.3,.3,0.), vec3(.1), smoothstep(-.7, .2, v.y)),
            vec3(0.3, 0.65, 1.), smoothstep(-.0, 1., v.y)) + spot * vec3(1., 0.9, .8)) * mix(.15, .2, pupil) *sqrt(fre)*2.5;
        
        
        sceneSDF(p);
        albedo *= smoothstep(0.,0.015, headDist)*.4+.6;
        spe *= 0.;
    } else if (material-- == 0.) { 
        albedo = vec3(.025);
        sss *= 0.;
        spe = pow(spe, vec3(15.))*fre*10.;
    } else if(material-- == 0.) { 
        albedo = vec3(.6);
        sss *= 0.;
        spe = pow(spe, vec3(8.))*fre*2.;
    }  else if(material-- == 0.) { 
        albedo = vec3(1.,.01,.01)*.3;
        diff *= vec3(3.);
        amb *= vec3(2.)*fre*fre;
        sss *= 0.;
        spe = vec3(1.,.3,.3) * pow(spe, vec3(500.))*5.;
    } else if (material-- == 0.) { 
       vec3 p = p - panelWarningPos;
        sss *= 0.;
        spe = pow(spe, vec3(8.))*fre*20.;

        if (n.z > .5) {
            vec3 pp = p - vec3(-0.3,warningHeight - 0.25,0.);

            float symbol;
            if (sceneID==SCENE_MOTO) {
                pp.xy *= 0.9;
                float dist = 5.;
                headRot = vec2(0., -0.3);
                animationSpeed = vec3(0);
                for (float x = -0.2; x <= 0.2; x += 0.08) {
                    vec3 point = vec3(x, pp.y, pp.x);
                    point.xz *= Rotation(0.1);
                    dist = min(dist, sheep(point, false).x);
                }
                symbol = 1. - smoothstep(0.001, 0.01, dist);
            } else {
                symbol = smoothstep(0.13,0.1295, distance(p, vec3(0.,warningHeight-.45,-4.9)));
                symbol += smoothstep(0.005,0.,UnevenCapsule2d(p.xy-vec2(0.,warningHeight-0.15), .06,.14,0.8));
            }
            float tri = Triangle(p-vec3(0.,warningHeight,-5.), vec2(1.3,.2), .01);
            albedo = vec3(1.5,0.,0.);
            albedo = mix(albedo, vec3(2.), smoothstep(0.005,.0, tri));
            albedo = mix(albedo, vec3(0.), symbol);
        } else {
            albedo = vec3(.85,.95,1.);
        }
    } else if (material-- == 0.) { 
        albedo = vec3(1., .8, .65);
        amb *= vec3(1.0, 0.85, 0.85);
        sss = pow(sss, vec3(0.8, 1.8, 3.0) + 2.) * 2.;
        spe = pow(spe, vec3(32.0)) * fre * fre * 40.;
    }

    
    vec3 radiance = albedo * (amb*1. + diff*.5 + bnc*2. + sss*2. ) + envm + spe + emi;
    float fogAmount = 1.0 - exp(-t*0.005);
    vec3 col = mix(radiance, fogColor, fogAmount);

    return col;
}
// --------------------------------------------------------------------
// Include end: rendering.frag

// Include begin: camera.frag
// --------------------------------------------------------------------
void motoFaceImpactShot(float t_in_shot) {
        sceneID = SCENE_MOTO;
        float shift = t_in_shot*.1;
        float impact = smoothstep(9.7,10., t_in_shot);
        camPos = vec3(3. - impact - shift*1.2, 0.5, 0.);
        camPos.xz += valueNoise2(500.*t_in_shot)*shift*.1;
        camTa = vec3(0., 1. + shift*.2, 0.);
        camProjectionRatio = 1.5 + impact*5. + shift;

        globalFade *= 1. - impact;
}

void sheepScaredShot(float t_in_shot) {
    animationSpeed *= 0.;
    eyeDir = vec3(0.,-0.1,1.);
    vec2 noise = valueNoise2(100.*t_in_shot)*smoothstep(0., 5., t_in_shot);
    if (t_in_shot > 2.)
        sheepTears = t_in_shot;
    if (t_in_shot >= 5.) {
        noise *= 0.3;
        sheepTears = 4.*(t_in_shot - 4.);
    }

    headRot = vec2(0., -0.1) + noise*.1;
    camPos = vec3(1., 0.9, 6. - t_in_shot*.2);
    camTa = vec3(1., 0.8, 7.);
    sheepPos = 7.;
    camProjectionRatio = 1.5 + t_in_shot*.4;
    pupilSize = 0.2;
}

bool get_shot(inout float time, float duration) {
    if (time < duration) {
        return true;
    }
    time -= duration;
    return false;
}

void selectShot() {
    float time = iTime;
    float verticalBump = valueNoise2(6.*iTime).x;
    blink = max(fract(iTime*.333), fract(iTime*.123+.1));

    if (get_shot(time, 10.)) {
        globalFade *= smoothstep(0., 7., time);

        
        float motion = time*.1;
        float vshift = smoothstep(6., 0., time);
        camPos = vec3(1., 0.9 + vshift*.5, 6. - motion);
        camTa = vec3(1., 0.8 + vshift*1., 7. - motion);
        sheepPos = 7. - motion;
        camProjectionRatio = 1.5;

        float headShift =
            smoothstep(6., 6.5, time) * smoothstep(9., 8.5, time);
        headRot = vec2(0., 0.4 - headShift*.5);
        eyeDir = vec3(0.,0.1-headShift*0.2,1.);

    } else if (get_shot(time, 5.)) {
        sceneID = SCENE_MOTO;
        camTa = vec3(1., 1., 0.);
        camPos = vec3(-2. - 2.5*time, .5+0.2*time, sin(time));

    } else if (get_shot(time, 5.)) {
        
        float motion = time*.1;
        camPos = vec3(2.5, 0.5, 3. - motion);
        sheepPos = 5. - motion;
        camTa = vec3(0., 1., 4.8 - motion);
        camProjectionRatio = 1.5;
        headRot = vec2(0., 0.2);
        eyeDir = vec3(0.,0.1,1.);

    } else if (get_shot(time, 5.)) { 
        sceneID = SCENE_MOTO;
        float xnoise = mix(-1., 1., valueNoise2(0.5*iTime).y) +
            + mix(-0.01, 0.01, valueNoise2(600.*iTime).y);
        float ynoise = 0.05 * verticalBump;
        vec2 p = vec2(0.95 + xnoise, 0.55 + ynoise);
        camPos = vec3(p, -2);
        camTa = vec3(p, 0.);
        camProjectionRatio = 1.5;

    } else if (get_shot(time, 5.)) {
        
        float shift = smoothstep(3.5, 3.8, time)*.5;
        float motion = time*.1;
        camPos = vec3(2.5, 1., 6.);
        sheepPos = 5. - motion;
        panelWarningPos = vec3(4., 0., 2.5);
        camTa = mix(vec3(1,1,5), vec3(1., 1.5, 1), shift*2.);
        headRot = vec2(0., 0.5);

    } else if (get_shot(time, 5.)) {
        sceneID = SCENE_MOTO;
        
        float bump = 0.02 * verticalBump;
        camPos = vec3(-0.2 - 0.3 * time, 0.88 + 0.17*time + bump, 0.42);
        camTa = vec3(0.5, 1. + 0.1 * time + bump, 0.25);
        panelWarningPos = vec3(4, 0., -40.);
        camProjectionRatio = 1.5;

    } else if (get_shot(time, 3.)) {
        
        float shift = smoothstep(0., 5., time) + time*.1;
        headRot = vec2(sin(time*2.)*.2, 0.5);
        eyeDir = vec3(sin(time*2.)*.2,0.3,1.);

        camPos = vec3(1., 0.6, 6. - shift);
        camTa = vec3(1., 0.8, 7.);
        sheepPos = 7. - shift;

    } else if (get_shot(time, 2.)) {
        sceneID = SCENE_MOTO;
        
        camPos = vec3(4. - time*.2, 0.8, 0.);
        camTa = vec3(-10., 0., 0.);
        camProjectionRatio = 1.5 + time*.2;

    } else if (get_shot(time, 5.)) {
        

        float motion = time*.1;
        float shift = smoothstep(3.5, 4., time);
        float headShift = smoothstep(2.5, 3., time);
        headRot = vec2(0., 0.4 - headShift*.5);
        eyeDir = vec3(
            .18-smoothstep(4.3, 4.5, time)*.18-smoothstep(3., 1., time)*.4,
            0.1-headShift*0.2,
            1.);
        camPos = vec3(1., 0.9, 6. - shift - motion);
        camTa = vec3(1., 0.8, 7. - motion);
        sheepPos = 7. - motion;
        camProjectionRatio = 1.5 + shift*2.;
        squintEyes = smoothstep(3.3, 3.5, time);

    } else if (get_shot(time, 3.)) {
        sceneID = SCENE_MOTO;
        
        float shift = time/10.;
        vec2 noise = valueNoise2(500.*time);
        camPos = vec3(3. - shift*1.2, 0.5, noise.y*.05);
        float shiftLeft = smoothstep(0.5, 0., time);
        float shiftUp = smoothstep(2., 2.5, time);
        camTa = vec3(0., 0.5 + shiftUp*.5, shiftLeft*0.5);
        camProjectionRatio = 2.;

    } else if (get_shot(time, 1.6)) {
        sheepScaredShot(time);
        pupilSize = .1+smoothstep(0., 1., time)*.1;

    } else if (get_shot(time, 1.4)) {
        motoFaceImpactShot(time);

    } else if (get_shot(time, 1.4)) {
        sheepScaredShot(time+1.5);
        
        blink = time*2.;
        headRot += vec2(sin(time*40.)*.15, -0.1+time*.5);

    } else if (get_shot(time, 1.4)) {
        motoFaceImpactShot(time+3.);
        lightFalloff /= 3.;

    } else if (get_shot(time, 1.6)) {
        sheepScaredShot(time+3.4);

    } else if (get_shot(time, 1.)) {
        motoFaceImpactShot(time+5.);
        lightFalloff /= 9.;

    } else if (get_shot(time, 1.6)) {
        sheepScaredShot(time+5.);
        camProjectionRatio++;
        
        blink = 1.6 - time;

    } else if (get_shot(time, 2.)) {
        motoFaceImpactShot(time+8.);
        lightFalloff /= 30.;
        

    } else if (get_shot(time, 10.)) {
        sceneID = SCENE_BLOOD;
        globalFade *= smoothstep(2., 5., time)
            	* smoothstep(9., 7., time);

        
        camPos = vec3(2.5, 1.5, -6. + time*.5);
        camTa = vec3(1., 0., -9. + time*.5);


    } else if (get_shot(time, 5.)) {
        sceneID = SCENE_MOUTARD;
        globalFade *= smoothstep(0., 1., time);
        float xnoise = mix(-1., 1., valueNoise2(0.5*time).y)
                + mix(-0.01, 0.01, valueNoise2(600.*time).y);
        float ynoise = 0.05 * verticalBump;
        vec2 p = vec2(0.95 + xnoise, 0.5 + ynoise);
        camPos = vec3(p, -1.5);
        camTa = vec3(p.x, p.y - 0.4, 0.);
        camProjectionRatio = 1.2;

    } else if (get_shot(time, 5.)) {
        sceneID = SCENE_MOUTARD;
        
        float trans = smoothstep(3., 0., time);
        camTa = vec3(3., 1. - trans*.8, 0.);
        camPos = vec3(5. - 0.1*time, 1. + 0.02 * verticalBump, 0.);
        headRot = vec2(0., 0.3);
        camProjectionRatio = 2. - smoothstep(0., 6., time);
        camProjectionRatio = 3. - time/5.;

    } else if (get_shot(time, 10.)) {
        sceneID = SCENE_MOUTARD;
        
        vec3 shift = mix(vec3(0), vec3(-3.5, 0, -3.5), smoothstep(7, 9, time));
        wheelie = smoothstep(4.2, 4.6, time)*(1+sin(time*2.)*.2);

        camTa = vec3(0., 1. - wheelie*.2, 0) + shift;
        camPos = vec3(5. - 0.1*time, 0.4-0.3*wheelie, -1.-0.4*time) + shift;
        headRot = vec2(0., 0.6);
        camProjectionRatio = 2.;
        camTa.xy += valueNoise2(500.*time)*.01;

        globalFade *= smoothstep(10., 8., time);

    } else {
        sceneID = SCENE_MOUTARD;
        camTa = vec3(0., 1., .7);
        camPos = vec3(4. - 0.1*time, 1., -3.-0.5*time);
        headRot = vec2(0., 0.3);
        camProjectionRatio = 3.;

        shouldDrawLogo = smoothstep(0., 1., time) * smoothstep(15., 9., time);
        globalFade = float(time < 15.);
    }

    if (sceneID == SCENE_MOUTARD) {
        headRot.y += abs(sin(iTime*4.)*.1);
        animationSpeed = vec3(0.);
    }

    float shotStartTime = iTime - time;

    
    float t = mod(shotStartTime, 14.)
        + (iTime - shotStartTime);
    if (sceneID == SCENE_SHEEP || sceneID == SCENE_BLOOD) {
        t = 0.;
    }

    motoPos = vec3(0, 0.3 + 0.43 * wheelie, 300. - 50.*t);
    motoPos.xz += 0.5*sin(iTime);
}
// --------------------------------------------------------------------
// Include end: camera.frag

// Include begin: logo.frag
// --------------------------------------------------------------------
float rect(vec2 p, vec2 size, float r) {
  float f1 = pow(max(abs(p.x) + r - size.x, 0.), 4.);
  float f2 = pow(max(abs(p.y) + r - size.y, 0.), 4.);
  return smoothstep(0., 0.000000008, f1 + f2 - pow(r, 4.));
}

float spacing = 0.15;

float base(vec2 p, float t) {
  float col = 1.;
  vec2 size = vec2(mix(0., 0.06, t));
  
  for (float i = 0.; i < 4.; i++) {
    for (float j = 0.; j < 3.; j++) {
      col *= (i == 3. && j == 1.) ? 1. : rect(p - vec2(i, j) * spacing, size, 0.01);
    }
  }
  
  return col;
}

float holes(vec2 p, float t) {
  float col = 1.;
  vec2 size = vec2(mix(0., 0.0255, t));
  float r = 0.01;
  
  float h = 0.25;  
  float v = 0.25;  
  float v2 = 0.19; 

  
  col *= rect(p - vec2(0.+h, 2.) * spacing, size, r);
  col *= rect(p - vec2(1.-h, 2.-v) * spacing, size, r);
  col *= rect(p - vec2(1.+h, 2.-v) * spacing, size, r);
  col *= rect(p - vec2(2.+h, 2.-v) * spacing, size, r);
  col *= rect(p - vec2(3.+h, 2.+v) * spacing, size, r);

  
  col *= rect(p - vec2(0., 1.-v) * spacing, size, r);
  col *= rect(p - vec2(1.+h, 1.+v) * spacing, size, r);
  col *= rect(p - vec2(2.-h, 1.-v) * spacing, size, r);
  col *= rect(p - vec2(2.+h, 1.-v) * spacing, size, r);
  
  
  col *= rect(p - vec2(-h, -v) * spacing, size, r);
  col *= rect(p - vec2(h, -v) * spacing, size, r);

  col *= rect(p - vec2(1.+h, v2) * spacing, size, r);
  col *= rect(p - vec2(1.+h, -v2) * spacing, size, r);

  col *= rect(p - vec2(2.-h, -v2) * spacing, size, r);
  col *= rect(p - vec2(2.+h, v2) * spacing, size, r);

  col *= rect(p - vec2(3.-h, -v) * spacing, size, r);
  col *= rect(p - vec2(3.+h, -v) * spacing, size, r);

  return col;
}

vec3 drawLogo(vec2 uv) {
  if (shouldDrawLogo <= 0.) return vec3(1.);

  uv = uv*0.6 + vec2(0.25, 0.15);
  float t1 = min(shouldDrawLogo*2., 1.);
  float t2 = min(shouldDrawLogo*2.-1., 1.);
  vec3 col = vec3(holes(uv, t2) - base(uv, t1));
  return col;
}


float digit7(vec2 q, int n)
{ 
	const int digitsegs[10] = int[] ( 
		95,10,118,122,43,121,125,26,127,123
		);
	if (n < 0 || n >= digitsegs.length()) return -1.; 
	int segs = digitsegs[n];
	const ivec2 segpos[7] = ivec2[] ( 
		  ivec2(-1,1), ivec2(1,1), ivec2(-1,-1), ivec2(1,-1) 
		, ivec2(0,2), ivec2(0,0), ivec2(0,-2) 
		
		);
	float d = 1e9;
	for (int i = segpos.length(); i-- > 0; ) {
		if ((segs & (1 << i)) == 0) continue;
		vec2 p = vec2(segpos[i]); 
		p *= vec2(.45, .45);
		p = q - p; 
		bool vertical = i < 4;
		
		
		if (vertical) p = p.yx; 
		p = abs(p);
		vec2 w = vec2(.35, .0); 
		p -= w;
		p = max(p, vec2(0));
		float dx = (p.x + p.y);
		d = min(d, dx);
	}
	d *= sqrt(.5); 
	return d - .05; 
}

void digits7(inout vec4 o, vec4 c, vec2 q, vec2 R, uint value)
{
	float d = 1e9;
    
	for (int i = 2; i-- > 0; ) {
		d = min(d, digit7(q, int(value%10u)));
		value = value / 10u;
		q.x += 1.25;
	}
	float a = clamp(.5 - .25 * R.y * d, 0., 1.); 
    a = mix(a, 1., exp2(-2. * max(0., d)) * .3 * (1. - abs(sin(3.14*iTime)))); 
	o = mix(o, c, a);
}// --------------------------------------------------------------------
// Include end: logo.frag



float bloom(vec3 ro, vec3 rd, vec3 lightPosition, vec3 lightDirection, float distFalloff)
{
    vec3 ol = motoToWorld(lightPosition, true) - ro;
    vec3 cameraToLightDir = normalize(ol);
    float dist = mix(1., length(ol), distFalloff);
    float aligned = max(0., dot(cameraToLightDir, -motoToWorld(normalize(lightDirection), false)));
    float d = 1.-dot(rd, cameraToLightDir);
    return aligned / (1.+lightFalloff*d) / dist;
}

void main()
{
    vec2 iResolution = vec2(XRES, YRES);
    vec2 texCoord = gl_FragCoord.xy/iResolution.xy;
    vec2 uv = (texCoord * 2. - 1.) * vec2(1., iResolution.y / iResolution.x);

    selectShot();

    
    vec3 cameraTarget = camTa;
    vec3 cameraUp = vec3(0., 1., 0.);
    vec3 cameraPosition = camPos;
    if (sceneID == SCENE_MOTO || sceneID == SCENE_MOUTARD) {
        cameraPosition = motoToWorldForCamera(camPos);
        cameraTarget = motoToWorldForCamera(camTa);
    }

    
    vec3 cameraForward = normalize(cameraTarget - cameraPosition);
    vec3 ro = cameraPosition;
    if (abs(dot(cameraForward, cameraUp)) > 0.99)
    {
        cameraUp = vec3(1., 0., 0.);
    }
    vec3 cameraRight = normalize(cross(cameraForward, cameraUp));
    cameraUp = normalize(cross(cameraRight, cameraForward));

    uv *= mix(1., length(uv), 0.1);
    vec3 rd = normalize(cameraForward * camProjectionRatio + uv.x * cameraRight + uv.y * cameraUp);
    

    vec3 radiance = rayMarchScene(ro, rd);

    
    if (sceneID == SCENE_MOTO || sceneID == SCENE_MOUTARD) {
        radiance += 0.3*bloom(ro, rd, headLightOffsetFromMotoRoot + vec3(0.1, -0.05, 0.),
                    vec3(1.0, -0.15, 0.0), 0.)
            * 5.*vec3(1., 0.9, .8);
    }

    radiance = pow(pow(radiance, vec3(1./2.2)), vec3(1.0,1.05,1.1));

    
    fragColor.rgb = radiance * globalFade * drawLogo(uv);

    
    
    

    fragColor /= 1.+pow(length(uv),4.)*0.6;
}
