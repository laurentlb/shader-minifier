
float stretch, gunsUp, gunsForward, edWalk, edTwist, edDown, edShoot, doorOpen, glow;

struct MarchData {
    vec3 mat;
    float specPower;
    float A,Z,e,R,T,Y,u,i,o,P,m,l,k,j,h,G,f,S,d,Q,W,X,c,v,B,n,_;
    float a1,a2,a3,a4,a5,a6,a7,a8,a9,a10;
    vec2 a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30;
};

struct AprilData {
    vec3 mat;
    float specPower;
    float A,Z,e,R,T,Y,u,i,o,P,m,l,k,j,h,G,f,S,d,Q,W,X,c,v,B,n;
};

void set(inout MarchData mat, AprilData a) {
    mat.mat = vec3(0.36, 0.45, 0.5);
    mat.specPower = 30.0;
    mat.d = 1.0;
    a.a11 = vec2(a.a1, a.a2);
    a.a21 = a.a22.yx;
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) { fragColor = vec4(1.0); }
