// Vestige by Wrighter
// Source: https://github.com/wrightwriter/vestige
// MIT License

#version 460

layout(local_size_x=256) in;

layout(std430,binding=0) coherent buffer Aa{uint hist[];};


layout(location = 1) uniform float Tt;


vec2 R = vec2(1300,740) + 20;


float pi = acos(-1.);
//layout(binding = 3) uniform sampler2D iChannel2;

uint seed;
uint hash_u(uint a) {

    a ^= a >> 16;
    a *= 0x7feb352du;
    a ^= a >> 15;
    a *= 0x846ca68bu;
    a ^= a >> 16;
    return a;
}

float hash_f_s(float s){ return ( float( hash_u(uint(s)) ) / float( -1u ) ); }
float hash_f(){ uint s = hash_u(seed); seed = s;return ( float( s ) / float( -1u ) ); }

vec3 hash_v3_s(float s){ return vec3(hash_f_s(s), hash_f_s(s), hash_f_s(s)); }









// [
#define pmod(p,amt) (mod((p+amt*0.5),amt)-amt*0.5)
// ]

mat2 rot(float a){return mat2(cos(a), -sin(a), sin(a), cos(a));}
float quant(float a, float b){return floor(a*b)/b;} // remove?
float hash_f_p_neg(float prob){ return float(hash_f() < prob)*2. - 1.; }




void main( ){
    uint id = gl_GlobalInvocationID.x; 
    seed = id + 1111111u;

	float T;
	T = mod(Tt,360);

	//float end = smoothstep(250,250.1,T);
	float end = float(T>250);

	//T -= hash_f()*10.*end;
	//T = mod()

	float repd = .5 + sin(T/2)*2/T;
	float eenv = mod(T,repd);
	eenv = exp(-eenv*4);


	float midsec = smoothstep(155,180,T) * smoothstep(250,249,T);
	// K OFFS LATER ON
	float K_OFFS = 
		.4* smoothstep(45,15,T)
		//-(.1)* midsec * eenv * sign(hash_f_s(T)- 0.5)
		-(.1)* midsec * eenv * (hash_f_s(T)- .5)
		+(.4)* smoothstep(314,320,T)*eenv
		;
	K_OFFS *=  1. - end;
	float K_ITERS = 
		200 * smoothstep(25,65,T)
		;
	bool K_CUBE = mod(T,8) < 2;
	bool K_SPHINV = hash_f_s(floor(T + 1555))>.5;
	float K_DIR_SC = 1
		+ .02* smoothstep(90,150,T)
		- midsec*0.8
	;
	float K_PERSP = .2
		+ .4* smoothstep(60,80,T)
		- .4* smoothstep(90,110,T)
		- .4* smoothstep(120,160,T)
		 - .2 * midsec
	;
	float K_RB_SC = 1
		- 1* smoothstep(90,110,T)
		+ 1* smoothstep(90,150,T)
	;

	if(T < 10){
		K_CUBE = false;
	}

    int iters = 30 + int(K_ITERS);

    float t = T;
    float env = t + sin(t)*.8;









    vec3 p = vec3(0);
    vec3 tr = vec3(hash_f(), hash_f(), hash_f());;
    vec3 rb = hash_v3_s(floor(T));

    vec3 dir = vec3(1,0,0);
    if(tr.y < .5){ dir = vec3(0,-1,0); } 

	dir *= K_DIR_SC;

	rb *= K_RB_SC;
    

    for(int i = 0; i < iters; i++){
        float r = hash_f();
        p += dir*(.002*(1 + 20*rb.x) + sin(float(id.x))*.01);
        if(tr.y < .5){
            if(i%10 == 0){
                dir += p;
            }
            if(i%50 == 10){
                dir.yz *= rot(pi*hash_f_p_neg(.5)/4);
            }
            if(i == 20 + int(sin(T)*45)){
                dir.xy *= rot(pi*hash_f_p_neg(.5)/4 + T);
                if(rb.z > .5){
                    p.xy *= rot(rb.x);
                }
            }
            
            if(rb.x > 0.7){
                p/= (dot(p,p) - 0.1 + sin(T)*0.5 + 0.5);
                p *= 1. + sin(T + sin(T));
            }
        } else if(tr.y < 1.){
            if(i == 20){
                dir.xz *= rot(0.25*pi);
            }
            if(i%50 == 50 + int(sin(T*0.6)*45)){ // can go lower
                dir.yz *= rot(pi*hash_f_p_neg(0.5)/4);
                p += dir * 1.;
            }
            if(i == 20 + int(sin(T*0.3 + float(id))*25)){
                p += dir * (1. + env) ;
//                dir *= rotZ(0.5*pi*hash_f_p_neg(0.5));
                dir.xy *= rot(0.5*pi*hash_f_p_neg(0.5));
            }
            // p.x -= 0.001 + T*0.01;

        } else {


        }
		// SQUARE GOOD SHIT
		if(abs(K_OFFS) > .0001){
			p = pmod(p,3 + K_OFFS);
		} else {
			p = pmod(p,3);
		}
        

        vec3 q = p;

		if (K_CUBE){
			q = sin(q*5 + T);
			q.xz *= rot(T*2 + sin(T*2) + K_OFFS*100);
			
			if (K_SPHINV){
				q /= dot(q, q) - .2 + K_OFFS;
				q += .4;
				q /= dot(q, q) - .2;
				q.xz *= rot(T);






			}
		} else {
			q.xz *= rot(quant(hash_f_s(floor(T + 50)), 4)*pi*2);
		}

		q.z += 4;









		if(T < 316){
			q.xy /= q.z * K_PERSP;
		}

		//q.x /= R.x/R.y;

		q.x = q.x*R.y/R.x
			+ 10*(hash_f() - 0.5)*abs(
				q.z - sin(T*9)*4*float(T>280)
			)* end * (1 -  smoothstep(314,320,T)) ; 

		;

        
        uvec2 cc = uvec2(( (q.xy )/2. + 0.5)*R);
        uint idx = cc.x + uint(R.x) * cc.y;

		atomicAdd(hist[idx],7);
		atomicAdd(hist[(idx*(1 + uint(rb.x*6 + T)%2))%uint(R.x*R.y)],uint(111)); // intentional bug lol, but looks cool
    }    

}
