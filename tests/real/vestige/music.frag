// Vestige by Wrighter
// Source: https://github.com/wrightwriter/vestige
// MIT License

#version 460


layout(local_size_x=256) in;

layout(std430,binding=0)buffer Aa{vec2 hist[];};
//layout(std430,binding=0) coherent buffer Aa{uint hist[];};

//#define ANTI_TDR
//#ifdef ANTI_TDR
layout(location = 0) uniform int offs;
//#endif

float tau = 2.*acos(-1.); // can minify

uvec3 hash3u(uvec3 v) {
  v = v * 1664525u + 1013904223u;
  v.x += v.y * v.z; v.y += v.z * v.x; v.z += v.x * v.y; 
  v ^= v >> 16u;
  v.x += v.y * v.z; v.y += v.z * v.x; v.z += v.x * v.y;
  return v;
}


vec3 hash3f(vec3 v) {
  uvec3 u = hash3u(floatBitsToUint(v*125.12));
  return vec3(u) / float(-1u);
}
vec3 hash3fb(vec3 s) {
  s += 12.124;
  uvec3 r = floatBitsToUint(s);
  r = ((r >> 16u) ^ r.yzx) * 1111111111u;
  r = ((r >> 16u) ^ r.yzx) * 1111111111u;
  r = ((r >> 16u) ^ r.yzx) * 1111111111u;
  return vec3(r) / float(-1u);
}
vec3 h(float v){
	return hash3fb(vec3(v));
}

float ntf(float n){return 440.*pow(2.,n/12.);}
mat2 rot(float t) { return mat2(cos(t), -sin(t), sin(t), cos(t)); }


// == noise ========================================================================================
vec3 cyclic(vec3 p, float pers, float lacu) {
  vec4 sum = vec4(0);

  for (int i = 0; i ++ < 5;) {
    //p *= rot;
    p.yz *= rot(4.5);
    p.xy *= rot(1.5);
    p += sin(p.zxy);
    sum += vec4(cross(cos(p), sin(p.yzx)), 1);
    sum /= pers;
    p *= lacu;
  }


  return sum.xyz / sum.w;
}






void main(){
    //uint repd = ;

	uint samp = gl_GlobalInvocationID.x + uint(offs); // cast needed?

	vec2 s = vec2(0);
    vec2 qq = vec2(0);
//#ifdef ANTI_TDR
//#endif

	float T = float(samp) / 44100;
	float Tt = T;

	//float interp = 

    float TT = float(samp%(44100*12))/44100;
	float outro = smoothstep(430,470,Tt);
	float genv = 1;

    float its = 1000;

	if(T> 280.0){
		float t = mod(T, 4.);
		float env = exp(-t*10.);
		//env +=  ;
		
		float f = 35+ pow(outro,40)*100;
        s += (env
			//+ pow(outro,40.)*0.
		)*sin(TT*f*tau + sin(TT*f*tau*10) * env*.2  )*30;


		//float sub = 
		//s += exp()
	}

	if(T > 250.){
	//if(true){
		float md = 2 + sin(T)*.0002;

		float sc = 4;
		T *= sc;

		float id = floor(-T/md);

		//T = fract(-T*md + h(floor(T*md)).x*100.)*0.3;
		float mt = mod(-T , md);
		T = mt + h(id).x*sc + (T/sc - 260)*.6;
		genv = 
			//smoothstep(0.,0.2,mod(-T , md)) 
			//* smoothstep(md,md - .04,mod(-T , md))
			smoothstep(0.,md*.04,mt) 
			* smoothstep(md,md*.98,mt);
		//genv *= 4.;
		genv *= 1.-outro;
		TT *= .25 + float(Tt > 260)/4 + h(id).z*0.02;
		//TT -= h(id).x*10.;
		//samp += int(h(id).x*10000.);
	} else {
		T = mix(T,0,pow(clamp((T - 230)*.04,0, 1),2));
	}

    
     
    //float[4] notes = float[](0.,3.,5.,10.);
    float[4] notes = float[](- 12.,1.,3.,10.);

    //its = 10. + smoothstep(0.,1.,T)*40.;

	/*
	vec2 outro_mw = 
		-pow(outro,35.) * (
			cyclic(0.2*vec3(vec2(sin(T),cos(T)),10.*T), 0.2, 0.3).xy*30.*40.

		);
	outro_mw -= outro_mw;
	*/
	
	vec2 chord = vec2(0);
	{ // SINE CHORD
		float its = 2400;
		float[7] scale = float[](0., 1., 3., 5., 7., 8., 10.);
		for(float i = 10; i++ < its ;){
		  //float replen = 1.0/4.0*4.0
		  float replen = 60./140*4
			// + sin(time.z)*0.02
		  ;


		  // . . . .
		  // .
		  float t = T;
		  t = t - replen * floor(h(i).z *4);
		  t -= replen * pow(h(i
			// + tid
		  //).y,1.2)*1.;
		  ).y,1.2)*(1. + 6 * smoothstep(10,20,T));
		  float tid = floor(t/(replen*4));

		  float offsc = smoothstep(0,4,tid)/2;      

		  t = mod(t,replen*4.);
		  // time.x = a beat

			  //
		  float f = ntf(
			scale[int(i + tid)%7] 
			- 12. 
			+ 12. * float(h(i + tid).y < .4*offsc) 
			+ 12. * float(h(i +tid).x < .4*offsc) 
			- 12. * float(h(i + tid).z < .5*offsc)
			- 12. * float(h(i*1.5 + tid).z < .5*offsc)
			- 12. * float(h(i*1.25 + tid).z < .2*offsc)
		  );

		  float kits = 2. + pow(h(i + tid).x,2.5)*20.;
		  for(float k = 0; k < kits; k++){
			float g = f;
			//if(h(g).x < 0.5){
				//g *= 0.5;
			//}
			float ots = 1.
				+ smoothstep(0,4,tid) * 2

				* exp(-mod(T,1)) 
				//* smoothstep(0.,0.004,mod(t,1.))
				;
			
			float ot = pow(h(i + k*1.5 + tid).x,2. +.5 * smoothstep(500,2000,g));
			g *= 1. + floor(ot*ots);
			g += (h(k + i*1.4).x - .5)*20*(smoothstep(0,30,T));
			float env = exp(
			  -t*(7. - 6.*smoothstep(2000,800,g))*.7) 
			  * smoothstep(replen*2,0,t) 
			  * smoothstep(0.,0.002,t)
			  * smoothstep(7000,800,g)
			;

			
		  
			vec2 wave = vec2(
			  sin(
				tau * g * TT 
				+sin(h(i*2.4 + tid).x*15*TT)*4. * smoothstep(10,20,T) // chorus
				+ sin(tau * g * TT*2)*env*env*env*.6 * smoothstep(20,60,T)  // fm

				//- outro_mw
				//- pow(outro,65.)* cyclic(vec3(vec2(sin(t),cos(t)),i)*100., 1., 1.).xy*30.*100000.
				//- pow(outro,65.)* cyclic(vec3(vec2(sin(t),cos(t)),10.*t)*100., 1., 1.).xy*30.*100000.
			  )
			) * rot(i);



			if(Tt < 260 )
				wave = mix(wave,tanh(wave),0.);
			else
				wave *= 2.;

			//float otenv = exp(-mod(T,1.)*1.)* smoothstep(0.,0.004,mod(t,1.));
			//float otenv = exp(-mod(T,1)*1.)* smoothstep(0.,.004,mod(T,1.));
			//env *= mix(1.,0.,ot*(1.- otenv));
			//
	  
			chord += env * wave/its/kits*200. * (float(T < 60.) 
				+ outro*4.
			);
		  }
		}
	}
	s += chord;

    its = 400;

    for(float i = 0.; ++i < its;){ // ---- LEAD
        float t = T - pow(smoothstep(0,1,hash3f(vec3(i)).x),1.2)*(.2 + 3 * hash3f(vec3(i)).y);
        
        if(i < 2){
            t = T; // ??
        }


		float[4] b_notes = float[](
			notes[0],
			notes[1],
			notes[2],
			notes[3]
		);
		

		if(mod(t,8) < 1){
			for(int i = 0; i < 4; i++){
				b_notes[i] += 7;
			}
		}

        float repd = .5 + sin(t/2)*2/t * smoothstep(10,20,T);

        repd = mix(10,repd,smoothstep(0,60,T)) ;
        
        int tid = int(t/repd);
        float tt = mod(t,repd);
        float n = b_notes[tid%4] 
			- 12 * float(hash3f(vec3(tid)).x < 0.2)
            + 12 * float(hash3f(vec3(tid)).z < 0.2);

		n += 12 * float(t > 60) - 24;

        float f = ntf( n );
        
		// detune
        f -= 12 * float(hash3f(vec3(tid)).x < 0.2);
        
        float env = smoothstep(0.,0.004,tt) * exp(-tt*10.);
		float vol = 6;
        float fmamt = 2 + 10*smoothstep(60,100,T);
        float fmwave = env * env * sin(TT*f*tau) * fmamt;
        if(i < 2){
            s += env*sin(TT*f*tau + fmwave * smoothstep(1400,500,f))*.3*vol;
        } else {
            //float q = tt*0.0;
			//float q = 0.; // MINIFIABLE
			vec2 w = vec2(
				7*env*sin(TT*f*tau 
				//+ fmwave * smoothstep(600.,500.,f)
				+ fmwave * (1 - float(T > 260)*.4)
				//+ i*vec2(1.53251,1.2)
				+ cyclic(vec3(1.,.5,i), 1, 1).xy*30 // CAN BE MINIFIED
				)
			)/its*vol;

			s += w;
            //s += tanh(w);
        }
    }
    
    
    
    //s /= its;
    s *= .14;
    
    its = 44. - 10.*smoothstep(30.,50.,T);
    
    for(int n = 0; n < 4; n++){ // ---- choir
    for(float i = 0.; ++i < its;){
        float t = T;
        float f = ntf(
            notes[n] 
            + 12 * float(hash3f(vec3(i)).z < 0.1)
            + 12 * float(hash3f(vec3(i)*100.).x < 0.1)
            - 12 * float(hash3f(vec3(i)).x < 0.5)
            - 12 
            //+ 12. * clamp((t - 0.5)*0.1,0.,1.)
            )
            //+ 12. * float(hash3f(vec3(i)).z < 0.1)
            //- 12. * float(hash3f(vec3(i)).y < 0.5)
                
        ;
        
        //f -= hash3f(vec3(i)*2.).z*211. * pow(smoothstep(50.,20.,t),60.);
		//f -= 12.;
        f += 12 * float(hash3f(vec3(i)).z < 0.1);
		f -= hash3f(vec3(i)*4.).x * smoothstep(120,144,T)*100;
        

		if(Tt > 340){
			t = TT*2;
			//ss -= ss;
			//ss += sin(t*f*tau);
		}
        t *= tau * f;
        //t += sin(i*4. + float(n) + T*0.3)*14.;



        

        vec2 ss = cyclic(
            //vec3(vec2(sin(t),cos(t)),i*0.1 + float(n))
            vec3(fract(t/tau)) * vec3(2, -3, -8)
            , 
            .5, 
            .4
        ).xy * rot(i*.2 );
        ss -= cyclic(
            vec3(vec2(sin(t),cos(t)),i*.1 + float(n))
            //vec3(fract(t/tau)) * vec3(2.0, -3.0, -8.0)
            , 
            .5, 
            smoothstep(120,144,T)*2
        ).xy * rot(-i*.5 );


        
        s += ss /its*.15*smoothstep(60,100,T)*5*smoothstep(250,230,Tt);
    }
    }
    
    { // --------- BASS
        //float t = T - pow(smoothstep(0.,1.,hash3f(vec3(i)).x),1.2)*(0.2 + 3. * hash3f(vec3(i)).y);
        //float t = T;


        float repd = .5 + sin(T/2)*2/T;
        
        float tid = floor(T/repd);
        float tt = mod(T,repd);
        //float n = notes[int(mod(tid,4.))] - 24. - 12. - 12.;
        float n = notes[0] - (12. + 12. + 12. - 5);
        float f = ntf(n);
        
        
        //f -= 12. * float(hash3f(vec3(tid)).x < 0.2);
        

			
//+ .5*smoothstep(140.,150.,T)
		float end = smoothstep(160,180,T);
        float env = smoothstep(0.,0.04,tt) 
			* exp(-tt*(20. - 15. * end)) 
			* smoothstep(repd,0.,tt);


		
        float fmamt = 1 - .9 * end;
        float fmwave = env * env * sin(TT*(f*(1 + end*10 * floor(h(tid).x*10)))*tau) * fmamt;
        
		env *= smoothstep(100,110,T);

		if(T > 60 && T < 70){
			env = exp(-(T-60))*4;
		}
		if(T > 80 && T < 100){
			env = exp(-mod(T,4))*.4
			//*smoothstep(2,1,mod(T,4))
			;
		}
		/*
		env *= 1. 
			//+ .0*smoothstep(140.,150.,T)
		;*/
        //qq += env*sin(TT*f*2)*0;
        qq += env*sin(TT*f*tau + fmwave )*6;
		//s = qq;
    }
	s *= 2;
    //qq /= its;
    s += qq;

    
	s = tanh(s*1.2*smoothstep(0,5,Tt))*.7 ;
	s *= 1.; // remove clipping?
	s = clamp(s,-1,1);
	//s += chord*outro;
	//s *= 0.;
 

	hist[samp + 3000000] = s*genv 
		+ tanh(chord*outro*.2 + sin(tau*30*Tt)*10 * pow(outro,30));
}
