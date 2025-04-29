// Vestige by Wrighter
// Source: https://github.com/wrightwriter/vestige
// MIT License

#version 460

layout(location = 1)uniform float T;

layout(std430,binding=0) coherent buffer Aa{uint hist[];};

out vec4 C;

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

uint get_hist_id(ivec2 c){
    c += 20;
	return (c.x + 1320 * c.y + uint(1320*760))%uint(1320*760);
}



float draw_char(inout vec2 p, inout float sd, int char_idx, int char_cnt){
int[148] chars = int[](
    // C 0 4
    -1,-1,-1,1,
    -1,1,1,1,
    -1,1,-1,-1,
    -1,-1,1,-1,

    // A 4 5
    -1,-1,-1,1,
    -1,1,1,1,
    -1,0,1,0,
    -1,1,-1,-1,
    1,1,1,-1,
    
    
    // N 9 4
    -1,-1,-1,1,
    -1,1,1,1,
    -1,1,-1,-1,
    1,1,1,-1,
    
    // T 13 3
    -1,-1,-1,1,
    -1,1,1,1,
    0,1,0,-1,
    
    
    // G 16 5
    -1,-1,-1,1,
    -1,1,1,1,
    -1,-1,1,-1,
    -1,1,-1,-1,
    1,0,1,-1,
    1,0,0,0,
    
    
    // O 22 5
    -1,-1,-1,1,
    -1,1,1,1,
    -1,-1,1,-1,
    -1,1,-1,-1,
    1,1,1,-1,
    
    
    // O 27 6
    -1,-1,-1,1,
    -1,1,1,1,
    -1,-1,1,-1,
    -1,0,1,0,
    -1,1,-1,-1,
    1,1,1,-1,
    
    // K 33 4
    -1,-1,-1,1,
    -1,0,1,-1,
    -1,0,1,1,
    -1,1,-1,-1
    
);

    float th = 0.2;
    for(int i = char_idx; ++i < char_cnt + char_idx;){
		vec2 a = vec2(chars[i*4], chars[i*4 + 1]);
		vec2 b = vec2(chars[i*4 + 2], chars[i*4 + 3]);
		float l = length(b-a);
		vec2  d = (b-a)/l;
		vec2  q = (p-(a+b)/2);
			  q = mat2(d.x,-d.y,d.y,d.x)*q;
			  q = abs(q)-vec2(l,th)/2;
        sd = min(sd,length(max(q,0.0)) + min(max(q.x - th,q.y),0.0));
    }
    p.x -= 2.5;
    return sd;
}



void main( ){
	//float end = smoothstep(250.,250.1,T);
	float end = float(T > 250);
	//float endb = smoothstep(280.,280.1,T);
	float endb = float(T > 280);

	float repd = 0.5 + sin(T/2)*2/T; // MUSIC ENV

	float K_COL = .03
		+ .05* smoothstep(90,190,T)
		//+ 0.1* smoothstep(90.,190.,T)
		- end 
		//+ 4*endb * smoothstep(4,0,mod(T,4))
		+ endb * (0.8 + 3 *smoothstep(4,0,mod(T,4)))

		;

	;
	//K_COL = 0.2;
	float rewind = smoothstep(230,250,T)*float(T<250);
	
	float K_GLITCH = pow(smoothstep(0,60,T),10.);
	K_GLITCH *= pow(hash_f_s(floor(T)), 2);
	K_GLITCH += .9* smoothstep(160,190,T)*exp(-mod(T,repd)*40.); // doesnt do much
	K_GLITCH *= 1-end + endb;
	K_GLITCH += rewind;


	seed = uint(gl_FragCoord.x +gl_FragCoord.y*1111);
	hash_f(); // ------------ DELETE IF DESPARATE

	if(T > 80 && T < 100){
		K_GLITCH = 100 * smoothstep(1,0,mod(T,4.));
	}

	uint hist_id = get_hist_id(ivec2(gl_FragCoord.xy));

	// tonemap
	vec3 col = vec3(hist[hist_id]) * .0001;
	float pal_idx = col.x*460;
	
	vec3[4] kCols = vec3[](
		vec3(1), vec3(1,0,1), vec3(1,1,.1), vec3(.75,1.5,1.5)
	);
	vec3 pal = mix( kCols[int(pal_idx)%4], kCols[int(pal_idx + 1)%4], smoothstep(0,1,fract(pal_idx)) );
	col = col/(1+col);
	if(hash_f_s(floor(T*.8))<.7){
		col = 1- col;
	} else {
		col = pow(step(col,vec3(.5)),vec3(0.02)); 
		col = pow(abs(col),vec3(0.02));
	}


	if(T > 60 && T < 65){
		col = 1 - col;
		//env = exp(-(T-60.))*4.;
	}

	if(abs(col.x - .1) < K_COL
	//&& hash_f_s(floor(T)) < 1
	){
		col -= pal;
	}

	C = pow(abs(col.xyzz), vec4(4.4));

	if(T > 460){
		//hist[hist_id] *= 5;
		hist[hist_id] = (hist[hist_id]*400)%10000000;
	} else if(mod(T,9.- endb*5 + rewind * 5) < 1. - float(T > 460)){
		hist[hist_id] = 0;
	} else {
		
		if (col.x < K_GLITCH){
			ivec2 offs = -ivec2(0, 4);
			//seed += int(T*10); // ???????
			offs *= ((int(hash_f() < col.x*1111))*2 - 1)*(1 + 5*int(hash_f_s(floor(T))));
			offs *= int(hash_f_s(floor(T) * 124)*2)*2 - 1;
			offs *= 
				1 
				+ int(endb * smoothstep(3.,0.,mod(T,4.))*20.) 
				+ int(rewind*100)
				+ int(20*smoothstep(130,190,T)*exp(-mod(T,repd)*40.)*float(T<250))
			;
			if(T > 124 && T < 250.){
				offs = ivec2(offs.y,offs.x);
			}
			//if(T > 316 && hash_f() > 0.5){ // ? makes it stay in place sometimes
			if(T > 350 && hash_f() > 0.5){ // ? makes it stay in place sometimes
				//offs = ivec2(offs.y,offs.x)/10;
				offs -= offs;
			}
			if(T > 280){ // ? makes it stay in place sometimes
				offs = ivec2(-offs.y,offs.x);
			}

			
			hist[hist_id] = hist[get_hist_id((ivec2(gl_FragCoord.xy) + offs))];
		}else {
			hist[hist_id] = 0;
		}
	}

	//hist[hist_id] = uint(hash_f()*10000);

	if(end > .1){
		//C*= 0.1;
		C = 1-C;
	}
	if(T > 350){
		C = vec4(dot(C,C) < .5);
	}



	vec2 uv = (gl_FragCoord.xy - vec2(1280,720)/2)/720*20;
	//uv *= 20.;

    float sd = 10;

	if(int(T)%3 == 0){
		uv.x += 3.75; // can be smaller
		draw_char(uv,  sd, 0, 4); // C
		draw_char(uv,  sd, 4, 5); // A
		draw_char(uv,  sd, 9, 4); // N
		draw_char(uv,  sd, 13, 3);// T
	} else if (int(T)%3 == 1){
		//uv.x += 20./13.5 - 0.25; // can be smaller
		uv.x += 1.23;
		draw_char(uv,  sd, 16, 6); // G
		draw_char(uv,  sd, 22, 5); // O
	} else {
		uv.x += 3.75;
		draw_char(uv,  sd, 27, 6); // B
		draw_char(uv,  sd, 4, 5); // A
		draw_char(uv,  sd, 0, 4); // C
		draw_char(uv,  sd, 33, 4); // K
	}
	if(sd < 0 && (T > 420 && T < 450)){
		//if(false){
	
		//hist[hist_id] = 1 - hist[hist_id]*500000;
		hist[hist_id] += 100000;
		//hist[hist_id] = -1u;
	}
}
