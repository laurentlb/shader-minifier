uniform float time;
uniform float param1;


uniform sampler1D distRamp;
uniform vec3 matColor;
uniform vec3 ambientColor;



float gausa( float a )
{ 
	return texture1D(distRamp, abs(a) * 0.125 ).r; /*    pow(0.5f, abs(a)); */
}

float gausb( float a )
{ 
	return pow(0.5, abs(a));
}

mat3 rotationMatrix( float a , float b , float c )
{
	float xvs = sin(a);
	float yvs = sin(b);
	float zvs = sin(c);
	
	float xvc = cos(a);
	float yvc = cos(b);
	float zvc = cos(c);
	
	float Ox1 = yvc * zvc;
	float Ox2 = xvc * yvs * zvc + xvs * zvs;
	float Ox3 = xvc * zvs - xvs * yvs * zvc;
	
	float Oy1 = xvc * zvc + xvs * yvs * zvs;
	float Oy2 = xvs * zvc - xvc * yvs * zvs;
	float Oy3 = -yvc * zvs;
	
	float Oz1 = xvc * yvc;
	float Oz2 = -xvs * yvc;
	float Oz3 = -yvs;
	
	return mat3( Ox1 , Ox3 , Ox2 ,
	          Oy3 , Oy1 , Oy2 ,
	          Oz3 , Oz2 , Oz1 );
}





vec3 objet(vec3 c , vec3 v , float t )
{
	vec3 vt = vec3(8.0,2.0,2.0);
	
	float ti = cos(t * 3.0);
	float to = cos(t * 0.2) * 0.02 + 0.5; 
	
	vec3 co = vec3(0.0);
	
	
	mat3 disto = rotationMatrix( v.z , v.x , -v.y );
	
	vec3 vi = v * disto;
	float lvi = length(vi);
	
	float gg = gausa((lvi - 12.0 * to))
	         * gausa((8.485 - 12.0 * to))
	         * gausa((lvi - 8.485));
	
	vec3 col = c * vec3(1.0 - gg);
	
	vt.x = vi.x + 3.0;
	
	
	float size = param1;
	float gg2 = gausa(size * (lvi - 6.0 * to))	
	          * gausa(size * (lvi - length(vt)));
	
	vec3 material = matColor;
	
	return mix(col, material , gg2 * param1);
}

void  main(void)
{
	float t = time;
	
	vec2 xy = gl_TexCoord[0].xy;
	
	
	vec3 coll = vec3(0.0);
	
	const int iter = 3;
	
	mat3 rotation = rotationMatrix( t, t, -t);
	
	vec3 d = rotation * vec3(xy, float(iter));
	vec3 a = rotation * vec3(xy * vec2(2.0), -float(iter));
	
	
	for (int z = 0; z <= iter; ++z)
	{
		float fr = float(z) / float(iter);
		vec3 posi = mix(d, a, fr); 
		coll = objet(coll, posi, t);	
	
	}
	gl_FragColor = vec4 (coll, 1.0);
}
