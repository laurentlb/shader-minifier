const float PI = 3.1415926;


uniform float localTime;

vec3 coul2(float x, float y)
{
	 float h = (x + 1.0) * 350.0;
	 float w = (y + 1.0) * 350.0;
	 
 	 float hi = (h - 350.0) / 20.0;
     float wi = (w - 350.0) / 20.0;
    
	 float xo = hi;
	 float yo = wi + localTime;/*cos(hi/5.0) + wi + 0.1; */
	    
	 float xa = cos(hi/5.0);
	 float ya = cos(wi/5.0);
		
	 float cosxa = cos(xa);
	 float sinxa = sin(xa);
	 float cosya = cos(ya);
	 float sinya = sin(ya);
		
    vec3 res = vec3(0.0,0.0,0.0);
    
    for (int l = 0; l <= 30; l++)
    {
    	float li = (float(l) - 15.0) * 2.0;
    	
		float zo = li;
		
		float za = cos(li / 20.0); 
		
		float tmp = yo * cosxa + zo * sinxa;		
		zo = zo * cosxa - yo * sinxa;
		yo = tmp;
		
		float tmp2 = xo * cosya + zo * sinya;
		zo = zo * cosya - xo * sinya;
		xo = tmp2;
	/*	
		float tmp3 = xo * cos(za) + yo * sin(za);
		yo = yo * cos(za) - xo * sin(za);
		xo = tmp3;
	*/	
		vec3 color = vec3(128.0) + vec3(128.0) * vec3(cos(zo), cos(zo + PI * 2.0 / 3.0), cos(zo - PI * 2.0 / 3.0));
		
		float length = sqrt( xo * xo + yo * yo + zo * zo ) - 30.0;
		float contrib = 0.25 / (1.0 + 400.0 * length * length);
		
		res = res * (1.0 - contrib) + color * contrib;
	/*	res += contrib * color; */
 	 }
 	 
 	 return res;
 }

void main()
{	
	vec2 p = 0.5 * gl_TexCoord[0].xy + vec2(0.1,0.1);
	
	
	gl_FragColor = vec4( 0.15 * coul2(p.x, p.y) , 1.0);
}


