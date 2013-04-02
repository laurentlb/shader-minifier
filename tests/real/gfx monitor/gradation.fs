// FragmentProgram

varying vec4 p;

void main()
{
	float g = p.y * 0.5 + 0.5;
	gl_FragColor = vec4(g,g,g,0);
	return;
}