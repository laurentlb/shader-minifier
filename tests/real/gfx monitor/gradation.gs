// VertexProgram

varying vec4 p;

void main()
{
	gl_Position = gl_Vertex;
	p = gl_Vertex;
}

