// VertexProgram
// This program is 16:10 ratio

varying vec3 org,dir;
void main()
{
	gl_Position=gl_Vertex;
	org=vec3(0,0,0);
	dir=normalize(-vec3(-gl_Vertex.x*1.6,-gl_Vertex.y,1));
}