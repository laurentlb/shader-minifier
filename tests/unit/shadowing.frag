float f(float g)
{
	float f = 0.0;
	f += f++ + ++f;
	return f;
}
float g(float f)
{
	{
		f++;
		float f = f + 1.0, g = 9.0;
		f+=g;
	}
	return f;
}