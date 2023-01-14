float f(){return 0.;}
void main()
{
	float c;
	for (float a=0; a<50.; ++a)
	{
		c+=cos(a);
	}
	float b;
	for (b=0; b<50.; ++b)
	{
		c+=cos(c);
	}
	b = a;
	while (b<50.)
	{
		c+=cos(c);
		b++;
	}
	b = a;
	while (b<50.)
	{
		b++;
	}
	b = a;
	while (b<50.)
	{
		c+=cos(c);
		float d=f(); // d prevents moving b+=d to a for
		b+=d;
	}
	b = a;
	while (b<50.)
	{
		if (a < b) continue; // continue prevents moving b++ to a for
		b++;
	}
	b = a;
	while (b<50.)
	{
		if (a < b) a=b; else continue; // continue prevents moving b++ to a for
		b++;
	}
	for (; b<50.; ++b)
	{
		c+=cos(b);
	}
}