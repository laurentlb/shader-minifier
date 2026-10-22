// Initializer lists can have a trailing comma.
static const float2 positions[3] =
{
	{ -1.0, 1.0 },
	{ 3.0, 1.0 },
	{ -1.0, -3.0, },
};

float4 VS(uint id : SV_VertexID) : SV_POSITION
{
	return float4(positions[id], 0.0, 1.0);
}
