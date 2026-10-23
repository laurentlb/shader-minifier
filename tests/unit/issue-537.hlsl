// With --preserve-all-globals, local variables must not reuse the global names (#537).
// There are more than 26 globals, so that all the candidate names would be long.
cbuffer Params : register(b0)
{
	float4 gParam01;
	float4 gParam02;
	float4 gParam03;
	float4 gParam04;
	float4 gParam05;
	float4 gParam06;
	float4 gParam07;
	float4 gParam08;
	float4 gParam09;
	float4 gParam10;
	float4 gParam11;
	float4 gParam12;
	float4 gParam13;
	float4 gParam14;
	float4 gParam15;
	float4 gParam16;
	float4 gParam17;
	float4 gParam18;
	float4 gParam19;
	float4 gParam20;
	float4 gParam21;
	float4 gParam22;
	float4 gParam23;
	float4 gParam24;
	float4 gParam25;
	float4 gParam26;
	float4 gParam27;
	float4 gParam28;
};

float2x2 rotate2D(float a)
{
	float s = sin(a), c = cos(a);
	return float2x2(c, -s, s, c);
}

float4 main(float4 pos : POSITION) : SV_Position
{
	float2 p = mul(rotate2D(pos.w), pos.xy);
	return float4(p, pos.zw) * gParam01 + gParam28;
}
