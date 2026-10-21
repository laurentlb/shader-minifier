struct MyStruct {
   float3 pos;
};
StructuredBuffer<MyStruct> mySB;
RWStructuredBuffer<MyStruct> myRWSB;
Texture2D<float4> tex;
Buffer<vector<float, 2>> buf;

float4 main() : SV_Target {
   vector<float, 2> v = buf[0];
   myRWSB[0].pos = tex[uint2(0, 0)].xyz;
   return float4(mySB[0].pos, v.x);
}
