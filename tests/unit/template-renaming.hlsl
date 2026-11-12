// Template types in function bodies, with renaming enabled.
// Variables used as template arguments must not be inlined or removed.
// The value arguments of templates must be renamed consistently with their declaration.
static const int Size = 3;
static const float Scale = 2.0;

template <typename T>
struct Wrapper {
  T value;
};

float2 local(float2 p) {
  const int N = 2;
  vector<float, N> v = p * 2.0;
  return v * v.y;
}

float4 main(float4 pos : SV_POSITION, float3x3 rot : ROTATION) : SV_TARGET {
  vector<float, Size> v = pos.xyz * Scale;
  matrix<float, Size, Size> m = rot;
  Wrapper<vector<float, Size> > w;
  w.value = mul(m, v);
  return float4(w.value + v, local(pos.xy).x);
}
