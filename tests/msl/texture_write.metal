#include <metal_stdlib>
using namespace metal;

kernel void tex_kernel(
    texture2d<float, access::write> dst [[texture(0)]],
    uint2 gid [[thread_position_in_grid]])
{
    // Method-syntax call with discarded return value. Dot-callee FunCalls
    // fall through to impure in Effects.sideEffects \u2014 must survive.
    dst.write(float4(float(gid.x) / 256.0, float(gid.y) / 256.0, 0.0, 1.0), gid);
}
