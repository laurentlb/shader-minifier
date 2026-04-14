#include <metal_stdlib>
using namespace metal;

kernel void array_kernel(
    device float *dst [[buffer(0)]],
    uint tid [[thread_position_in_grid]])
{
    // Array-typed const local used once: must NOT be inlined into `{..}[i]`.
    const float weights[4] = { 0.1, 0.2, 0.3, 0.4 };
    dst[tid] = weights[tid & 3];
}
