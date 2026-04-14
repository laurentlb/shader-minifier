#include <metal_stdlib>
using namespace metal;

struct Ray {
    float3 origin;
    float3 dir;
};

// Output parameter passed by thread reference. Writes through `r` must be
// preserved by the minifier - the function's only observable behaviour
// is the side effect through the ref.
static void make_ray(thread Ray &r, float3 o, float3 d)
{
    r.origin = o;
    r.dir = d;
}

kernel void ray_kernel(
    device float *dst [[buffer(0)]],
    uint tid [[thread_position_in_grid]])
{
    Ray r;
    make_ray(r, float3(1.0, 2.0, 3.0), float3(0.0, 0.0, 1.0));
    dst[tid] = r.origin.x + r.dir.z;
}
