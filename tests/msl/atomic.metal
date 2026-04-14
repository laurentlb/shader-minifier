#include <metal_stdlib>
using namespace metal;

kernel void atomic_kernel(
    device atomic_uint *counter [[buffer(0)]],
    device uint *dst [[buffer(1)]],
    uint tid [[thread_position_in_grid]])
{
    // This call MUST survive minification: atomic_fetch_add_explicit is a
    // write through `counter`. A broken purity model would drop it because
    // its return value is unused.
    atomic_fetch_add_explicit(counter, 1u, memory_order_relaxed);

    // Also must survive: store through a device pointer via explicit call.
    atomic_store_explicit(counter, 0u, memory_order_relaxed);

    dst[tid] = atomic_load_explicit(counter, memory_order_relaxed);
}
