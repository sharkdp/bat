#ifndef CUDA_VECTORS_CUH
#define CUDA_VECTORS_CUH

#include <cuda_runtime.h>

template <typename T>
struct DeviceArray {
    T *data;
    int size;
    __device__ T at(int index) const { return data[index]; }
};

__device__ float4 shift(float4 value, int2 offset) {
    value.x += offset.x;
    value.y += offset.y;
    return value;
}

__device__ unsigned int active_threads() {
    return __syncthreads_count(threadIdx.x < warpSize);
}

__device__ double rounded(double value) {
    return __dadd_rn(value, 1.0);
}

__device__ unsigned long long ticks() {
    return clock64();
}

#endif
