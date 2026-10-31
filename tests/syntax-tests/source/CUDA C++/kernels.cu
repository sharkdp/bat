#include <cuda_runtime.h>

__constant__ float multiplier;
__managed__ int total;

__global__ __launch_bounds__(128, 2)
void scale(const float *input, float *output, int size) {
    __shared__ float scratch[128];
    int index = blockIdx.x * blockDim.x + threadIdx.x;
    scratch[threadIdx.x] = index < size ? input[index] : 0.0f;
    __syncthreads();
    if (index < size) {
        output[index] = scratch[threadIdx.x] * multiplier;
        atomicAdd(&total, 1);
    }
}

__host__ __device__ __forceinline__ float square(float value) {
    return value * value;
}

void launch(const float *input, float *output, int size, cudaStream_t stream) {
    dim3 block(128, 1, 1);
    dim3 grid((size + block.x - 1) / block.x, 1, 1);
    scale<<<grid, block, 0, stream>>>(input, output, size);
    scale<<<dim3(4), dim3(32)>>>(input, output, size);
    const char *literal = "__global__ threadIdx __syncthreads() <<< >>>";
    const char *raw = R"cuda(__shared__ blockIdx atomicAdd() <<< >>>)cuda";
    // __device__ dim3 threadIdx __syncthreads() <<< >>>
    /* __global__ float4 atomicAdd() <<< >>> */
}
