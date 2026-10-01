namespace example {
    template <typename T>
    __global__ void assign(T *values, T value) {
        values[threadIdx.x] = value;
    }
}

void launch(int *values) {
    example::assign<int><<<1, 32>>>(values, 42);
    example::assign<<<1, 32>>>(values, 42);
    example::assign<<<
        1,
        dim3(32, 1, 1),
        (4 * sizeof(int)),
        0
    >>>(values, 42);
    int comparison = 1 << 2;
    comparison >>= 1;
    const char *operator_text = "<<< >>> << >>";
}
