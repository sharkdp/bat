#!/usr/bin/env bash
set -euo pipefail

CUDA_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CPP_SOURCE="$CUDA_DIR/../../01_Packages/C++/C++.sublime-syntax"
CUDA_STAGE="$(mktemp "$CUDA_DIR/.cuda-build.XXXXXX")"
trap 'rm -f "$CUDA_STAGE"' EXIT

# Copy the pinned C++ grammar so nested expressions retain the CUDA additions.
# A top-level include of C++ alone cannot customize its pushed contexts.
cp "$CPP_SOURCE" "$CUDA_STAGE"
patch --batch --fuzz=0 "$CUDA_STAGE" < "$CUDA_DIR/cuda.patch"
mv "$CUDA_STAGE" "$CUDA_DIR/CUDA.sublime-syntax"
