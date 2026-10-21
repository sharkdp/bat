CUDA rules are adapted from [CUDA C++ for Sublime Text](https://github.com/harrism/sublimetext-cuda-cpp)
at commit `2bf66d15322717284a6d101f99ec8b72c98c1c1d` (`cuda-c++.JSON-tmLanguage`),
under the accompanying NVIDIA BSD license. The syntax meets the download
criterion on [Package Control](https://packagecontrol.io/packages/CUDA%20C%2B%2B).

`build.sh`, called by `assets/create.sh`, derives a native syntax from the pinned
Sublime Packages C++ grammar. The initial base is commit
`759d6eed9b4beed87e602a23303a121c3a6c2fb3`. Its permissive license is included here.
The generated file is removed after building the cache and is not committed.

The original grammar includes C++ before the CUDA patterns. That prevents the
CUDA additions from matching inside the nested contexts used by bat's C++
grammar. `cuda.patch` instead places qualifiers, vector types, built-in variables,
intrinsics and launch delimiters in the corresponding C++ contexts. It also fixes
the original `printf` regex and preserves C++ parsing inside launch arguments.
Comments and strings keep their own contexts. Only `.cu` and `.cuh` are registered;
ordinary C++ mappings and the C++ grammar itself are unchanged.

When updating Sublime Packages, verify that `build.sh` applies without fuzz,
review the resulting CUDA syntax and regenerate the CUDA syntax fixtures.
