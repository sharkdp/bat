# Profile-guided builds

Profile-guided optimization (PGO) uses measurements from representative runs to
guide compiler decisions. The resulting binary still accepts ordinary bat inputs
and options. Performance depends on the training files and how the binary is used;
measure the workloads that matter to you.

The optional [build script](../scripts/build-pgo.py) follows the
[Rust compiler's PGO workflow](https://doc.rust-lang.org/rustc/profile-guided-optimization.html).
It requires Python 3, a Rust toolchain, and the matching LLVM tools:

```sh
rustup component add llvm-tools-preview
python3 scripts/build-pgo.py --benchmark src/printer.rs Cargo.toml README.md
```

Without filenames, the script uses those three repository files. Supply examples
of the languages and file sizes you normally read. Each file is used in both
highlighted and plain modes. `--training-runs` controls repetition; its default is
three. `--offline` uses Cargo's cached dependencies.

The script builds for the compiler's native host target with bat's normal release
settings and default features. It preserves additional compiler flags supplied via
`RUSTFLAGS` or `CARGO_ENCODED_RUSTFLAGS`. An explicit target keeps instrumentation
out of Cargo's build scripts. Compiler arguments use absolute, individually encoded
profile paths, including when the work directory contains spaces.

Each invocation creates its own directory under `target/pgo`, or under the path
given by `--work-dir`. The directory contains build logs, raw profiles, merged
profile data, and the optimized executable. Existing runs are retained. The final
output prints the executable and `report.json` paths.

Training records output hashes. The optimized binary must reproduce those hashes
before the script reports success. Keep the inputs and their Git status unchanged
throughout a run, because the highlighted workload includes Git decorations.
Configuration files and custom assets are
disabled for consistent training and comparison; the generated executable retains
its normal configuration and custom-asset support.

`--benchmark` also builds an ordinary release and checks its output against the
training output. It warms both binaries, alternates their execution order, and
records individual timings and medians for each file and display mode. Use
`--benchmark-runs` to change the default ten repetitions. A reported ratio greater
than one means the optimized binary was faster in that measurement. Small timing
differences can be noise, especially for short files.

The report records the compiler version, target, flags, input paths, output hashes,
and raw benchmark results so a measurement can be reproduced. Rebuild and retrain
after changing the compiler, source, release settings, or representative workload.
The script enables LLVM's missing-profile warnings in the optimized build log.
