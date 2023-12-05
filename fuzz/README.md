# Fuzz Testing
Fuzz testing is a software testing technique that involves providing invalid or
incomplete input to a program and observing its behavior to identify potential
bugs or vulnerabilities. The goal of fuzz testing is to simulate real-world
scenarios where users may provide incorrect or unexpected data, and test the
robustness of the software in these situations. Fuzz testing can help identify
issues such as buffer overflows, null pointer dereferences, and input validation
flaws that could lead to security vulnerabilities. By extensively testing fuzz
inputs, developers can improve the overall reliability and stability of their
software.

## Quickstart
To start fuzzing bat you will need to first install `cargo-fuzz`.

`$ cargo install cargo-fuzz`

To get a list of the available fuzzing harnesses simply run;
```
$ cargo fuzz list
fuzz_buffer
```

To run a fuzz testing harness you will need to run;

```
$ cargo +nightly fuzz run fuzz_buffer
```

You should expect the fuzzer to run indefinetely or until;

- It finds a bug and crashes
- You manually kill the fuzzer e.g. ctrl-C
- You manually set a timeout from the command line. See the help menu for
  details. e.g.

```
$ cargo +nightly fuzz run fuzz_buffer -- -help=1
```

By default libfuzzer/cargo-fuzz will fuzz on a single core. To fuzz in paralell
you can use the fork command e.g.

```
# 8 cores.
$ cargo +nightly fuzz run fuzz_buffer -- -fork=8
# However many cores you have.
$ cargo +nightly fuzz run fuzz_buffer -- -fork=$(nproc)
```

## Other resources
- The [cargo-fuzz book](https://rust-fuzz.github.io/book/cargo-fuzz.html).
- The [llvm libfuzzer docs](https://www.llvm.org/docs/LibFuzzer.html).

