#!/usr/bin/env python3
"""Build bat with profile-guided optimization using representative input files."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import statistics
import subprocess
import sys
import tempfile
import time


ROOT = Path(__file__).resolve().parent.parent
MODES = {
    "highlighted": ["--color=always", "--decorations=always", "--style=full"],
    "plain": ["--color=never", "--decorations=never", "--style=plain"],
}


def positive(value):
    number = int(value)
    if number < 1:
        raise argparse.ArgumentTypeError("must be at least 1")
    return number


def run(command, **kwargs):
    return subprocess.run(command, check=True, **kwargs)


def tool_output(command):
    return subprocess.check_output(command, cwd=ROOT, text=True).strip()


def compiler_flags():
    # Match Cargo's whitespace splitting for RUSTFLAGS, while preserving the
    # encoded form exactly. Generated paths remain single compiler arguments.
    encoded = os.environ.get("CARGO_ENCODED_RUSTFLAGS")
    if encoded is not None:
        flags = encoded.split("\x1f") if encoded else []
    else:
        flags = os.environ.get("RUSTFLAGS", "").split()
    if any("profile-generate" in flag or "profile-use" in flag for flag in flags):
        raise ValueError("remove existing PGO flags before starting a new training run")
    return flags


def build(stage, flags, args, host, directory):
    env = os.environ.copy()
    env.pop("RUSTFLAGS", None)
    env["CARGO_ENCODED_RUSTFLAGS"] = "\x1f".join(flags)
    env["CARGO_TARGET_DIR"] = str(directory / "cargo")
    env["CARGO_INCREMENTAL"] = "0"
    command = ["cargo", "build", "--locked", "--release", "--bin", "bat", "--target", host]
    if args.offline:
        command.append("--offline")
    print(f"Building {stage}; log: {directory / (stage + '.log')}", flush=True)
    with (directory / (stage + ".log")).open("wb") as log:
        run(command, cwd=ROOT, env=env, stdout=log, stderr=subprocess.STDOUT)
    suffix = ".exe" if os.name == "nt" else ""
    binary = directory / ("bat-" + stage + suffix)
    shutil.copy2(directory / "cargo" / host / "release" / ("bat" + suffix), binary)
    return binary


def display_environment(profiles=None):
    env = {key: value for key, value in os.environ.items() if not key.startswith("BAT_")}
    for key in ("NO_COLOR", "PAGER", "LESSOPEN", "LESSCLOSE", "LLVM_PROFILE_FILE"):
        env.pop(key, None)
    env["COLORTERM"] = "truecolor"
    if profiles is not None:
        env["LLVM_PROFILE_FILE"] = str(profiles / "%p-%m.profraw")
    return env


def display_command(binary, mode, source):
    return [str(binary), "--no-config", "--no-custom-assets", "--paging=never",
            "--theme=Monokai Extended", "--terminal-width=100", *MODES[mode], "--", str(source)]


def digest(binary, mode, source, env):
    # Hash incrementally so output checks also work for large training files.
    with subprocess.Popen(display_command(binary, mode, source), stdout=subprocess.PIPE,
                          env=env, cwd=ROOT) as child:
        result = hashlib.sha256()
        while True:
            block = child.stdout.read(65536)
            if not block:
                break
            result.update(block)
        if child.wait() != 0:
            raise subprocess.CalledProcessError(child.returncode, child.args)
    return result.hexdigest()


def benchmark(baseline, optimized, files, count):
    env = display_environment()
    results = []
    for source in files:
        for mode in MODES:
            samples = {"baseline": [], "optimized": []}
            binaries = {"baseline": baseline, "optimized": optimized}
            for binary in binaries.values():
                run(display_command(binary, mode, source), env=env, cwd=ROOT,
                    stdout=subprocess.DEVNULL)
            for iteration in range(count):
                order = ["baseline", "optimized"] if iteration % 2 == 0 else ["optimized", "baseline"]
                for label in order:
                    start = time.perf_counter()
                    run(display_command(binaries[label], mode, source), env=env, cwd=ROOT,
                        stdout=subprocess.DEVNULL)
                    samples[label].append(time.perf_counter() - start)
            medians = {label: statistics.median(values) for label, values in samples.items()}
            ratio = medians["baseline"] / medians["optimized"]
            results.append({"file": str(source), "mode": mode, "seconds": samples,
                            "median_seconds": medians, "speedup": ratio})
            print(f"{source.name} ({mode}): {ratio:.3f}x baseline/optimized", flush=True)
    return results


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("files", nargs="*", type=Path, help="representative files for training")
    parser.add_argument("--work-dir", type=Path, default=ROOT / "target" / "pgo")
    parser.add_argument("--training-runs", type=positive, default=3)
    parser.add_argument("--benchmark", action="store_true", help="also build and measure an ordinary release")
    parser.add_argument("--benchmark-runs", type=positive, default=10)
    parser.add_argument("--offline", action="store_true", help="use Cargo's cached dependencies")
    args = parser.parse_args()
    files = [path.resolve() for path in (args.files or [ROOT / "src/printer.rs", ROOT / "Cargo.toml", ROOT / "README.md"])]
    for path in files:
        if not path.is_file():
            parser.error(f"input is not a regular file: {path}")
    rustc = os.environ.get("RUSTC", "rustc")
    version = tool_output([rustc, "-vV"])
    host = next(line.split(": ", 1)[1] for line in version.splitlines() if line.startswith("host: "))
    sysroot = Path(tool_output([rustc, "--print", "sysroot"]))
    profdata = sysroot / "lib" / "rustlib" / host / "bin" / ("llvm-profdata.exe" if os.name == "nt" else "llvm-profdata")
    if not profdata.is_file():
        parser.error("llvm-profdata is missing; install it with: rustup component add llvm-tools-preview")
    flags = compiler_flags()
    work = args.work_dir.resolve()
    work.mkdir(parents=True, exist_ok=True)
    # Each run owns fresh profile data; existing profiles and binaries are kept.
    directory = Path(tempfile.mkdtemp(prefix="run-", dir=work))
    profiles = directory / "profiles"
    profiles.mkdir()
    print(f"Run directory: {directory}", flush=True)
    baseline = build("baseline", flags, args, host, directory) if args.benchmark else None
    instrumented = build("instrumented", flags + [f"-Cprofile-generate={profiles}"], args, host, directory)
    expected = {}
    env = display_environment(profiles)
    print("Training and recording output hashes", flush=True)
    for iteration in range(args.training_runs):
        for source in files:
            for mode in MODES:
                value = digest(instrumented, mode, source, env)
                key = (str(source), mode)
                if key in expected and expected[key] != value:
                    raise ValueError(f"input or output changed during training: {source}")
                expected[key] = value
    if not list(profiles.glob("*.profraw")):
        raise ValueError("training produced no raw profiles")
    merged = directory / "merged.profdata"
    run([str(profdata), "merge", "-o", str(merged), str(profiles)], cwd=ROOT)
    optimized = build("optimized", flags + [f"-Cprofile-use={merged}", "-Cllvm-args=-pgo-warn-missing-function"],
                      args, host, directory)
    for source in files:
        for mode in MODES:
            for binary in [optimized] + ([baseline] if baseline else []):
                if digest(binary, mode, source, display_environment()) != expected[(str(source), mode)]:
                    raise ValueError(f"output differs for {source} in {mode} mode")
    report = {"rustc": version, "host": host, "compiler_flags": flags,
              "files": [str(path) for path in files], "training_runs": args.training_runs,
              "optimized_binary": str(optimized), "profile": str(merged),
              "output_hashes": [{"file": path, "mode": mode, "sha256": value}
                                for (path, mode), value in expected.items()],
              "benchmarks": benchmark(baseline, optimized, files, args.benchmark_runs) if baseline else []}
    report_path = directory / "report.json"
    report_path.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print(f"Optimized binary: {optimized}\nReport: {report_path}", flush=True)


if __name__ == "__main__":
    try:
        main()
    except (OSError, ValueError, subprocess.CalledProcessError) as error:
        print(f"build-pgo: {error}", file=sys.stderr)
        sys.exit(1)
