import time
import os
import subprocess as sp


# Threshold speed in chars per second
THRESHOLD_SPEED = 20000

# Maximum time we allow `bat` to run
BAT_TIMEOUT_SEC = 10

# Maximum number of files to measure
MAX_NUM_FILES = 100

# Root folder for the search
SEARCH_ROOT = os.getenv("HOME")


def find_slow_files(startup_time, language, glob_pattern):
    out = sp.check_output(
        [
            "fd",
            "--hidden",
            "--no-ignore",
            "--type=file",
            "--max-results",
            str(MAX_NUM_FILES),
            "--glob",
            glob_pattern,
            SEARCH_ROOT,
        ]
    )

    paths = out.split(b"\n")[:-1]
    print(f"Language: {language}, glob pattern: {glob_pattern} ({len(paths)} matches)")

    for path in paths:
        num_chars = int(sp.check_output(["wc", "-c", path]).split(b" ")[0].decode())

        if num_chars < 500:
            # It is hard to measure the exact speed for short files
            continue

        try:
            start = time.time()
            sp.check_output(["bat", "--color=always", path], timeout=BAT_TIMEOUT_SEC)
            duration = time.time() - start - startup_time

            if duration <= 0:
                continue

            highlighting_speed = num_chars / duration

            if highlighting_speed < THRESHOLD_SPEED:
                print(f"  {highlighting_speed:10.0f} chars/s:  {path.decode()}")

        except sp.CalledProcessError:
            print(f"  Error while highlighting file '{path.decode()}'.")

        except sp.TimeoutExpired:
            if num_chars < THRESHOLD_SPEED * BAT_TIMEOUT_SEC:
                print(f"  Error: bat timed out on file '{path.decode()}'.")
            else:
                print(
                    f"  Warning: bat timed out on file '{path.decode()} (but the file is large)."
                )


def measure_bat_startup_speed():
    min_duration = None
    for _ in range(20):
        start = time.time()
        p = sp.Popen(
            ["bat", "--color=always", "--language=py"], stdin=sp.PIPE, stdout=sp.PIPE
        )
        p.communicate(input=b"test")
        duration = time.time() - start

        if not min_duration or duration < min_duration:
            min_duration = duration

    return min_duration


def main():
    print("Measuring 'bat' startup speed ... ", flush=True, end="")
    startup_time = measure_bat_startup_speed()
    print(f"{startup_time * 1000:.1f} ms")

    output = sp.check_output(["bat", "--list-languages"]).decode()

    for line in output.strip().split("\n"):
        language, extensions = line.split(":")
        for ext in extensions.split(","):
            find_slow_files(startup_time, language, ext)
            if not ext.startswith("."):
                find_slow_files(startup_time, language, f"*.{ext}")


if __name__ == "__main__":
    main()
