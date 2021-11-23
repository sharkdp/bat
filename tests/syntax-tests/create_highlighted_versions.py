#!/usr/bin/env python3

import subprocess
import glob
import sys
import os.path as path
import os
import argparse
from multiprocessing import Pool

BAT_OPTIONS = [
    "--no-config",
    "--style=plain",
    "--color=always",
    "--theme=default",
    "--italic-text=always",
]

SKIP_FILENAMES = [
    "LICENSE.md",
    "NOTICE",
    "README.md",
    "bat_options",
]


def get_options(source):
    options = BAT_OPTIONS.copy()

    source_dirpath = path.dirname(source)
    options_file = path.join(source_dirpath, "bat_options")
    try:
        with open(options_file, "r") as f:
            options.extend(map(lambda x: x.rstrip(), f.readlines()))
    except FileNotFoundError:
        pass

    return options


def create_highlighted_version(args):
    output_basepath, source = args
    env = os.environ.copy()
    env.pop("BAT_CACHE_PATH", None)
    env.pop("BAT_CONFIG_DIR", None)
    env.pop("BAT_CONFIG_PATH", None)
    env.pop("BAT_OPTS", None)
    env.pop("BAT_PAGER", None)
    env.pop("BAT_STYLE", None)
    env.pop("BAT_TABS", None)
    env.pop("BAT_THEME", None)
    env.pop("NO_COLOR", None)
    env.pop("PAGER", None)
    env["COLORTERM"] = "truecolor"  # make sure to output 24bit colors

    source_dirname = path.basename(path.dirname(source))
    source_filename = path.basename(source)

    if source_filename in SKIP_FILENAMES:
        return

    bat_output = subprocess.check_output(
        ["bat"] + get_options(source) + [source],
        stderr=subprocess.PIPE,
        env=env,
    )

    output_dir = path.join(output_basepath, source_dirname)
    output_path = path.join(output_dir, source_filename)

    os.makedirs(output_dir, exist_ok=True)

    with open(output_path, "wb") as output_file:
        output_file.write(bat_output)

    print("Created '{}'".format(output_path))


def create_highlighted_versions(output_basepath):
    root = os.path.dirname(os.path.abspath(__file__))
    source_paths = path.join(root, "source", "*")

    sources = []
    for source in glob.glob(path.join(source_paths, "*")) + glob.glob(
        path.join(source_paths, ".*")
    ):
        sources.append((output_basepath, source))

    try:
        with Pool() as p:
            p.map(create_highlighted_version, sources)
    except subprocess.CalledProcessError as err:
        print(
            "=== Error: Could not highlight source file:\n" + " ".join(err.cmd),
            file=sys.stderr,
        )
        print(
            "=== bat stdout:\n{}".format(err.stdout.decode("utf-8")),
            file=sys.stderr,
        )
        print(
            "=== bat stderr:\n{}".format(err.stderr.decode("utf-8")),
            file=sys.stderr,
        )
        return False
    except FileNotFoundError:
        print(
            "Error: Could not execute 'bat'. Please make sure that the executable "
            "is available on the PATH."
        )
        return False

    return True


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="This script creates syntax-highlighted versions of all "
        "files in the 'source' directory."
    )
    parser.add_argument(
        "--output",
        "-O",
        metavar="PATH",
        help="Output directory",
        required=True,
        type=str,
    )

    args = parser.parse_args()

    if not create_highlighted_versions(output_basepath=args.output):
        sys.exit(1)
