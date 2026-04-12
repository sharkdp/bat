#!/usr/bin/env python3

import itertools
import subprocess
import pathlib
import shutil
from typing import Iterable


def generate_snapshots():
    single_styles = ["changes", "grid", "header", "numbers", "rule"]
    collective_styles = ["full", "plain"]

    for num in range(len(single_styles)):
        for grouped in itertools.combinations(single_styles, num + 1):
            generate_style_snapshot(",".join(grouped))

    for style in collective_styles:
        generate_style_snapshot(style)


def generate_style_snapshot(style):
    generate_snapshot(style.replace(",", "_"), ["--style={}".format(style)])


def generate_snapshot(name: str, arguments: Iterable[str]):
    output_file = "output/{name}.snapshot.txt".format(name=name)
    command = [
        "cargo", "run", "--", "--paging=never", "--color=never",
        "--decorations=always", *arguments, "sample.rs"
    ]
    print("generating snapshot for {}".format(name))
    with open(output_file, "w") as f:
        subprocess.call(command, stdout=f)


def build_bat():
    print("building bat")
    subprocess.call(["cargo", "build"], cwd="../..")


def prepare_output_dir():
    shutil.rmtree("output", ignore_errors=True)
    pathlib.Path("output").mkdir()


def modify_sample_file():
    print("modifying sample.rs to show changes")
    shutil.copyfile("sample.modified.rs", "sample.rs")


def undo_sample_file_modification():
    print("undoing sample.rs modifications")
    subprocess.call(["git", "checkout", "--", "sample.rs"])


build_bat()
prepare_output_dir()
modify_sample_file()
generate_snapshots()
undo_sample_file_modification()
