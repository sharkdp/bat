#!/usr/bin/env python3

import itertools
import subprocess
import pathlib
import shutil


def generate_snapshots():
    single_styles = ["changes", "grid", "header", "numbers"]
    collective_styles = ["full", "plain"]

    for num in range(len(single_styles)):
        for grouped in itertools.combinations(single_styles, num + 1):
            generate_style_snapshot(",".join(grouped))

    for style in collective_styles:
        generate_style_snapshot(style)


def generate_style_snapshot(style):
    generate_snapshot(style.replace(",", "_"), "--style={}".format(style))


def generate_snapshot(name, arguments):
    command = "cargo run -- --paging=never --color=never --decorations=always "
    command += "{args} sample.rs > output/{name}.snapshot.txt".format(
        name=name,
        args=arguments
    )
    print("generating snapshot for {}".format(name))
    subprocess.call(command, shell=True)


def build_bat():
    print("building bat")
    subprocess.call("cargo build", cwd="../..", shell=True)


def prepare_output_dir():
    shutil.rmtree("output", ignore_errors=True)
    pathlib.Path("output").mkdir()


def modify_sample_file():
    print("modifying sample.rs to show changes")
    shutil.copyfile("sample.modified.rs", "sample.rs")


def undo_sample_file_modification():
    print("undoing sample.rs modifications")
    subprocess.call("git checkout -- sample.rs", shell=True)


build_bat()
prepare_output_dir()
modify_sample_file()
generate_snapshots()
undo_sample_file_modification()
