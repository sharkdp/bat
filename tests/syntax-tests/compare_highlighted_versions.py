#!/usr/bin/env python3

import glob
import sys
import os.path as path
import difflib
import argparse


def compare_highlighted_versions(root_old, root_new):
    print("Comparing the following directories:")
    print(" -", root_old)
    print(" -", root_new)
    has_changes = False
    # Used to check for newly added files that don't have a test
    unknown_files = {strip_root(p) for p in glob.glob(path.join(root_new, "*", "*"))}

    for path_old in glob.glob(path.join(root_old, "*", "*")):
        rel_path = strip_root(path_old)
        unknown_files.discard(rel_path)
        path_new = path.join(root_new, rel_path)

        print("\n========== {}".format(rel_path))

        with open(path_old) as file_old:
            lines_old = file_old.readlines()

        with open(path_new) as file_new:
            lines_new = file_new.readlines()

        diff = difflib.unified_diff(
            lines_old, lines_new, fromfile=path_old, tofile=path_new
        )

        file_has_changes = False
        for line in diff:
            print(line, end="")
            file_has_changes = True

        if file_has_changes:
            has_changes = True
        else:
            print("No changes")

    for f in unknown_files:
        print("\n========== {}: No fixture for this language, run update.sh".format(f))
        has_changes = True

    print()
    return has_changes


def strip_root(p: str) -> str:
    filename = path.basename(p)
    dirname = path.basename(path.dirname(p))
    return path.join(dirname, filename)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="This script compares two directories that were created "
        "by 'create_highlighted_versions.py'."
    )
    parser.add_argument(
        "OLD", help="Path to the old (stored) version of the highlighted output",
    )
    parser.add_argument(
        "NEW", help="Path to the new version of the highlighted output",
    )

    args = parser.parse_args()

    if compare_highlighted_versions(args.OLD, args.NEW):
        print("Error: files with changes have been found")
        sys.exit(1)
    else:
        print("Directories are the same")
