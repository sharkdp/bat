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
    for path_old in glob.glob(path.join(root_old, "*", "*")):
        filename = path.basename(path_old)
        dirname = path.basename(path.dirname(path_old))

        path_new = path.join(root_new, dirname, filename)

        print("\n========== {}/{}".format(dirname, filename))

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
    print()

    return has_changes


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
