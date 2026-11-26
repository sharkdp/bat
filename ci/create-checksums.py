#!/usr/bin/env python3
"""Write portable SHA-256 sidecars for release archives and packages."""

import argparse
import hashlib
from pathlib import Path


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("files", nargs="+", type=Path)
    args = parser.parse_args()
    for path in args.files:
        digest = hashlib.sha256()
        with path.open("rb") as source:
            for block in iter(lambda: source.read(1024 * 1024), b""):
                digest.update(block)
        checksum = path.with_name(path.name + ".sha256")
        checksum.write_text(f"{digest.hexdigest()}  {path.name}\n", encoding="utf-8", newline="\n")


if __name__ == "__main__":
    main()
