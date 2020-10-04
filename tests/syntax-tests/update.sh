#!/usr/bin/env bash

python="python3"
if ! command -v python3 &>/dev/null; then python="python"; fi
"$python" create_highlighted_versions.py -O highlighted
