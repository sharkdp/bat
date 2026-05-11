#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")" || exit

python="python3"
if ! command -v python3 &>/dev/null; then python="python"; fi
"$python" create_highlighted_versions.py -O highlighted
