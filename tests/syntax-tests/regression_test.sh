#!/usr/bin/env bash

set -eou pipefail

script_directory="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

output_directory=$(mktemp -d)

"$script_directory"/create_highlighted_versions.py --output="$output_directory"

echo

"$script_directory"/compare_highlighted_versions.py \
    "$script_directory/highlighted" \
    "$output_directory"
