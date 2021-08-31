#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail

actual_themes="$(mktemp -d)/actual_themes.txt"
bat --color=never --list-themes > "${actual_themes}"

if diff -u "tests/syntax-tests/expected_themes.txt" "${actual_themes}"; then
    echo "PASS: All themes are present!"
else
    echo "FAIL: Not all themes are present! See diff above."
    exit 1
fi
