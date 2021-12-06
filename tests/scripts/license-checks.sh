#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail

# Make sure that we don't accidentally include GPL licenced files
gpl_term="General Public License"
gpl_excludes=(
    # Snippet expands to GPL, but is not under GPL
    ":(exclude)assets/syntaxes/01_Packages/Matlab/Snippets/Octave-function.sublime-snippet"

    # 'gpl_term' matches itself :D
    ":(exclude)tests/scripts/license-checks.sh"

    # Contains a reference to GPL, but is not under GPL
    ":(exclude)tests/syntax-tests/source/Java Server Page (JSP)/LICENSE.md"
)
gpl_occurances=$(git grep --recurse-submodules "${gpl_term}" -- "${gpl_excludes[@]}" || true)

if [ -z "${gpl_occurances}" ]; then
    echo "PASS: No files under GPL were found"
else
    echo "FAIL: GPL:ed code is not compatible with bat, but occurances of '${gpl_term}' were found:"
    echo "${gpl_occurances}"
    exit 1
fi
