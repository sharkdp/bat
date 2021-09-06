#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail

### ENVIRONMENT

BAT_CONFIG_DIR=$(mktemp -d)
export BAT_CONFIG_DIR

BAT_CACHE_PATH=$(mktemp -d)
export BAT_CACHE_PATH

echo "
BAT_CONFIG_DIR = ${BAT_CONFIG_DIR}
BAT_CACHE_PATH = ${BAT_CACHE_PATH}
"

### HELPER VARS

custom_syntax_args=(
    "--language=BatTestCustomAssets"
    "tests/syntax-tests/source/BatTestCustomAssets/NoColorsUnlessCustomAssetsAreUsed.battestcustomassets"
)

integrated_syntax_args=(
    "--language=Rust"
    "examples/simple.rs"
)

### HELPER FUNCTIONS

echo_step() {
    echo -e "\n== $1 =="
}

fail_test() {
    echo -e "FAIL: $1"
    exit 1
}

### TEST STEPS

echo_step "TEST: Make sure 'BatTestCustomAssets' is not part of integrated syntaxes"
bat -f "${custom_syntax_args[@]}" &&
    fail_test "EXPECTED: 'unknown syntax' error ACTUAL: no error occured"

echo_step "PREPARE: Install custom syntax 'BatTestCustomAssets'"
custom_syntaxes_dir="$(bat --config-dir)/syntaxes"
mkdir -p "${custom_syntaxes_dir}"
cp -v "tests/syntax-tests/BatTestCustomAssets.sublime-syntax" \
    "${custom_syntaxes_dir}/BatTestCustomAssets.sublime-syntax"

echo_step "PREPARE: Build custom assets to enable 'BatTestCustomAssets' syntax"
bat cache --build

echo_step "TEST: 'BatTestCustomAssets' is a known syntax"
bat -f "${custom_syntax_args[@]}" ||
    fail_test "EXPECTED: syntax highlighting to work ACTUAL: there was an error"

echo_step "TEST: The 'Rust' syntax is still available"
bat -f "${integrated_syntax_args[@]}" ||
    fail_test "EXPECTED: syntax highlighting still works with integrated assets ACTUAL: there was an error"

echo_step "TEST: 'BatTestCustomAssets' is an unknown syntax with --no-custom-assets"
bat -f --no-custom-assets "${custom_syntax_args[@]}" &&
    fail_test "EXPECTED: 'unknown syntax' error because of --no-custom-assets ACTUAL: no error occured"

echo_step "TEST: 'bat cache --clear' removes all files"
bat cache --clear
remaining_files=$(ls -A "${BAT_CACHE_PATH}")
[ -z "${remaining_files}" ] ||
    fail_test "EXPECTED: no files remain ACTUAL: some files remain:\n${remaining_files}"

echo_step "CLEAN"
rm -rv "${BAT_CONFIG_DIR}" "${BAT_CACHE_PATH}"
