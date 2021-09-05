#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail

### HELPER VARS

custom_syntaxes_dir="$(bat --config-dir)/syntaxes"

installed_custom_syntax="${custom_syntaxes_dir}/BatTestCustomAssets.sublime-syntax"

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
    echo "FAIL: $1"
    exit 1
}

### TEST STEPS

echo_step "TEST: Make sure 'BatTestCustomAssets' is not part of integrated syntaxes"
bat -f "${custom_syntax_args[@]}" &&
    fail_test "EXPECTED: 'unknown syntax' error ACTUAL: no error occured
               HINT: try manually doing 'bat cache --clear' before running tests"

echo_step "PREPARE: Install custom syntax 'BatTestCustomAssets'"
mkdir -p "${custom_syntaxes_dir}"
cp -v "tests/syntax-tests/BatTestCustomAssets.sublime-syntax" "${installed_custom_syntax}"

echo_step "PREPARE: Build custom assets to enable 'BatTestCustomAssets' syntax"
bat cache --build

echo_step "TEST: 'BatTestCustomAssets' is a known syntax"
bat -f "${custom_syntax_args[@]}" ||
    fail_test "EXPECTED: syntax highlighting to work ACTUAL: there was an error"

echo_step "TEST: The 'Rust' syntax is still available"
bat -f "${integrated_syntax_args[@]}" ||
    fail_test "EXPECTED: syntax highlighting to still work for integrated assets ACTUAL: there was an error"

echo_step "TEST: 'BatTestCustomAssets' is an unknown syntax with --no-custom-assets"
bat -f --no-custom-assets "${custom_syntax_args[@]}" &&
    fail_test "EXPECTED: 'unknown syntax' error because of --no-custom-assets, but no error occured"

echo_step "CLEANING: To avoid unwanted side effects of running tests"
bat cache --clear
rm -rfv "${installed_custom_syntax}"
