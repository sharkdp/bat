#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail

test_file="tests/syntax-tests/source/BatTestCustomAssets/NoColorsUnlessCustomAssetsAreUsed.battestcustomassets"

# First make sure our test syntax is not part of integrated syntaxes
echo "Testing that BatTestCustomAssets is an unknown syntax:"
bat -f --language BatTestCustomAssets "${test_file}" &&
    echo "should have failed because of unknown syntax" &&
    exit 1

# Now build custom assets and include our test syntax
# We do as instructed to regular users at
# https://github.com/sharkdp/bat/blob/master/README.md#adding-new-syntaxes--language-definitions

# Create dir to put our test syntax
custom_syntaxes_dir="$(bat --config-dir)/syntaxes"
mkdir -p "${custom_syntaxes_dir}"

# Put the syntax in place
cp tests/syntax-tests/BatTestCustomAssets.sublime-syntax "${custom_syntaxes_dir}/BatTestCustomAssets.sublime-syntax"

# Build custom assets to include the above syntax
echo "Building custom assets to include the BatTestCustomAssets syntax"
bat cache --build

# Now bat shall not fail when the syntax is used. If it does not fail, we can be reasonably sure
# that the custom assets functionality is working
echo "Testing that BatTestCustomAssets is a KNOWN syntax:"
bat -f --language BatTestCustomAssets "${test_file}" ||
    (echo "command shall not have failed this time, but did" &&
        exit 1)

# While we're at it  and for extra safety in the test, we also
# make sure that the --no-custom-assets flag work as intended
echo "Testing that BatTestCustomAssets is an unknown syntax when using --no-custom-assets:"
bat -f --no-custom-assets --language BatTestCustomAssets "${test_file}" &&
    echo "should have failed because of unknown syntax via --no-custom-assets" &&
    exit 1

# We clean up after ourselves to reduce risk of problems later
bat cache --clear
rm -rf "${custom_syntaxes_dir}"
