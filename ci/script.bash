#!/usr/bin/env bash

set -ex

# Incorporate TARGET env var to the build and test process
cargo build --target "$TARGET" --verbose

# We cannot run arm executables on linux
if [[ $TARGET != arm-unknown-linux-gnueabihf ]] && [[ $TARGET != aarch64-unknown-linux-gnu ]]; then
    cargo test --target "$TARGET" --verbose

    # Disable truecolor (not supported on Travis)
    export COLORTERM=""

    # Run 'bat' on its own source code and the README
    cargo run --target "$TARGET" -- \
        --paging=never src/controller.rs README.md
fi
