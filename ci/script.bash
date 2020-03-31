#!/usr/bin/env bash

set -ex

# Incorporate TARGET env var to the build and test process
cargo build --target "$TARGET" --verbose

# We cannot run arm executables on linux
if [[ $TARGET != arm-unknown-linux-gnueabihf ]] && [[ $TARGET != aarch64-unknown-linux-gnu ]]; then
    cargo test --target "$TARGET" --verbose

    # Run 'bat' on its own source code and the README
    cargo run --target "$TARGET" -- src/bin/bat/main.rs README.md --paging=never
fi

# Check bat-as-a-library, which has a smaller set of dependencies
cargo check --target "$TARGET" --verbose --lib --no-default-features --features regex-onig
cargo check --target "$TARGET" --verbose --lib --no-default-features --features regex-onig,git
cargo check --target "$TARGET" --verbose --lib --no-default-features --features regex-onig,paging
cargo check --target "$TARGET" --verbose --lib --no-default-features --features regex-onig,git,paging
