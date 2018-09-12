#!/usr/bin/env bash

set -ex

if [ "$TRAVIS_OS_NAME" != linux ]; then
    exit 0
fi

sudo apt-get update

# needed for musl targets
sudo apt-get install -y musl-tools

# needed to build deb packages
sudo apt-get install -y fakeroot

# needed for i686 linux gnu target
if [[ $TARGET == i686-unknown-linux-gnu ]]; then
    sudo apt-get install -y gcc-multilib
fi

# needed for cross-compiling for arm
if [[ $TARGET == arm-unknown-linux-gnueabihf ]]; then
    sudo apt-get install -y \
        gcc-4.8-arm-linux-gnueabihf \
        binutils-arm-linux-gnueabihf \
        libc6-armhf-cross \
        libc6-dev-armhf-cross
fi

# needed for cross-compiling for arm64
if [[ $TARGET == aarch64-unknown-linux-gnu ]]; then
    sudo apt-get install -y \
        gcc-4.8-aarch64-linux-gnu \
        binutils-aarch64-linux-gnu \
        gcc-aarch64-linux-gnu
fi
