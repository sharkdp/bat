#!/usr/bin/env bash

# Spawns a new zsh shell that's set up with a PATH that points
# the `bat` executable and completions from the `target/` dir.

# Yes, this is a .zsh file that's executed with bash instead :)

target_dir="$(dirname "$(realpath "$0")")/../target/debug"
completion_file=$target_dir/build/bat-*/out/assets/completions/bat.zsh

# Setup a temporary ZDOTDIR-
# that's the place where ZSH looks up config files (defaults to $HOME)
ZDOTDIR=$(mktemp -d); export ZDOTDIR
mkdir "$ZDOTDIR/completions"
cp $completion_file "$ZDOTDIR/completions/_bat"

# Setup an RC file that adds our temporary completions dir to the autoload path
# and initializes completions.
echo 'fpath+=("$ZDOTDIR/completions"); autoload -U compinit; compinit' > "$ZDOTDIR/.zshrc"

export PATH="$target_dir:$PATH"
zsh --no-globalrcs

rm -rf "$ZDOTDIR"
