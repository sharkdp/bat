#!/usr/bin/env bash

# Spawns a new bash shell that's set up with a PATH that points
# the `bat` executable and completions from the `target/` dir.

# Requires https://github.com/scop/bash-completion

target_dir="$(dirname "$(realpath "$0")")/../target/debug"
completion_file=$target_dir/build/bat-*/out/assets/completions/bat.bash
export PATH="$target_dir:$PATH"
bash --noprofile --rcfile <(echo "source /usr/share/bash-completion/bash_completion; source $completion_file") +o history
