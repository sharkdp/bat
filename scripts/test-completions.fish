#!/usr/bin/env fish

# Spawns a new fish shell that's set up with a PATH that points
# the `bat` executable and completions from the `target/` dir.

set target_dir "$(dirname (realpath (status -f)))/../target/debug"
set completion_file $target_dir/build/bat-*/out/assets/completions/bat.fish
set --export PATH "$target_dir:$PATH"
fish \
    --no-config --private \
    --init-command="set -U fish_color_command brcyan; set -U fish_pager_color_description yellow; source $completion_file"
