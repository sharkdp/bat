#!/bin/bash

if ! which hyperfine > /dev/null 2>&1; then
    echo "'hyperfine' does not seem to be installed."
    echo "You can get it here: https://github.com/sharkdp/hyperfine"
    exit 1
fi

SRC="test-src/jquery-3.3.1.js"


cmd_bat="bat --style=full --color=always --paging=never '$SRC'"
cmd_bat_simple="bat --plain --wrap=never --tabs=0 --color=always --paging=never '$SRC'"
cmd_pygmentize="pygmentize -g '$SRC'"
cmd_highlight="highlight -O truecolor '$SRC'"
cmd_ccat="ccat --color=always '$SRC'"
cmd_source_highlight="source-highlight --failsafe --infer-lang -f esc -i '$SRC'"
cmd_hicat="hicat '$SRC'"
cmd_coderay="coderay '$SRC'"
cmd_rouge="rougify '$SRC'"

hyperfine --warmup 3 \
    "$cmd_bat" \
    "$cmd_bat_simple" \
    "$cmd_pygmentize" \
    "$cmd_highlight" \
    "$cmd_ccat" \
    "$cmd_source_highlight" \
    "$cmd_hicat" \
    "$cmd_coderay" \
    "$cmd_rouge" \
