#!/bin/bash

if ! which hyperfine > /dev/null 2>&1; then
    echo "'hyperfine' does not seem to be installed."
    echo "You can get it here: https://github.com/sharkdp/hyperfine"
    exit 1
fi

echo "### Startup time"
echo

hyperfine --warmup 3 bat

echo
echo "### Plain text"
echo

hyperfine --warmup 3 "bat --language txt --paging=never 'test-src/jquery-3.3.1.js'"

echo
echo "### Time to syntax-highlight large files"
echo

for SRC in test-src/*; do
    hyperfine --warmup 3 "bat --style=full --color=always --paging=never '$SRC'"
done
