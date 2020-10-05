#!/usr/bin/env bash
cd "$(dirname "${BASH_SOURCE[0]}")"

if ! which hyperfine > /dev/null 2>&1; then
    echo "'hyperfine' does not seem to be installed."
    echo "You can get it here: https://github.com/sharkdp/hyperfine"
    exit 1
fi

BAT=''
for arg in "$@"; do
	case "$arg" in
		--system)  BAT="bat" ;;
		--debug)   BAT="../../target/debug/bat" ;;
		--release) BAT="../../target/release/bat" ;;
	esac
done

if [[ -z "$BAT" ]]; then
	echo "A build of 'bat' must be specified for benchmarking."
	echo "You can use '--system', '--debug', or '--release'."
	exit 1
fi

if ! command -v "$BAT" &>/dev/null; then
	echo "Could not find the build of bat to benchmark."
	case "$BAT" in
		"bat")                        echo "Make you sure to symlink 'batcat' as 'bat'." ;;
		"../../target/debug/debug")   echo "Make you sure to 'cargo build' first." ;;
		"../../target/debug/release") echo "Make you sure to 'cargo build --release' first." ;;
	esac
	exit 1
fi

echo "### Startup time"
echo

hyperfine --warmup 3 "$BAT"

echo
echo "### Plain text"
echo

hyperfine --warmup 3 "$(printf "%q" "$BAT") --language txt --paging=never 'test-src/jquery-3.3.1.js'"

echo
echo "### Time to syntax-highlight large files"
echo

for SRC in test-src/*; do
    hyperfine --warmup 3 "$(printf "%q" "$BAT") --style=full --color=always --paging=never $(printf "%q" "$SRC")"
done
