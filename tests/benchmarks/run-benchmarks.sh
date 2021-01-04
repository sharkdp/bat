#!/usr/bin/env bash
cd "$(dirname "${BASH_SOURCE[0]}")" || exit

# Check that Hyperfine is installed.
if ! command -v hyperfine > /dev/null 2>&1; then
    echo "'hyperfine' does not seem to be installed."
    echo "You can get it here: https://github.com/sharkdp/hyperfine"
    exit 1
fi

# Determine the target directories.
get_target_dir() {
	if [[ -f "$HOME/.cargo/config" ]]; then
		grep 'target-dir[[:space:]]*=' "$HOME/.cargo/config" \
			| sed 's/^[[:space:]]*target-dir[[:space:]]*=//; s/^[[:space:]]*"//; s/"[[:space:]]*$//' \
			&& return 0
	fi
	
	echo "../../target"
}

TARGET_DIR="$(get_target_dir)"
TARGET_DEBUG="${TARGET_DIR}/debug/bat"
TARGET_RELEASE="${TARGET_DIR}/release/bat"

# Determine which target to benchmark.
BAT=''
for arg in "$@"; do
	case "$arg" in
		--system)  BAT="bat" ;;
		--debug)   BAT="$TARGET_DEBUG" ;;
		--release) BAT="$TARGET_RELEASE" ;;
		--bat=*)   BAT="${arg:6}" ;;
	esac
done

if [[ -z "$BAT" ]]; then
	echo "A build of 'bat' must be specified for benchmarking."
	echo "You can use '--system', '--debug', or '--release'."
	exit 1
fi

# Ensure that the target is built.
if ! command -v "$BAT" &>/dev/null; then
	echo "Could not find the build of bat to benchmark."
	case "$BAT" in
		"bat")             echo "Make you sure to symlink 'batcat' as 'bat'." ;;
		"$TARGET_DEBUG")   echo "Make you sure to 'cargo build' first." ;;
		"$TARGET_RELEASE") echo "Make you sure to 'cargo build --release' first." ;;
	esac
	exit 1
fi

# Run the benchmark.
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
