#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")" || exit

# Check that Hyperfine is installed.
if ! command -v hyperfine > /dev/null 2>&1; then
	echo "'hyperfine' does not seem to be installed."
	echo "You can get it here: https://github.com/sharkdp/hyperfine"
	exit 1
fi

# Check that jq is installed.
if ! command -v jq > /dev/null 2>&1; then
	echo "'jq' does not seem to be installed."
	echo "You can get it here: https://stedolan.github.io/jq"
	exit 1
fi

get_cargo_target_dir() {
	cargo metadata --no-deps --format-version 1 | jq -r .target_directory
}

heading() {
    bold=$(tput bold)$(tput setaf 220)
    normal=$(tput sgr0)
    echo
    printf "\n%s%s%s\n\n" "$bold" "$1" "$normal"

    echo -e "\n### $1\n" >> "$REPORT"
}

# Clean up environment
unset BAT_CACHE_PATH
unset BAT_CONFIG_DIR
unset BAT_CONFIG_PATH
unset BAT_OPTS
unset BAT_PAGER
unset BAT_STYLE
unset BAT_TABS
unset BAT_THEME
unset COLORTERM
unset NO_COLOR
unset PAGER


RESULT_DIR="benchmark-results"
REPORT="$RESULT_DIR/report.md"

TARGET_DIR="$(get_cargo_target_dir)"
TARGET_RELEASE="${TARGET_DIR}/release/bat"

WARMUP_COUNT=3

# Determine which target to benchmark.
BAT=''
for arg in "$@"; do
	case "$arg" in
		--system)  BAT="bat" ;;
		--release) BAT="$TARGET_RELEASE" ;;
		--bat=*)   BAT="${arg:6}" ;;
	esac
done

if [[ -z "$BAT" ]]; then
	echo "A build of 'bat' must be specified for benchmarking."
	echo "You can use '--system', '--release' or '--bat=path/to/bat'."
	exit 1
fi

if ! command -v "$BAT" &>/dev/null; then
	echo "Could not find the build of bat to benchmark ($BAT)."
	case "$BAT" in
		"bat")             echo "Make you sure to symlink 'batcat' as 'bat'." ;;
		"$TARGET_RELEASE") echo "Make you sure to 'cargo build --release' first." ;;
	esac
	exit 1
fi

# Run the benchmarks
mkdir -p "$RESULT_DIR"
rm -f "$RESULT_DIR"/*.md

echo "## \`bat\` benchmark results" >> "$REPORT"


heading "Startup time"
hyperfine \
	"$(printf "%q" "$BAT") --no-config" \
	--command-name "bat" \
	--warmup "$WARMUP_COUNT" \
    --export-markdown "$RESULT_DIR/startup-time.md" \
    --export-json "$RESULT_DIR/startup-time.json"
cat "$RESULT_DIR/startup-time.md" >> "$REPORT"


heading "Startup time with syntax highlighting"
hyperfine \
	"$(printf "%q" "$BAT") --no-config --color=always startup-time-src/small-CpuInfo-file.cpuinfo" \
	--command-name "bat … small-CpuInfo-file.cpuinfo" \
	--warmup "$WARMUP_COUNT" \
    --export-markdown "$RESULT_DIR/startup-time-with-syntax-highlighting.md" \
    --export-json "$RESULT_DIR/startup-time-with-syntax-highlighting.json"
cat "$RESULT_DIR/startup-time-with-syntax-highlighting.md" >> "$REPORT"


heading "Startup time with syntax with dependencies"
hyperfine \
	"$(printf "%q" "$BAT") --no-config --color=always startup-time-src/small-Markdown-file.md" \
	--command-name "bat … small-Markdown-file.md" \
	--warmup "$WARMUP_COUNT" \
    --export-markdown "$RESULT_DIR/startup-time-with-syntax-with-dependencies.md" \
    --export-json "$RESULT_DIR/startup-time-with-syntax-with-dependencies.json"
cat "$RESULT_DIR/startup-time-with-syntax-with-dependencies.md" >> "$REPORT"


heading "Plain-text speed"
hyperfine \
	"$(printf "%q" "$BAT") --no-config --language=txt --style=plain highlighting-speed-src/numpy_test_multiarray.py" \
	--command-name 'bat … --language=txt numpy_test_multiarray.py' \
	--warmup "$WARMUP_COUNT" \
    --export-markdown "$RESULT_DIR/plain-text-speed.md" \
    --export-json "$RESULT_DIR/plain-text-speed.json"
cat "$RESULT_DIR/plain-text-speed.md" >> "$REPORT"


for wrap in character never; do
	for SRC in highlighting-speed-src/*; do
		filename="$(basename "$SRC")"

		heading "Syntax highlighting speed --wrap=${wrap}: \`$filename\`"
		hyperfine --warmup "$WARMUP_COUNT" \
			"$(printf "%q" "$BAT") --no-config --style=full --color=always --wrap=${wrap} --terminal-width=80 '$SRC'" \
			--command-name "bat … ${filename}" \
			--export-markdown "$RESULT_DIR/syntax-highlighting-speed-${filename}.md" \
			--export-json "$RESULT_DIR/syntax-highlighting-speed-${filename}.json"
		cat "$RESULT_DIR/syntax-highlighting-speed-${filename}.md" >> "$REPORT"
	done
done
