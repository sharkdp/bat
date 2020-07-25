#!/usr/bin/env bash
_modules=('system' 'bat' 'bat_config' 'bat_wrapper' 'bat_wrapper_function' 'tool')
_modules_consented=()

set -o pipefail

export LC_ALL=C
export LANG=C

BAT="bat"
if ! command -v bat &>/dev/null; then
	if command -v batcat &> /dev/null; then
		BAT="batcat"
	else
		tput setaf 1
		printf "%s\n%s\n" \
			"Unable to find a bat executable on your PATH." \
			"Please ensure that 'bat' exists and is not named something else."
		tput sgr0
		exit 1
	fi
fi

# -----------------------------------------------------------------------------
# Modules:
# -----------------------------------------------------------------------------

_bat_:description() {
	_collects "Version information for 'bat'."
	_collects "Custom syntaxes and themes for 'bat'."
}

_bat_config_:description() {
	_collects "The environment variables used by 'bat'."
	_collects "The 'bat' configuration file."
}

_bat_wrapper_:description() {
	_collects "Any wrapper script used by 'bat'."
}

_bat_wrapper_function_:description() {
	_collects "The wrapper function surrounding 'bat' (if applicable)."
}

_system_:description() {
	_collects "Operating system name."
	_collects "Operating system version."
}

_tool_:description() {
	_collects "Version information for 'less'."
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

_bat_:run() {
	_out "$BAT" --version
	_out env | grep '^BAT_\|^PAGER='

	local cache_dir="$($BAT --cache-dir)"
	if [[ -f "${cache_dir}/syntaxes.bin" ]]; then
		_print_command "$BAT" "--list-languages"
		echo "Found custom syntax set."
	fi

	if [[ -f "${cache_dir}/themes.bin" ]]; then
		_print_command "$BAT" "--list-themes"
		echo "Found custom theme set."
	fi
}

_bat_config_:run() {
	if [[ -f "$("$BAT" --config-file)" ]]; then
		_out_fence cat "$("$BAT" --config-file)"
	fi
}

_bat_wrapper_:run() {
	_bat_wrapper_:detect_wrapper() {
		local bat="$1"
		if file "$(which "${bat}")" | grep "text executable" &> /dev/null; then
			_out_fence cat "$(which "${bat}")"
			return
		fi

		printf "\nNo wrapper script for '%s'.\n" "${bat}"
	}

	_bat_wrapper_:detect_wrapper bat
	if [[ "$BAT" != "bat" ]]; then
		_bat_wrapper_:detect_wrapper "$BAT"
	fi
}

_bat_wrapper_function_:run() {
	_bat_wrapper_function_:detect_wrapper() {
		local command="$1"
		case "$("$SHELL" --version | head -n 1)" in
			*fish*)
				if "$SHELL" --login -i -c "type ${command}" 2>&1 | grep 'function' &> /dev/null; then
					_out_fence "$SHELL" --login -i -c "functions ${command}"
					return
				fi ;;

			*bash* | *zsh*)
				local type="$("$SHELL" --login -i -c "type ${command}" 2>&1)"
				if grep 'function' <<< "$type" &> /dev/null; then
					_out_fence "$SHELL" --login -i -c "declare -f ${command}"
					return
				elif grep 'alias' <<< "$type" &> /dev/null; then
					_out_fence "$SHELL" --login -i -c "type ${command}"
					return
				fi ;;

			*)
				echo "Unable to determine if a wrapper function for '${command}' is set."
				return ;;
		esac
		printf "\nNo wrapper function for '%s'.\n" "${command}"
	}

	_bat_wrapper_function_:detect_wrapper bat
	_bat_wrapper_function_:detect_wrapper cat
	if [[ "$BAT" != "bat" ]]; then
		_bat_wrapper_function_:detect_wrapper "$BAT"
	fi
}

_system_:run() {
	_out uname -srm

	if command -v "sw_vers" &> /dev/null; then _out sw_vers; fi
	if command -v "lsb_release" &> /dev/null; then _out lsb_release -a; fi
}

_tool_:run() {
	_out less --version | head -n1
}

# -----------------------------------------------------------------------------
# Functions:
# -----------------------------------------------------------------------------

_print_command() {
	printf '\n**$' 1>&2
	printf ' %s' "$@" 1>&2
	printf '**\n' 1>&2
}

_out() {
	_print_command "$@"
	"$@" 2>&1 | sed 's/$/  /'
}

_out_fence() {
	_print_command "$@"
	printf '```\n' 1>&2
	"$@" 2>&1
	printf '```\n' 1>&2
}

_tput() {
	tput "$@" 1>&2 2> /dev/null
}

_collects() {
	printf " - %s\n" "$1" 1>&2
}

_ask_module() {
	_tput clear
	_tput cup 0 0

	cat 1>&2 << EOF
--------------------------------------------------------------------------------
This script runs some harmless commands to collect information about your
system and bat configuration. It will give you a small preview of the commands
that will be run, and ask consent before running them. Once completed, it will
output a small report that you can review and copy into the issue description.
--------------------------------------------------------------------------------
EOF

	# Print description.
	_tput setaf 3
	printf "The following data will be collected:\n" 1>&2
	_tput sgr0
	"_$1_:description"
	_tput sgr0

	# Print preview.
	_tput setaf 3
	printf "\nThe following commands will be run:\n" 1>&2
	_tput sgr0
	declare -f "_$1_:run" \
		| sed 's/^ *//; s/;$//' \
		| grep '^_out[^ ]* ' \
		| sed 's/^_out[^ ]* //' \
		| sed "s/\"\$BAT\"/$BAT/" 1>&2

	# Prompt
	printf "\n" 1>&2
	local response
	while true; do
		_tput cup "$(($( tput lines || echo 22) - 2))"
		_tput el
		read -er -p "Collect $(sed 's/_/ /' <<< "$1") data? [Y/n] " response
		case "$response" in
			Y | y | yes | '') return 0 ;;
			N | n | no) return 1 ;;
			*) continue ;;
		esac
	done
}

_run_module() {
	local module="$1"
	printf "%s\n%s\n" "$module" "$(printf "%${#module}s" | tr ' ' '-')"
	"_$1_:run"
}

# -----------------------------------------------------------------------------
# Functions:
# -----------------------------------------------------------------------------

# Tell the user if their executable isn't named "bat".
if [[ "$BAT" != "bat" ]] && [[ "$1" != '-y' ]]; then
	trap '_tput rmcup; exit 1' INT
	_tput smcup
	_tput clear
	_tput cup 0 0
	_tput setaf 1
	printf "The %s executable on your system is named '%s'.\n%s\n" "bat" "$BAT" \
		"If your issue is related to installation, please check that this isn't the issue."
	_tput sgr0
	printf "Press any key to continue...\n"
	read -rsn1
	_tput rmcup
fi

# Ask for consent.
if [[ "$1" == '-y' ]]; then
	_modules_consented=("${_modules[@]}")
else
	trap '_tput rmcup; exit 1' INT
	_tput smcup
	for _module in "${_modules[@]}"; do
		if _ask_module "$_module"; then
			_modules_consented+=("$_module")
		fi
	done
	_tput rmcup
fi

# Collect information.
for _module in "${_modules_consented[@]}"; do
	_run_module "$_module" 2>&1
	printf "\n"
done
