# shellcheck disable=SC2207

# Requires https://github.com/scop/bash-completion

# Macs have bash3 for which the bash-completion package doesn't include
# _init_completion. This is a minimal version of that function.
__bat_init_completion()
{
	COMPREPLY=()
	_get_comp_words_by_ref "$@" cur prev words cword
}

__bat_escape_completions()
{
	# Do not escape if completing a quoted value.
	[[ $cur == [\"\']* ]] && return 0
	# printf -v to an array index is available in bash >= 4.1.
	# Use it if available, as -o filenames is semantically incorrect if
	# we are not actually completing filenames, and it has side effects
	# (e.g. adds trailing slash to candidates matching present dirs).
	if ((
		BASH_VERSINFO[0] > 4 || \
		BASH_VERSINFO[0] == 4 && BASH_VERSINFO[1] > 0
	)); then
		local i
		for i in ${!COMPREPLY[*]}; do
			printf -v "COMPREPLY[i]" %q "${COMPREPLY[i]}"
		done
	else
		compopt -o filenames
	fi
}

_bat() {
	local cur prev words split=false
	if declare -F _init_completion >/dev/null 2>&1; then
		_init_completion -s || return 0
	else
		__bat_init_completion -n "=" || return 0
		_split_longopt && split=true
	fi

	if [[ ${words[1]-} == cache ]]; then
		case $prev in
		--source | --target)
			_filedir -d
			return 0
			;;
		esac
		COMPREPLY=($(compgen -W "
			--build
			--clear
			--source
			--target
			--blank
			--help
		" -- "$cur"))
		return 0
	fi

	case $prev in
	-l | --language)
		local IFS=$'\n'
		COMPREPLY=($(compgen -W "$(
			"$1" --list-languages | while IFS=: read -r lang _; do
				printf "%s\n" "$lang"
			done
		)" -- "$cur"))
                __bat_escape_completions
		return 0
		;;
	-H | --highlight-line | \
	--diff-context | \
	--tabs | \
	--terminal-width | \
	-m | --map-syntax | \
	--ignored-suffix | \
	--list-themes | \
	--squeeze-limit | \
	--line-range | \
	-L | --list-languages | \
	--lessopen | \
	--diagnostic | \
	--acknowledgements | \
	-h | --help | \
	-V | --version | \
	--cache-dir | \
	--config-dir | \
	--config-file | \
	--generate-config-file)
		# argument required but no completion available, or option
		# causes an exit
		return 0
		;;
	--file-name)
		_filedir
		return 0
		;;
	--wrap)
		COMPREPLY=($(compgen -W "auto never character" -- "$cur"))
		return 0
		;;
	--color | --decorations | --paging)
		COMPREPLY=($(compgen -W "auto never always" -- "$cur"))
		return 0
		;;
	--italic-text)
		COMPREPLY=($(compgen -W "always never" -- "$cur"))
		return 0
		;;
	--pager)
		COMPREPLY=($(compgen -c -- "$cur"))
		return 0
		;;
	--theme)
    	local IFS=$'\n'
    	COMPREPLY=($(compgen -W "auto${IFS}auto:always${IFS}auto:system${IFS}dark${IFS}light${IFS}$("$1" --list-themes)" -- "$cur"))
                    __bat_escape_completions
    	return 0
    	;;
	--theme-dark | \
	--theme-light)
		local IFS=$'\n'
		COMPREPLY=($(compgen -W "$("$1" --list-themes)" -- "$cur"))
                __bat_escape_completions
		return 0
		;;
	--style)
		# shellcheck disable=SC2034
		local -a styles=(
			default
			full
			auto
			plain
			changes
			header
			header-filename
			header-filesize
			grid
			rule
			numbers
			snip
		)
                # shellcheck disable=SC2016
		if declare -F _comp_delimited >/dev/null 2>&1; then
			# bash-completion > 2.11
			_comp_delimited , -W '"${styles[@]}"'
		else
			COMPREPLY=($(compgen -W '${styles[@]}' -- "$cur"))
		fi
		return 0
	esac

	$split && return 0

	if [[ $cur == -* ]]; then
		# --unbuffered excluded intentionally (no-op)
		COMPREPLY=($(compgen -W "
			--show-all
			--plain
			--language
			--highlight-line
			--file-name
			--diff
			--diff-context
			--tabs
			--wrap
			--chop-long-lines
			--terminal-width
			--number
			--color
			--italic-text
			--decorations
			--force-colorization
			--paging
			--pager
			--map-syntax
			--ignored-suffix
			--theme
			--theme-dark
			--theme-light
			--list-themes
			--squeeze-blank
			--squeeze-limit
			--style
			--line-range
			--list-languages
			--lessopen
			--diagnostic
			--acknowledgements
			--set-terminal-title
			--help
			--version
			--cache-dir
			--config-dir
			--config-file
			--generate-config-file
			--no-config
			--no-custom-assets
			--no-lessopen
		" -- "$cur"))
		return 0
	fi

	_filedir
	
	## Completion of the 'cache' command itself is removed for better UX
	## See https://github.com/sharkdp/bat/issues/2085#issuecomment-1271646802
} && complete -F _bat {{PROJECT_EXECUTABLE}}
