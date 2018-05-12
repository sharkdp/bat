#
# Completions for the bat command
# https://github.com/sharkdp/bat
#

# Helpers
function __bat_has_subcommand
	set -l tokens (commandline -poc)
	if [ (count $tokens) -eq 0 ]
		return 1
	end

	if [ -n "$argv[1]" ]
		 [ "$argv[1]" = "$tokens[2]" ]
		 return $status
	else
		switch "$tokens[2]"
			case "cache"
				return 0
		end
		return 1
	end
end

# bat
complete -c bat -n 'not __bat_has_subcommand' -s h -l help --description "Display help"
complete -c bat -n 'not __bat_has_subcommand' -s V -l version --description "Display version information"
complete -c bat -n 'not __bat_has_subcommand' -s l -l langauge -rf --description "Set the language for highlighting"
complete -c bat -n 'not __bat_has_subcommand' -l paging --description "Set the theme for highlighting"
complete -c bat -n 'not __bat_has_subcommand' -l list-themes --description "List the available themes"
complete -c bat -n 'not __bat_has_subcommand' -l list-languages --description "List the available languages"

# bat --style
complete -c bat -n 'not __bat_has_subcommand' -l style --description "Set additional info to display with content"
complete -c bat -n 'not __bat_has_subcommand; and __fish_contains_opt style' -l style -rf -xa auto --description "Automatically determine style"
complete -c bat -n 'not __bat_has_subcommand; and __fish_contains_opt style' -l style -rf -xa plain --description "Only display content"
complete -c bat -n 'not __bat_has_subcommand; and __fish_contains_opt style' -l style -rf -xa full --description "Display everything"
complete -c bat -n 'not __bat_has_subcommand; and __fish_contains_opt style' -l style -rf -xa header --description "Display header line"
complete -c bat -n 'not __bat_has_subcommand; and __fish_contains_opt style' -l style -rf -xa changes --description "Display git changes"
complete -c bat -n 'not __bat_has_subcommand; and __fish_contains_opt style' -l style -rf -xa numbers --description "Display line numbers"
complete -c bat -n 'not __bat_has_subcommand; and __fish_contains_opt style' -l style -rf -xa grid --description "Display grid"

# bat --color
complete -c bat -n 'not __bat_has_subcommand' -l color --description "Set color mode"
complete -c bat -n 'not __bat_has_subcommand; and __fish_contains_opt color' -l color -rf -xa auto --description "Automatically determine color mode"
complete -c bat -n 'not __bat_has_subcommand; and __fish_contains_opt color' -l color -rf -xa never --description "Plain text"
complete -c bat -n 'not __bat_has_subcommand; and __fish_contains_opt color' -l color -rf -xa always --description "Colored text"

# bat --paging
complete -c bat -n 'not __bat_has_subcommand' -l paging --description "Set paging mode"
complete -c bat -n 'not __bat_has_subcommand; and __fish_contains_opt paging' -l paging -rf -xa auto --description "Automatically determine paging mode"
complete -c bat -n 'not __bat_has_subcommand; and __fish_contains_opt paging' -l paging -rf -xa never --description "Never use the pager"
complete -c bat -n 'not __bat_has_subcommand; and __fish_contains_opt paging' -l paging -rf -xa always --description "Always use the pager"

# bat cache
complete -c bat -n '__bat_has_subcommand cache' -f -s h -l help --description "Display help for 'bat cache'"
complete -c bat -n '__bat_has_subcommand cache' -f -s i -l init --description "Initialize the cache by loading from the config dir"
complete -c bat -n '__bat_has_subcommand cache' -f -s c -l clear --description "Reset the cache"
complete -c bat -n '__bat_has_subcommand cache' -f -s d -l config-dir --description "Show the configuration directory"
