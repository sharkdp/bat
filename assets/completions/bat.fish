# Fish Shell Completions
# Place or symlink to $XDG_CONFIG_HOME/fish/completions/bat.fish ($XDG_CONFIG_HOME is usually set to ~/.config)

# Helper function:
function __bat_autocomplete_languages --description "A helper function used by "(status filename)
	bat --list-languages | awk -F':' '
		{
			lang=$1
			split($2, exts, ",")

			for (i in exts) {
				ext=exts[i]
				if (ext !~ /[A-Z].*/ && ext !~ /^\..*rc$/) {
					print ext"\t"lang
				}
			}
		}
	' | sort
end

# Completions:

complete -c bat -l color -xka "auto never always" -d "Specify when to use colored output (default: auto)" -n "not __fish_seen_subcommand_from cache"

complete -c bat -l config-dir -d "Display location of 'bat' configuration directory" -n "not __fish_seen_subcommand_from cache"

complete -c bat -l config-file -d "Display location of 'bat' configuration file" -n "not __fish_seen_subcommand_from cache"

complete -c bat -l decorations -xka "auto never always" -d "Specify when to use the decorations specified with '--style' (default: auto)" -n "not __fish_seen_subcommand_from cache"

complete -c bat -s h -l help -d "Print help message" -n "not __fish_seen_subcommand_from cache"

complete -c bat -s H -l highlight-line -x -d "<N> Highlight the N-th line with a different background color" -n "not __fish_seen_subcommand_from cache"

complete -c bat -l italic-text -xka "always never" -d "Specify when to use ANSI sequences for italic text (default: never)" -n "not __fish_seen_subcommand_from cache"

complete -c bat -s l -l language -d "Set the language for syntax highlighting" -n "not __fish_seen_subcommand_from cache" -xa "(__bat_autocomplete_languages)" 

complete -c bat -s r -l line-range -x -d "<N:M> Only print the specified range of lines for each file" -n "not __fish_seen_subcommand_from cache"

complete -c bat -l list-languages -d "Display list of supported languages for syntax highlighting" -n "not __fish_seen_subcommand_from cache"

complete -c bat -l list-themes -d "Display a list of supported themes for syntax highlighting" -n "not __fish_seen_subcommand_from cache"

complete -c bat -s m -l map-syntax -x -d "<from:to> Map a file extension or file name to an existing syntax" -n "not __fish_seen_subcommand_from cache"

complete -c bat -s n -l number -d "Only show line numbers, no other decorations. Alias for '--style=numbers'" -n "not __fish_seen_subcommand_from cache"

complete -c bat -l pager -x -d "<command> Specify which pager program to use (default: less)" -n "not __fish_seen_subcommand_from cache"

complete -c bat -l paging -xka "auto never always" -d "Specify when to use the pager (default: auto)" -n "not __fish_seen_subcommand_from cache"

complete -c bat -s p -l plain -d "Only show plain style, no decorations. Alias for '--style=plain'" -n "not __fish_seen_subcommand_from cache"

complete -c bat -s A -l show-all -d "Show non-printable characters like space/tab/newline" -n "not __fish_seen_subcommand_from cache"

complete -c bat -l style -xka "auto full plain changes header grid numbers" -d "Comma-separated list of style elements or presets to display with file contents" -n "not __fish_seen_subcommand_from cache"

complete -c bat -l tabs -x -d "<T> Set the tab width to T spaces (width of 0 passes tabs through directly)" -n "not __fish_seen_subcommand_from cache"

complete -c bat -l terminal-width -x -d "<width> Explicitly set terminal width; Prefix with '+' or '-' to offset (default width is auto determined)" -n "not __fish_seen_subcommand_from cache"

complete -c bat -l theme -xka "(bat --list-themes | cat)" -d "Set the theme for syntax highlighting" -n "not __fish_seen_subcommand_from cache"

complete -c bat -s u -l unbuffered -d "POSIX-compliant unbuffered output. Option is ignored" -n "not __fish_seen_subcommand_from cache"

complete -c bat -s V -l version -d "Show version information" -n "not __fish_seen_subcommand_from cache"

complete -c bat -l wrap -xka "auto never character" -d "<mode> Specify the text-wrapping mode (default: auto)" -n "not __fish_seen_subcommand_from cache"

# Sub-command 'cache' completions
complete -c bat -a "cache" -d "Modify the syntax/language definition cache" -n "not __fish_seen_subcommand_from cache"

complete -c bat -l build -f -d "Parse syntaxes/language definitions into cache" -n "__fish_seen_subcommand_from cache"

complete -c bat -l clear -f -d "Reset syntaxes/language definitions to default settings" -n "__fish_seen_subcommand_from cache"
