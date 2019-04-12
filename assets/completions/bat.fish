# Fish Shell Completions (WIP)

# Place or symlink to $XDG_CONFIG_HOME/fish/completions/bat.fish ($XDG_CONFIG_HOME is usually set to ~/.config)
complete -c bat -s l -l language -d "Set the language for syntax highlighting"

complete -c bat -l list-languages -d "Display list of supported languages for syntax highlighting"

complete -c bat -s m -l map-syntax -d "<from:to> Map a file extension or file name to an existing syntax"

# TODO: add completion for theme list
complete -c bat -l theme -d "<theme> Set the theme for syntax highlighting"

complete -c bat -l list-themes -d "Display a list of supported themes for syntax highlighting"

# TODO: add completion for style list
complete -c bat -l style -d "<styles> Comma-separated list of style elements or presets to display with file contents" -a 'auto full plain changes header grid numbers'

complete -c bat -s p -l plain -d "Only show plain style, no decorations. Alias for '--style=plain'"

complete -c bat -s n -l number -d "Only show line numbers, no other decorations. Alias for '--style=numbers'"

complete -c bat -s A -l show-all -d "Show non-printable characters like space/tab/newline"

# TODO: possibly add completion showing max line (using 'wc -l') on file in current arguments (using commandline -opc)
complete -c bat -s r -l line-range -d "<N:M> Only print the specified range of lines for each file"

# TODO: possibly add completion showing max line (using 'wc -l') on file in current arguments (using commandline -opc)
complete -c bat -s H -l highlight-line -d "<N> Highlight the N-th line with a different background color"

complete -c bat -l color -xa 'auto never always' -d "<when> Specify when to use colored output (*auto*, never, always)"

complete -c bat -l italic-text -xa 'always never' -d "<when> Specify when to use ANSI sequences for italic text (always, *never*)"

complete -c bat -l decorations -xa 'auto never always' -d "<when> Specify when to use the decorations specified with '--style' (*auto*, never, always)"

complete -c bat -l paging -xa 'auto never always' -d "<when> Specify when to use the pager (*auto*, never, always)"

# TODO: add completion for sub-commands like less
complete -c bat -l pager -d "<command> Specify which pager to use (default is 'less') eg. 'less -RF'"

complete -c bat -l wrap -xa 'auto never character' -d "<mode> Specify the text-wrapping mode (*auto*, never, character)"

complete -c bat -l tabs -d "<T> Set the tab width to T spaces (width of 0 passes tabs through directly)"

complete -c bat -s u -l unbuffered -d "POSIX-compliant unbuffered output. Option is ignored"

complete -c bat -l terminal-width -d "<width> Explicitly set terminal width; Prefix with '+' or '-' to offset (default width is auto determined)"

complete -c bat -s h -l help -d "Print help message"

complete -c bat -s V -l version -d "Show version information"

# TODO: add completion for sub-command 'cache'
# # cache    Modify the syntax-definition and theme cache