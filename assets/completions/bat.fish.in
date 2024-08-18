# Fish Shell Completions
# Copy or symlink to $XDG_CONFIG_HOME/fish/completions/{{PROJECT_EXECUTABLE}}.fish
# ($XDG_CONFIG_HOME is usually set to ~/.config)

# `bat` is `batcat` on Debian and Ubuntu
set bat {{PROJECT_EXECUTABLE}}

# Helper functions:

function __bat_complete_files -a token
    # Cheat to complete files by calling `complete -C` on a fake command name,
    # like `__fish_complete_directories` does.
    set -l fake_command aaabccccdeeeeefffffffffgghhhhhhiiiii
    complete -C"$fake_command $token"
end

function __bat_complete_one_language -a comp
    command $bat --list-languages | string split -f1 : | string match -e "$comp"
end

function __bat_complete_list_languages
    for spec in (command $bat --list-languages)
        set -l name (string split -f1 : $spec)
        for ext in (string split -f2 : $spec | string split ,)
            test -n "$ext"; or continue
            string match -rq '[/*]' $ext; and continue
            printf "%s\t%s\n" $ext $name
        end
        printf "%s\t\n" $name
    end
end

function __bat_complete_map_syntax
    set -l token (commandline -ct)

    if string match -qr '(?<glob>.+):(?<syntax>.*)' -- $token
        # If token ends with a colon, complete with the list of language names.
        set -f comps $glob:(__bat_complete_one_language $syntax)
    else if string match -qr '\*' -- $token
        # If token contains a globbing character (`*`), complete only possible
        # globs in the current directory
        set -f comps (__bat_complete_files $token | string match -er '[*]'):
    else
        # Complete files (and globs).
        set -f comps (__bat_complete_files $token | string match -erv '/$'):
    end

    if set -q comps[1]
        printf "%s\t\n" $comps
    end
end

function __bat_cache_subcommand
    __fish_seen_subcommand_from cache
end

# Returns true if no exclusive arguments seen.
function __bat_no_excl_args
    not __bat_cache_subcommand; and not __fish_seen_argument \
        -s h -l help \
        -s V -l version \
        -l acknowledgements \
        -l config-dir -l config-file \
        -l diagnostic \
        -l list-languages -l list-themes
end

# Returns true if the 'cache' subcommand is seen without any exclusive arguments.
function __bat_cache_no_excl
    __bat_cache_subcommand; and not __fish_seen_argument \
        -s h -l help \
        -l acknowledgements -l build -l clear
end

function __bat_style_opts
    set -l style_opts \
        "default,recommended components" \
        "auto,same as 'default' unless piped" \
        "full,all components" \
        "plain,no components" \
        "changes,Git change markers" \
        "header,alias for header-filename" \
        "header-filename,filename above content" \
        "header-filesize,filesize above content" \
        "grid,lines b/w sidebar/header/content" \
        "numbers,line numbers in sidebar" \
        "rule,separate files" \
        "snip,separate ranges"

    string replace , \t $style_opts
end

# Use option argument descriptions to indicate which is the default, saving
# horizontal space and making sure the option description isn't truncated.
set -l color_opts '
    auto\tdefault
    never\t
    always\t
'
set -l decorations_opts $color_opts
set -l paging_opts $color_opts

# Include some examples so we can indicate the default.
set -l pager_opts '
    less\tdefault
    less\ -FR\t
    more\t
    vimpager\t
'

set -l italic_text_opts '
    always\t
    never\tdefault
'

set -l wrap_opts '
    auto\tdefault
    never\t
    character\t
'

# While --tabs theoretically takes any number, most people should be OK with these.
# Specifying a list lets us explain what 0 does.
set -l tabs_opts '
    0\tpass\ tabs\ through\ directly
    1\t
    2\t
    4\t
    8\t
'

set -l special_themes '
    auto\tdefault,\ Choose\ a\ theme\ based\ on\ dark\ or\ light\ mode
    auto:always\tChoose\ a\ theme\ based\ on\ dark\ or\ light\ mode
    auto:system\tChoose\ a\ theme\ based\ on\ dark\ or\ light\ mode
    dark\tUse\ the\ theme\ specified\ by\ --theme-dark
    light\tUse\ the\ theme\ specified\ by\ --theme-light
'

# Completions:

complete -c $bat -l acknowledgements -d "Print acknowledgements" -n __fish_is_first_arg

complete -c $bat -l cache-dir -f -d "Show bat's cache directory" -n __fish_is_first_arg

complete -c $bat -l color -x -a "$color_opts" -d "When to use colored output" -n __bat_no_excl_args

complete -c $bat -l config-dir -f -d "Display location of configuration directory" -n __fish_is_first_arg

complete -c $bat -l config-file -f -d "Display location of configuration file" -n __fish_is_first_arg

complete -c $bat -l decorations -x -a "$decorations_opts" -d "When to use --style decorations" -n __bat_no_excl_args

complete -c $bat -l diagnostic -d "Print diagnostic info for bug reports" -n __fish_is_first_arg

complete -c $bat -s d -l diff -d "Only show lines with Git changes" -n __bat_no_excl_args

complete -c $bat -l diff-context -x -d "Show N context lines around Git changes" -n "__fish_seen_argument -s d -l diff"

complete -c $bat -l generate-config-file -f -d "Generates a default configuration file" -n __fish_is_first_arg

complete -c $bat -l file-name -x -d "Specify the display name" -n __bat_no_excl_args

complete -c $bat -s f -l force-colorization -d "Force color and decorations" -n __bat_no_excl_args

complete -c $bat -s h -d "Print a concise overview" -n __fish_is_first_arg

complete -c $bat -l help -f -d "Print all help information" -n __fish_is_first_arg

complete -c $bat -s H -l highlight-line -x -d "Highlight line(s) N[:M]" -n __bat_no_excl_args

complete -c $bat -l ignored-suffix -x -d "Ignore extension" -n __bat_no_excl_args

complete -c $bat -l italic-text -x -a "$italic_text_opts" -d "When to use italic text in the output" -n __bat_no_excl_args

complete -c $bat -s l -l language -x -k -a "(__bat_complete_list_languages)" -d "Set the syntax highlighting language" -n __bat_no_excl_args

complete -c $bat -l lessopen -d "Enable the $LESSOPEN preprocessor" -n __fish_is_first_arg

complete -c $bat -s r -l line-range -x -d "Only print lines [M]:[N] (either optional)" -n __bat_no_excl_args

complete -c $bat -l list-languages -f -d "List syntax highlighting languages" -n __fish_is_first_arg

complete -c $bat -l list-themes -f -d "List syntax highlighting themes" -n __fish_is_first_arg

complete -c $bat -s m -l map-syntax -x -a "(__bat_complete_map_syntax)" -d "Map <glob pattern>:<language syntax>" -n __bat_no_excl_args

complete -c $bat -l no-config -d "Do not use the configuration file"

complete -c $bat -l no-custom-assets -d "Do not load custom assets"

complete -c $bat -l no-lessopen -d "Disable the $LESSOPEN preprocessor if enabled (overrides --lessopen)"

complete -c $bat -s n -l number -d "Only show line numbers, no other decorations" -n __bat_no_excl_args

complete -c $bat -l pager -x -a "$pager_opts" -d "Which pager to use" -n __bat_no_excl_args

complete -c $bat -l paging -x -a "$paging_opts" -d "When to use the pager" -n __bat_no_excl_args

complete -c $bat -s p -l plain -d "Disable decorations" -n __bat_no_excl_args

complete -c $bat -o pp -d "Disable decorations and paging" -n __bat_no_excl_args

complete -c $bat -s P -d "Disable paging" -n __bat_no_excl_args

complete -c $bat -s A -l show-all -d "Show non-printable characters" -n __bat_no_excl_args

complete -c $bat -l style -x -k -a "(__fish_complete_list , __bat_style_opts)" -d "Specify which non-content elements to display" -n __bat_no_excl_args

complete -c $bat -l tabs -x -a "$tabs_opts" -d "Set tab width" -n __bat_no_excl_args

complete -c $bat -l terminal-width -x -d "Set terminal <width>, +<offset>, or -<offset>" -n __bat_no_excl_args

complete -c $bat -l theme -x -a "$special_themes(command $bat --list-themes | command cat)" -d "Set the syntax highlighting theme" -n __bat_no_excl_args

complete -c $bat -l theme-dark -x -a "(command $bat --list-themes | command cat)" -d "Set the syntax highlighting theme for dark backgrounds" -n __bat_no_excl_args

complete -c $bat -l theme-light -x -a "(command $bat --list-themes | command cat)" -d "Set the syntax highlighting theme for light backgrounds" -n __bat_no_excl_args

complete -c $bat -s V -l version -f -d "Show version information" -n __fish_is_first_arg

complete -c $bat -l wrap -x -a "$wrap_opts" -d "Text-wrapping mode" -n __bat_no_excl_args

# Sub-command 'cache' completions
## Completion of the 'cache' command itself is removed for better UX
## See https://github.com/sharkdp/bat/issues/2085#issuecomment-1271646802

complete -c $bat -l build -f -d "Parse new definitions into cache" -n __bat_cache_no_excl

complete -c $bat -l clear -f -d "Reset definitions to defaults" -n __bat_cache_no_excl

complete -c $bat -l blank -f -d "Create new data instead of appending" -n "__bat_cache_subcommand; and not __fish_seen_argument -l clear"

complete -c $bat -l source -x -a "(__fish_complete_directories)" -d "Load syntaxes and themes from DIR" -n "__bat_cache_subcommand; and not __fish_seen_argument -l clear"

complete -c $bat -l target -x -a "(__fish_complete_directories)" -d "Store cache in DIR" -n __bat_cache_subcommand

complete -c $bat -l acknowledgements -d "Build acknowledgements.bin" -n __bat_cache_no_excl

complete -c $bat -s h -d "Print a concise overview of $bat-cache help" -n __bat_cache_no_excl

complete -c $bat -l help -f -d "Print all $bat-cache help" -n __bat_cache_no_excl

# vim:ft=fish
