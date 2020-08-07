#compdef bat
# FIXME: help me with the subcommand `cache`, zsh completion is hard as hell

local -a args
local state

args=(
    '(-A --show-all)'{-A,--show-all}'[Show non-printable characters (space, tab, newline, ..)]'
    '(-p --plain)'{-p,--plain}'[Show plain style (alias for `--style=plain`)]'
    '(-l --language)'{-l+,--language=}'[Set the language for syntax highlighting]: :->language'
    '(-H --highlight-line)'{-H,--highlight-line}'[Highlight lines N through M]'
    '(--file-name)'--file-name'[Specify the name to display for a file]: :_files'
    '(-d --diff)'--diff'[Only show lines that have been added/removed/modified]'
    '(--tabs)'--tabs'[Set the tab width to T spaces]'
    '(--wrap)'--wrap'[Specify the text-wrapping mode]'
    '(--number)'--number'[Show line numbers]'
    '(--color)'--color='[When to use colors]: :(auto never always)'
    '(--italic-text)'--italic-text='[Use italics in output]: :(always never)'
    '(--decorations)'--decorations='[When to show the decorations]: :(auto never always)'
    '(--paging)'--paging='[Specify when to use the pager]: :(auto never always)'
    '(-m --map-syntax)'{-m+,--map-syntax=}'[Use the specified syntax for files matching the glob pattern]'
    '(--theme)'--theme='[Set the color theme for syntax highlighting]: :->theme'
    '(: --list-themes --list-languages -L)'--list-themes'[Display all supported highlighting themes]'
    '(--style)'--style='[Comma-separated list of style elements to display]: :->style'
    '(-r --line-range)'{-r+,--line-range=}'[Only print the lines from N to M]'
    '(: --list-themes --list-languages -L)'{-L,--list-languages}'[Display all supported languages]'
    '(: -)'{-h,--help}'[Print this help message]'
    '(: -)'{-V,--version}'[Show version information]'
    '*: :_files'
)

_arguments -S -s $args

case "$state" in 
    language)
        local IFS=$'\n'
        local -a languages
        languages=( $(bat --list-languages | awk -F':|,' '{ for (i = 1; i <= NF; ++i) printf("%s:%s\n", $i, $1) }') )

        _describe 'language' languages 
    ;;

    theme)
        local IFS=$'\n'
        local -a themes
        themes=( $(bat --list-themes | sort) )

        _values $themes
    ;;

    style)
        _values -s , 'style' auto full plain changes header grid numbers snip
    ;;
esac
