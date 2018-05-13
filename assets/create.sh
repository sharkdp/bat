set -e

THEME_FOLDER="$HOME/.config/bat/themes"
SYNTAX_FOLDER="$HOME/.config/bat/syntax"

if [ ! -e "$THEME_FOLDER" ]; then
    mkdir -p "$THEME_FOLDER"
    (
        cd "$THEME_FOLDER"
        git clone https://github.com/jonschlinkert/sublime-monokai-extended
        ln -s "sublime-monokai-extended/Monokai Extended.tmTheme" Default.tmTheme
    )
fi

if [ ! -e "$SYNTAX_FOLDER" ]; then
    mkdir -p "$SYNTAX_FOLDER"
    (
        cd "$SYNTAX_FOLDER"
        git clone https://github.com/sublimehq/Packages/

        # Patch JavaScript syntax
        sed -i -e 's/{{identifier_break}}+/{{identifier_break}}/' Packages/JavaScript/JavaScript.sublime-syntax

        # Use extended Markdown syntax
        rm -rf Packages/Markdown
        git clone https://github.com/jonschlinkert/sublime-markdown-extended

        # Add additional sxntax definitions
        git clone https://github.com/princemaple/elixir-sublime-syntax/
        git clone https://github.com/sharkdp/sublime_toml_highlighting
        git clone https://github.com/JuliaEditorSupport/Julia-sublime
    )
fi

bat init-cache

cp "$HOME/.cache/bat"/* .
