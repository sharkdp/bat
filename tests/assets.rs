use bat::assets::HighlightingAssets;

/// This test ensures that we are not accidentally removing themes due to submodule updates.
/// It is 'ignore'd by default because it requires themes.bin to be up-to-date.
#[test]
#[ignore]
fn all_themes_are_present() {
    let assets = HighlightingAssets::from_binary();

    let mut themes: Vec<_> = assets.themes().collect();
    themes.sort_unstable();

    assert_eq!(
        themes,
        vec![
            "1337",
            "Catppuccin Frappe",
            "Catppuccin Latte",
            "Catppuccin Macchiato",
            "Catppuccin Mocha",
            "Coldark-Cold",
            "Coldark-Dark",
            "DarkNeon",
            "Dracula",
            "GitHub",
            "Monokai Extended",
            "Monokai Extended Bright",
            "Monokai Extended Light",
            "Monokai Extended Origin",
            "Nord",
            "OneHalfDark",
            "OneHalfLight",
            "Solarized (dark)",
            "Solarized (light)",
            "Sublime Snazzy",
            "TwoDark",
            "ansi",
            "base16",
            "base16-256",
            "gruvbox-dark",
            "gruvbox-light",
            "zenburn"
        ]
    );
}

/// Requires the rebuilt theme assets, like the theme inventory test above.
#[test]
#[ignore]
fn markdown_table_delimiters_have_a_distinct_foreground() {
    use syntect::highlighting::Highlighter;
    use syntect::parsing::Scope;

    let assets = HighlightingAssets::from_binary();
    let highlighter = Highlighter::new(assets.get_theme("Monokai Extended"));
    let base = Scope::new("text.html.markdown").unwrap();
    let normal = highlighter.style_for_stack(&[base]);
    for scope in [
        "punctuation.separator.table-cell",
        "punctuation.section.table-header",
    ] {
        let styled = highlighter.style_for_stack(&[base, Scope::new(scope).unwrap()]);
        assert_ne!(styled.foreground, normal.foreground, "{scope}");
    }
}
