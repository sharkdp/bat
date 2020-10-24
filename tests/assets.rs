use bat::assets::HighlightingAssets;

/// This test ensures that we are not accidentally removing themes due to submodule updates.
/// It is 'ignore'd by default because it requires themes.bin to be up-to-date.
#[test]
#[ignore]
fn all_themes_are_present() {
    let assets = HighlightingAssets::from_binary();

    let mut themes: Vec<_> = assets.themes().collect();
    themes.sort();

    assert_eq!(
        themes,
        vec![
            "1337",
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
            "ansi-dark",
            "ansi-light",
            "base16",
            "base16-256",
            "gruvbox",
            "gruvbox-light",
            "gruvbox-white",
            "zenburn"
        ]
    );
}
