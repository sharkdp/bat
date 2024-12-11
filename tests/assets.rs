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
            "Visual Studio Dark+",
            "ansi",
            "base16",
            "base16-256",
            "base2tone-cave-dark",
            "base2tone-desert-dark",
            "base2tone-drawbridge-dark",
            "base2tone-earth-dark",
            "base2tone-evening-dark",
            "base2tone-field-dark",
            "base2tone-forest-dark",
            "base2tone-garden-dark",
            "base2tone-heath-dark",
            "base2tone-lake-dark",
            "base2tone-lavender-dark",
            "base2tone-mall-dark",
            "base2tone-meadow-dark",
            "base2tone-morning-dark",
            "base2tone-motel-dark",
            "base2tone-pool-dark",
            "base2tone-porch-dark",
            "base2tone-sea-dark",
            "base2tone-space-dark",
            "base2tone-suburb-dark",
            "gruvbox-dark",
            "gruvbox-light",
            "zenburn"
        ]
    );
}
