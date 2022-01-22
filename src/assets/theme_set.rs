use super::lazy_theme_set::LazyThemeSet;

/// A collection of syntect themes, similar to [`syntect::highlighting::ThemeSet`].
/// The themes are deserialized on-demand only.
#[derive(Debug)]
pub enum ThemeSet {
    LazyThemeSet(LazyThemeSet),
}

impl ThemeSet {
    /// Look up a theme by name
    pub fn get(&self, name: &str) -> Option<&syntect::highlighting::Theme> {
        match self {
            ThemeSet::LazyThemeSet(lazy_theme_set) => lazy_theme_set.get(name),
        }
    }

    /// An iterator over all theme names
    pub fn theme_names(&self) -> impl Iterator<Item = &str> {
        match self {
            ThemeSet::LazyThemeSet(lazy_theme_set) => lazy_theme_set.themes(),
        }
    }
}
