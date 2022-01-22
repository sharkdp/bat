use super::lazy_theme_set::LazyThemeSet;

/// A collection of syntect themes, similar to [`syntect::highlighting::ThemeSet`].
/// The themes are deserialized on-demand only.
#[derive(Debug)]
pub struct ThemeSet(pub LazyThemeSet);

impl ThemeSet {
    /// Look up a theme by name
    pub fn get(&self, name: &str) -> Option<&syntect::highlighting::Theme> {
        self.0.get(name)
    }

    /// An iterator over all theme names
    pub fn theme_names(&self) -> impl Iterator<Item = &str> {
        self.0.themes()
    }
}
