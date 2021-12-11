use super::*;

use std::collections::BTreeMap;
use std::convert::TryFrom;

use serde::Deserialize;
use serde::Serialize;

use once_cell::unsync::OnceCell;

use syntect::highlighting::{Theme, ThemeSet};

/// Same structure as a [`syntect::highlighting::ThemeSet`] but with themes
/// stored in raw serialized form, and deserialized on demand.
#[derive(Debug, Default, Serialize, Deserialize)]
pub struct LazyThemeSet {
    /// This is a [`BTreeMap`] because that's what [`syntect::highlighting::ThemeSet`] uses
    themes: BTreeMap<String, LazyTheme>,
}

/// Stores raw serialized data for a theme with methods to lazily deserialize
/// (load) the theme.
#[derive(Debug, Serialize, Deserialize)]
struct LazyTheme {
    serialized: Vec<u8>,

    #[serde(skip, default = "OnceCell::new")]
    deserialized: OnceCell<syntect::highlighting::Theme>,
}

impl LazyThemeSet {
    /// Lazily load the given theme
    pub fn get(&self, name: &str) -> Option<&Theme> {
        self.themes.get(name).and_then(|lazy_theme| {
            lazy_theme
                .deserialized
                .get_or_try_init(|| lazy_theme.deserialize())
                .ok()
        })
    }

    /// Returns the name of all themes.
    pub fn themes(&self) -> impl Iterator<Item = &str> {
        self.themes.keys().map(|name| name.as_ref())
    }
}

impl LazyTheme {
    fn deserialize(&self) -> Result<Theme> {
        asset_from_contents(
            &self.serialized[..],
            "lazy-loaded theme",
            COMPRESS_LAZY_THEMES,
        )
    }
}

impl TryFrom<LazyThemeSet> for ThemeSet {
    type Error = Error;

    /// Since the user might want to add custom themes to bat, we need a way to
    /// convert from a `LazyThemeSet` to a regular [`ThemeSet`] so that more
    /// themes can be added. This function does that pretty straight-forward
    /// conversion.
    fn try_from(lazy_theme_set: LazyThemeSet) -> Result<Self> {
        let mut theme_set = ThemeSet::default();

        for (name, lazy_theme) in lazy_theme_set.themes {
            theme_set.themes.insert(name, lazy_theme.deserialize()?);
        }

        Ok(theme_set)
    }
}

#[cfg(feature = "build-assets")]
impl TryFrom<ThemeSet> for LazyThemeSet {
    type Error = Error;

    /// To collect themes, a [`ThemeSet`] is needed. Once all desired themes
    /// have been added, we need a way to convert that into [`LazyThemeSet`] so
    /// that themes can be lazy-loaded later. This function does that
    /// conversion.
    fn try_from(theme_set: ThemeSet) -> Result<Self> {
        let mut lazy_theme_set = LazyThemeSet::default();

        for (name, theme) in theme_set.themes {
            // All we have to do is to serialize the theme
            let lazy_theme = LazyTheme {
                serialized: crate::assets::build_assets::asset_to_contents(
                    &theme,
                    &format!("theme {}", name),
                    COMPRESS_LAZY_THEMES,
                )?,
                deserialized: OnceCell::new(),
            };

            // Ok done, now we can add it
            lazy_theme_set.themes.insert(name, lazy_theme);
        }

        Ok(lazy_theme_set)
    }
}
