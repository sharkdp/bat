use std::borrow::Cow;
use std::fs;
use std::path::PathBuf;

use crate::directories::PROJECT_DIRS;

use bat::HighlightingAssets;

fn theme_set_path() -> PathBuf {
    PROJECT_DIRS.cache_dir().join("themes.bin")
}

fn syntax_set_path() -> PathBuf {
    PROJECT_DIRS.cache_dir().join("syntaxes.bin")
}

pub fn config_dir() -> Cow<'static, str> {
    PROJECT_DIRS.config_dir().to_string_lossy()
}

pub fn cache_dir() -> Cow<'static, str> {
    PROJECT_DIRS.cache_dir().to_string_lossy()
}

pub fn clear_assets() {
    print!("Clearing theme set cache ... ");
    fs::remove_file(theme_set_path()).ok();
    println!("okay");

    print!("Clearing syntax set cache ... ");
    fs::remove_file(syntax_set_path()).ok();
    println!("okay");
}

pub fn assets_from_cache_or_binary() -> HighlightingAssets {
    HighlightingAssets::from_cache(&theme_set_path(), &syntax_set_path())
        .unwrap_or(HighlightingAssets::from_binary())
}
