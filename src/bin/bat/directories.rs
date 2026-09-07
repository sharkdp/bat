use std::env;
use std::path::{Path, PathBuf};

use etcetera::BaseStrategy;
use once_cell::sync::Lazy;

/// Wrapper for 'etcetera' that checks BAT_CACHE_PATH and BAT_CONFIG_DIR and falls back to the
/// XDG overrides on every platform, then native Windows folders or the XDG defaults.
pub struct BatProjectDirs {
    cache_dir: PathBuf,
    config_dir: PathBuf,
}

impl BatProjectDirs {
    fn new() -> Option<BatProjectDirs> {
        let basedirs = etcetera::choose_base_strategy().ok()?;

        let cache_dir = env::var_os("BAT_CACHE_PATH")
            .map(PathBuf::from)
            .or_else(|| xdg_home("XDG_CACHE_HOME").map(|path| path.join("bat")))
            .unwrap_or_else(|| basedirs.cache_dir().join("bat"));

        let config_dir = env::var_os("BAT_CONFIG_DIR")
            .map(PathBuf::from)
            .or_else(|| xdg_home("XDG_CONFIG_HOME").map(|path| path.join("bat")))
            .unwrap_or_else(|| basedirs.config_dir().join("bat"));

        Some(BatProjectDirs {
            cache_dir,
            config_dir,
        })
    }

    pub fn cache_dir(&self) -> &Path {
        &self.cache_dir
    }

    pub fn config_dir(&self) -> &Path {
        &self.config_dir
    }
}

// XDG requires absolute paths; empty and relative values are ignored.
fn xdg_home(name: &str) -> Option<PathBuf> {
    env::var_os(name)
        .map(PathBuf::from)
        .filter(|path| path.is_absolute())
}

pub static PROJECT_DIRS: Lazy<BatProjectDirs> =
    Lazy::new(|| BatProjectDirs::new().expect("Could not get home directory"));
