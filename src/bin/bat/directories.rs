use std::env;
use std::path::{Path, PathBuf};

use etcetera::BaseStrategy;
use once_cell::sync::Lazy;

/// Wrapper for 'etcetera' that checks BAT_CACHE_PATH and BAT_CONFIG_DIR and falls back to the
/// Windows known folder locations on Windows & the XDG Base Directory Specification everywhere else.
pub struct BatProjectDirs {
    cache_dir: PathBuf,
    config_dir: PathBuf,
}

impl BatProjectDirs {
    fn new() -> Option<BatProjectDirs> {
        let basedirs = etcetera::choose_base_strategy().ok()?;

        // Checks whether or not `$BAT_CACHE_PATH` exists. If it doesn't, set the cache dir to our
        // system's default cache home.
        let cache_dir = if let Some(cache_dir) = env::var_os("BAT_CACHE_PATH").map(PathBuf::from) {
            cache_dir
        } else {
            basedirs.cache_dir().join("bat")
        };

        // Checks whether or not `$BAT_CONFIG_DIR` exists. If it doesn't, set the config dir to our
        // system's default configuration home.
        let config_dir = if let Some(config_dir) = env::var_os("BAT_CONFIG_DIR").map(PathBuf::from)
        {
            config_dir
        } else {
            basedirs.config_dir().join("bat")
        };

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

pub static PROJECT_DIRS: Lazy<BatProjectDirs> =
    Lazy::new(|| BatProjectDirs::new().expect("Could not get home directory"));
