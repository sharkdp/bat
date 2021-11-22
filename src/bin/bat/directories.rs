use std::env;
use std::path::{Path, PathBuf};

use once_cell::sync::Lazy;

/// Wrapper for 'dirs' that treats MacOS more like Linux, by following the XDG specification.
/// The `XDG_CACHE_HOME` environment variable is checked first. `BAT_CONFIG_DIR`
///  is then checked before the `XDG_CONFIG_HOME` environment variable.
///  The fallback directories are `~/.cache/bat` and `~/.config/bat`, respectively.
pub struct BatProjectDirs {
    cache_dir: PathBuf,
    config_dir: PathBuf,
}

impl BatProjectDirs {
    fn new() -> Option<BatProjectDirs> {
        let cache_dir = BatProjectDirs::get_cache_dir()?;

        // Checks whether or not $BAT_CONFIG_DIR exists. If it doesn't, set our config dir
        // to our system's default configuration home.
        let config_dir =
            if let Some(config_dir_op) = env::var_os("BAT_CONFIG_DIR").map(PathBuf::from) {
                config_dir_op
            } else {
                #[cfg(target_os = "macos")]
                let config_dir_op = env::var_os("XDG_CONFIG_HOME")
                    .map(PathBuf::from)
                    .filter(|p| p.is_absolute())
                    .or_else(|| dirs_next::home_dir().map(|d| d.join(".config")));

                #[cfg(not(target_os = "macos"))]
                let config_dir_op = dirs_next::config_dir();

                config_dir_op.map(|d| d.join("bat"))?
            };

        Some(BatProjectDirs {
            cache_dir,
            config_dir,
        })
    }

    fn get_cache_dir() -> Option<PathBuf> {
        // on all OS prefer BAT_CACHE_PATH if set
        let cache_dir_op = env::var_os("BAT_CACHE_PATH").map(PathBuf::from);
        if cache_dir_op.is_some() {
            return cache_dir_op;
        }

        #[cfg(target_os = "macos")]
        let cache_dir_op = env::var_os("XDG_CACHE_HOME")
            .map(PathBuf::from)
            .filter(|p| p.is_absolute())
            .or_else(|| dirs_next::home_dir().map(|d| d.join(".cache")));

        #[cfg(not(target_os = "macos"))]
        let cache_dir_op = dirs_next::cache_dir();

        cache_dir_op.map(|d| d.join("bat"))
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
