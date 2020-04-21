use std::env;
use std::path::{Path, PathBuf};

use lazy_static::lazy_static;

/// Wrapper for 'dirs' that treats MacOS more like Linux, by following the XDG specification.
/// This means that the `XDG_CACHE_HOME` and `XDG_CONFIG_HOME` environment variables are
/// checked first. The fallback directories are `~/.cache/bat` and `~/.config/bat`, respectively.
pub struct BatProjectDirs {
    cache_dir: PathBuf,
    config_dir: PathBuf,
}

impl BatProjectDirs {
    fn new() -> Option<BatProjectDirs> {
        let cache_dir = BatProjectDirs::get_cache_dir()?;

        #[cfg(target_os = "macos")]
        let config_dir_op = env::var_os("XDG_CONFIG_HOME")
            .map(PathBuf::from)
            .filter(|p| p.is_absolute())
            .or_else(|| dirs::home_dir().map(|d| d.join(".config")));

        #[cfg(not(target_os = "macos"))]
        let config_dir_op = dirs::config_dir();

        let config_dir = config_dir_op.map(|d| d.join("bat"))?;

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
            .or_else(|| dirs::home_dir().map(|d| d.join(".cache")));

        #[cfg(not(target_os = "macos"))]
        let cache_dir_op = dirs::cache_dir();

        cache_dir_op.map(|d| d.join("bat"))
    }

    pub fn cache_dir(&self) -> &Path {
        &self.cache_dir
    }

    pub fn config_dir(&self) -> &Path {
        &self.config_dir
    }
}

lazy_static! {
    pub static ref PROJECT_DIRS: BatProjectDirs =
        BatProjectDirs::new().expect("Could not get home directory");
}
