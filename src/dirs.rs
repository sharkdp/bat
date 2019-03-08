use crate::dirs_rs;
use std::path::{Path, PathBuf};

#[cfg(target_os = "macos")]
use std::env;

/// Wrapper for 'dirs' that treats MacOS more like Linux, by following the XDG specification.
/// This means that the `XDG_CACHE_HOME` and `XDG_CONFIG_HOME` environment variables are
/// checked first. The fallback directories are `~/.cache/bat` and `~/.config/bat`, respectively.
pub struct BatProjectDirs {
    cache_dir: PathBuf,
    config_dir: PathBuf,
}

impl BatProjectDirs {
    fn new() -> Option<BatProjectDirs> {
        #[cfg(target_os = "macos")]
        let cache_dir_op = env::var_os("XDG_CACHE_HOME")
            .map(PathBuf::from)
            .filter(|p| p.is_absolute())
            .or_else(|| dirs_rs::home_dir().map(|d| d.join(".cache")));

        #[cfg(not(target_os = "macos"))]
        let cache_dir_op = dirs_rs::cache_dir();

        let cache_dir = cache_dir_op.map(|d| d.join("bat"))?;

        #[cfg(target_os = "macos")]
        let config_dir_op = env::var_os("XDG_CONFIG_HOME")
            .map(PathBuf::from)
            .filter(|p| p.is_absolute())
            .or_else(|| dirs_rs::home_dir().map(|d| d.join(".config")));

        #[cfg(not(target_os = "macos"))]
        let config_dir_op = dirs_rs::config_dir();

        let config_dir = config_dir_op.map(|d| d.join("bat"))?;

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

lazy_static! {
    pub static ref PROJECT_DIRS: BatProjectDirs =
        BatProjectDirs::new().expect("Could not get home directory");
}
