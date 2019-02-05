use dirs_rs::home_dir;
use std::env;
use std::ffi::OsString;
use std::path::{Path, PathBuf};

/// Wrapper for dirs that treats MacOS more like Linux.
/// First, env variables `XDG_CACHE_HOME` and `XDG_CONFIG_HOME` are checked and the fall back is
/// `~/.cache/bat` and `~/.config/bat`.
pub struct BatProjectDirs {
    cache_dir: PathBuf,
    config_dir: PathBuf,
}

impl BatProjectDirs {
    fn new() -> Option<BatProjectDirs> {
        #[cfg(target_os = "macos")]
        let cache_dir_op = env::var_os("XDG_CACHE_HOME")
            .and_then(is_absolute_path)
            .or_else(|| home_dir().map(|d| d.join(".cache")));

        #[cfg(not(target_os = "macos"))]
        let cache_dir_op = dirs_rs::cache_dir();

        let cache_dir = match cache_dir_op {
            Some(d) => d.join("bat"),
            None => return None,
        };

        #[cfg(target_os = "macos")]
        let config_dir_op = env::var_os("XDG_CONFIG_HOME")
            .and_then(is_absolute_path)
            .or_else(|| home_dir().map(|d| d.join(".config")));

        #[cfg(not(target_os = "macos"))]
        let config_dir_op = dirs_rs::config_dir();

        let config_dir = match config_dir_op {
            Some(d) => d.join("bat"),
            None => return None,
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

// Returns path if it is an absolute path
fn is_absolute_path(path: OsString) -> Option<PathBuf> {
    let path = PathBuf::from(path);
    if path.is_absolute() {
        Some(path)
    } else {
        None
    }
}

lazy_static! {
    pub static ref PROJECT_DIRS: BatProjectDirs =
        BatProjectDirs::new().expect("Could not get home directory");
}
