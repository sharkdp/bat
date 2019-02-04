use dirs_rs;
use std::path::{Path, PathBuf};

/// Wrapper for dirs that uses `~/.config/bat` for MacOS.
pub struct BatProjectDirs {
    cache_dir: PathBuf,
    config_dir: PathBuf,
}

impl BatProjectDirs {
    fn new() -> Option<BatProjectDirs> {
        #[cfg(target_os = "macos")]
        let cache_dir = match dirs_rs::home_dir() {
            Some(mut d) => {
                d.push(".cache/bat");
                d
            }
            None => return None,
        };

        #[cfg(not(target_os = "macos"))]
        let cache_dir = match dirs_rs::cache_dir() {
            Some(d) => d,
            None => return None,
        };

        #[cfg(target_os = "macos")]
        let config_dir = match dirs_rs::home_dir() {
            Some(mut d) => {
                d.push(".config/bat");
                d
            }
            None => return None,
        };

        #[cfg(not(target_os = "macos"))]
        let config_dir = match dirs_rs::config_dir() {
            Some(d) => d,
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

lazy_static! {
    pub static ref PROJECT_DIRS: BatProjectDirs =
        BatProjectDirs::new().expect("Could not get home directory");
}
