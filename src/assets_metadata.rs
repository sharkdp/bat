use std::fs::File;
use std::path::Path;
use std::time::SystemTime;

use semver::Version;
use serde::{Deserialize, Serialize};

use crate::error::*;

#[derive(Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct AssetsMetadata {
    bat_version: Option<String>,
    creation_time: Option<SystemTime>,
}

const FILENAME: &str = "metadata.yaml";

impl AssetsMetadata {
    pub(crate) fn new(current_version: &str) -> AssetsMetadata {
        AssetsMetadata {
            bat_version: Some(current_version.to_owned()),
            creation_time: Some(SystemTime::now()),
        }
    }

    pub(crate) fn save_to_folder(&self, path: &Path) -> Result<()> {
        let file = File::create(path.join(FILENAME))?;
        serde_yaml::to_writer(file, self)?;

        Ok(())
    }

    fn try_load_from_folder(path: &Path) -> Result<Self> {
        let file = File::open(path.join(FILENAME))?;
        Ok(serde_yaml::from_reader(file)?)
    }

    /// Load metadata about the stored cache file from the given folder.
    ///
    /// There are several possibilities:
    ///   - We find a metadata.yaml file and are able to parse it
    ///       => return the contained information
    ///   - We find a metadata.yaml file and but are not able to parse it
    ///       => return a SerdeYamlError
    ///   - We do not find a metadata.yaml file but a syntaxes.bin or themes.bin file
    ///       => assume that these were created by an old version of bat and return
    ///          AssetsMetadata::default() without version information
    ///   - We do not find a metadata.yaml file and no cached assets
    ///       => no user provided assets are available, return None
    pub fn load_from_folder(path: &Path) -> Result<Option<Self>> {
        match Self::try_load_from_folder(path) {
            Ok(metadata) => Ok(Some(metadata)),
            Err(e) => match e.kind() {
                ErrorKind::SerdeYamlError(_) => Err(e),
                _ => {
                    if path.join("syntaxes.bin").exists() || path.join("themes.bin").exists() {
                        Ok(Some(Self::default()))
                    } else {
                        Ok(None)
                    }
                }
            },
        }
    }

    pub fn is_compatible_with(&self, current_version: &str) -> bool {
        let current_version =
            Version::parse(current_version).expect("bat follows semantic versioning");
        let stored_version = self
            .bat_version
            .as_ref()
            .and_then(|ver| Version::parse(ver).ok());

        if let Some(stored_version) = stored_version {
            current_version.major == stored_version.major
                && current_version.minor == stored_version.minor
        } else {
            false
        }
    }
}
