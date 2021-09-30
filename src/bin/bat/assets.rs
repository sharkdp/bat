use std::borrow::Cow;
use std::fs;
use std::io;

use clap::crate_version;

use crate::directories::PROJECT_DIRS;

use bat::assets::HighlightingAssets;
use bat::assets_metadata::AssetsMetadata;
use bat::error::*;

pub fn config_dir() -> Cow<'static, str> {
    PROJECT_DIRS.config_dir().to_string_lossy()
}

pub fn cache_dir() -> Cow<'static, str> {
    PROJECT_DIRS.cache_dir().to_string_lossy()
}

pub fn clear_assets() {
    clear_asset("themes.bin", "theme set cache");
    clear_asset("syntaxes.bin", "syntax set cache");
    clear_asset("metadata.yaml", "metadata file");
}

pub fn assets_from_cache_or_binary(use_custom_assets: bool) -> Result<HighlightingAssets> {
    let cache_dir = PROJECT_DIRS.cache_dir();
    if let Some(metadata) = AssetsMetadata::load_from_folder(cache_dir)? {
        if !metadata.is_compatible_with(crate_version!()) {
            return Err(format!(
                "The binary caches for the user-customized syntaxes and themes \
                 in '{}' are not compatible with this version of bat ({}). To solve this, \
                 either rebuild the cache (bat cache --build) or remove \
                 the custom syntaxes/themes (bat cache --clear).\n\
                 For more information, see:\n\n  \
                 https://github.com/sharkdp/bat#adding-new-syntaxes--language-definitions",
                cache_dir.to_string_lossy(),
                crate_version!()
            )
            .into());
        }
    }

    let custom_assets = if use_custom_assets {
        HighlightingAssets::from_cache(cache_dir).ok()
    } else {
        None
    };
    Ok(custom_assets.unwrap_or_else(HighlightingAssets::from_binary))
}

fn clear_asset(filename: &str, description: &str) {
    print!("Clearing {} ... ", description);
    let path = PROJECT_DIRS.cache_dir().join(filename);
    match fs::remove_file(&path) {
        Err(err) if err.kind() == io::ErrorKind::NotFound => {
            println!("skipped (not present)");
        }
        Err(err) => {
            println!("could not remove the cache file {:?}: {}", &path, err);
        }
        Ok(_) => println!("okay"),
    }
}
