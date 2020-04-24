use std::borrow::Cow;
use std::fs;

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
    let theme_set_path = PROJECT_DIRS.cache_dir().join("themes.bin");
    let syntax_set_path = PROJECT_DIRS.cache_dir().join("syntaxes.bin");
    let metadata_file = PROJECT_DIRS.cache_dir().join("metadata.yaml");

    print!("Clearing theme set cache ... ");
    fs::remove_file(theme_set_path).ok();
    println!("okay");

    print!("Clearing syntax set cache ... ");
    fs::remove_file(syntax_set_path).ok();
    println!("okay");

    print!("Clearing metadata file ... ");
    fs::remove_file(metadata_file).ok();
    println!("okay");
}

pub fn assets_from_cache_or_binary() -> Result<HighlightingAssets> {
    let cache_dir = PROJECT_DIRS.cache_dir();
    if let Some(metadata) = AssetsMetadata::load_from_folder(&cache_dir)? {
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

    Ok(HighlightingAssets::from_cache(&cache_dir)
        .unwrap_or_else(|_| HighlightingAssets::from_binary()))
}
