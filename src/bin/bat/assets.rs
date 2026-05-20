use std::fs;
use std::io;
use std::path::Path;
use std::path::PathBuf;

use clap::crate_version;

use bat::assets::HighlightingAssets;
use bat::assets_metadata::AssetsMetadata;
use bat::error::*;

pub fn clear_assets(cache_dir: &Path) {
    clear_asset(cache_dir.join("themes.bin"), "theme set cache");
    clear_asset(cache_dir.join("syntaxes.bin"), "syntax set cache");
    clear_asset(cache_dir.join("metadata.yaml"), "metadata file");
}

pub fn assets_from_cache_or_binary(
    use_custom_assets: bool,
    cache_dir: &Path,
) -> Result<HighlightingAssets> {
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

fn clear_asset(path: PathBuf, description: &str) {
    print!("Clearing {description} ... ");
    match fs::remove_file(&path) {
        Err(err) if err.kind() == io::ErrorKind::NotFound => {
            println!("skipped (not present)");
        }
        Err(err) => {
            println!("could not remove the cache file {path:?}: {err}");
        }
        Ok(_) => println!("okay"),
    }
}
