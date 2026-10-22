use std::fs;
use std::io::{self, Write};
use std::path::Path;
use std::path::PathBuf;

use clap::crate_version;

use bat::assets::HighlightingAssets;
use bat::assets_metadata::AssetsMetadata;
use bat::error::*;

pub fn clear_assets(cache_dir: &Path) -> Result<()> {
    clear_asset(cache_dir.join("themes.bin"), "theme set cache")?;
    clear_asset(cache_dir.join("syntaxes.bin"), "syntax set cache")?;
    clear_asset(cache_dir.join("metadata.yaml"), "metadata file")?;
    Ok(())
}

pub fn assets_from_cache_or_binary(
    use_custom_assets: bool,
    config_dir: &Path,
    cache_dir: &Path,
) -> Result<HighlightingAssets> {
    if use_custom_assets {
        ensure_cache_built(config_dir, cache_dir);
    }

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

fn clear_asset(path: PathBuf, description: &str) -> Result<()> {
    write!(io::stdout(), "Clearing {description} ... ")?;
    match fs::remove_file(&path) {
        Err(err) if err.kind() == io::ErrorKind::NotFound => {
            writeln!(io::stdout(), "skipped (not present)")?;
        }
        Err(err) => {
            writeln!(
                io::stdout(),
                "could not remove the cache file {path:?}: {err}"
            )?;
        }
        Ok(_) => writeln!(io::stdout(), "okay")?,
    }
    Ok(())
}

/// Build the cache on first run when custom themes or syntaxes are present
/// but no cache exists yet (see #4017). A failed build is not fatal: warn
/// and fall back to the integrated assets.
#[cfg(feature = "build-assets")]
fn ensure_cache_built(config_dir: &Path, cache_dir: &Path) {
    if cache_exists(cache_dir) || !custom_sources_exist(config_dir) {
        return;
    }

    eprintln!("bat: custom themes or syntaxes found, building cache ...");
    if let Err(err) = bat::assets::build(config_dir, true, false, cache_dir, crate_version!()) {
        eprintln!("bat: automatic cache build failed ({err}), falling back to integrated assets.");
    }
}

#[cfg(not(feature = "build-assets"))]
fn ensure_cache_built(_config_dir: &Path, _cache_dir: &Path) {}

#[cfg(feature = "build-assets")]
fn cache_exists(cache_dir: &Path) -> bool {
    cache_dir.join("themes.bin").is_file()
        && cache_dir.join("syntaxes.bin").is_file()
        && cache_dir.join("metadata.yaml").is_file()
}

#[cfg(feature = "build-assets")]
fn custom_sources_exist(config_dir: &Path) -> bool {
    dir_has_files(&config_dir.join("themes")) || dir_has_files(&config_dir.join("syntaxes"))
}

#[cfg(feature = "build-assets")]
fn dir_has_files(dir: &Path) -> bool {
    fs::read_dir(dir)
        .map(|entries| {
            entries
                .filter_map(|entry| entry.ok())
                .any(|entry| entry.path().is_file())
        })
        .unwrap_or(false)
}
