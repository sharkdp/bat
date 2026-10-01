//! Opt-in source-based caches. Explicit caches stay available to library clients;
//! automatic rebuilds use separate, immutable generations for each input set.

use std::collections::hash_map::DefaultHasher;
use std::fs::{self, File};
use std::hash::{Hash, Hasher};
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::process::Command;

use bat::assets::HighlightingAssets;
use bat::assets_metadata::AssetsMetadata;
use bat::error::*;
use serde_derive::{Deserialize, Serialize};

const RECIPE: &str = "automatic.yaml";
const GENERATIONS: &str = "automatic";

#[derive(Serialize, Deserialize)]
pub(crate) struct Recipe {
    source_dir: PathBuf,
    #[serde(default)]
    preceding_source_dirs: Vec<PathBuf>,
    include_integrated_assets: bool,
    include_acknowledgements: bool,
    fingerprint: String,
}

impl Recipe {
    pub(crate) fn new(
        source_dirs: &[&Path],
        cache_dir: &Path,
        include_integrated_assets: bool,
        include_acknowledgements: bool,
    ) -> Result<Self> {
        let mut sources: Vec<PathBuf> = source_dirs
            .iter()
            .map(|source| source.canonicalize())
            .collect::<std::io::Result<_>>()?;
        for source in &sources {
            if !source.is_dir() {
                return Err("The automatic asset sources must be directories".into());
            }
        }
        let source_dir = sources
            .pop()
            .ok_or("At least one automatic asset source is required")?;
        let mut recipe = Self {
            source_dir,
            preceding_source_dirs: sources,
            include_integrated_assets,
            include_acknowledgements,
            fingerprint: String::new(),
        };
        recipe.fingerprint = recipe.current_fingerprint(cache_dir)?;
        Ok(recipe)
    }

    fn source_dirs(&self) -> impl Iterator<Item = &Path> {
        self.preceding_source_dirs
            .iter()
            .chain(std::iter::once(&self.source_dir))
            .map(PathBuf::as_path)
    }

    fn current_fingerprint(&self, cache_dir: &Path) -> Result<String> {
        let mut hash = DefaultHasher::new();
        clap::crate_version!().hash(&mut hash);
        self.include_integrated_assets.hash(&mut hash);
        self.include_acknowledgements.hash(&mut hash);
        let cache_dir = cache_dir.canonicalize().ok();
        let mut buffer = [0; 64 * 1024];
        for source_dir in self.source_dirs() {
            source_dir.hash(&mut hash);
            let entries = walkdir::WalkDir::new(source_dir)
                .follow_links(true)
                .sort_by_file_name()
                .into_iter()
                .filter_entry(|entry| {
                    if entry.depth() == 0 {
                        return true;
                    }
                    if entry.file_name() == ".git" {
                        return false;
                    }
                    if let Some(cache_dir) = &cache_dir {
                        if cache_dir != source_dir && entry.path() == cache_dir {
                            return false;
                        }
                    }
                    // A source can also be the cache target (as with assets/create.sh).
                    if entry.depth() == 1 {
                        return ![
                            RECIPE,
                            GENERATIONS,
                            "metadata.yaml",
                            "syntaxes.bin",
                            "themes.bin",
                            "acknowledgements.bin",
                        ]
                        .iter()
                        .any(|name| entry.file_name() == *name);
                    }
                    true
                });
            for entry in entries {
                let entry = entry.map_err(|error| {
                    format!("Could not inspect automatic asset sources: {error}")
                })?;
                if !entry.file_type().is_file() {
                    continue;
                }
                entry
                    .path()
                    .strip_prefix(source_dir)
                    .map_err(|error| format!("Invalid asset source path: {error}"))?
                    .hash(&mut hash);
                let mut file = File::open(entry.path())?;
                file.metadata()?.len().hash(&mut hash);
                loop {
                    let size = file.read(&mut buffer)?;
                    if size == 0 {
                        break;
                    }
                    hash.write(&buffer[..size]);
                }
            }
        }
        Ok(format!("{:016x}", hash.finish()))
    }
}

pub(crate) fn save_recipe(recipe: Option<&Recipe>, cache_dir: &Path) -> Result<()> {
    let path = cache_dir.join(RECIPE);
    if let Some(recipe) = recipe {
        if recipe.current_fingerprint(cache_dir)? != recipe.fingerprint {
            return Err(
                "Asset sources changed during the build; run bat cache --build --automatic again"
                    .into(),
            );
        }
        let mut file = tempfile::NamedTempFile::new_in(cache_dir)?;
        serde_yaml::to_writer(file.as_file_mut(), recipe)?;
        file.persist(path).map_err(|error| error.error)?;
    } else if let Err(error) = fs::remove_file(path) {
        if error.kind() != io::ErrorKind::NotFound {
            return Err(error.into());
        }
    }
    Ok(())
}

fn read_assets(path: &Path) -> Result<HighlightingAssets> {
    if !AssetsMetadata::load_from_folder(path)?
        .is_some_and(|metadata| metadata.is_compatible_with(clap::crate_version!()))
    {
        return Err("Incompatible automatic asset cache".into());
    }
    let assets = HighlightingAssets::from_cache(path)?;
    // A generation is usable only when both cache files can be read.
    assets.get_syntax_set()?;
    Ok(assets)
}

pub(crate) fn load(cache_dir: &Path) -> Result<Option<HighlightingAssets>> {
    let file = match File::open(cache_dir.join(RECIPE)) {
        Ok(file) => file,
        Err(error) if error.kind() == io::ErrorKind::NotFound => return Ok(None),
        Err(error) => return Err(error.into()),
    };
    let recipe: Recipe = serde_yaml::from_reader(file)?;
    let fingerprint = recipe.current_fingerprint(cache_dir)?;
    if fingerprint == recipe.fingerprint {
        if let Ok(assets) = read_assets(cache_dir) {
            return Ok(Some(assets));
        }
    }
    let generations = cache_dir.join(GENERATIONS);
    fs::create_dir_all(&generations)?;
    let target = generations.join(&fingerprint);
    if let Ok(assets) = read_assets(&target) {
        return Ok(Some(assets));
    }

    let stage = tempfile::Builder::new()
        .prefix("building-")
        .tempdir_in(&generations)?;
    let mut command = Command::new(std::env::current_exe()?);
    // Run in an empty directory so a user file named `cache` cannot hide the
    // subcommand. Capture progress messages so they cannot enter file output.
    command
        .current_dir(stage.path())
        .arg("cache")
        .arg("--build")
        .arg("--target")
        .arg(stage.path());
    for source in recipe.source_dirs() {
        command.arg("--source").arg(source);
    }
    if !recipe.include_integrated_assets {
        command.arg("--blank");
    }
    if recipe.include_acknowledgements {
        command.arg("--acknowledgements");
    }
    let output = command.output()?;
    if !output.status.success() {
        return Err(format!(
            "Could not automatically rebuild custom assets from {:?}:\n{}{}",
            recipe.source_dirs().collect::<Vec<_>>(),
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr),
        )
        .into());
    }
    read_assets(stage.path())?;
    if recipe.current_fingerprint(cache_dir)? != fingerprint {
        return Err("Asset sources changed during the automatic build; retry the command".into());
    }
    // Move a corrupt generation aside before replacement. Its contents remain
    // isolated until the replacement is published, then the temporary directory
    // cleans them up. A concurrent successful publisher wins.
    let quarantine = tempfile::Builder::new()
        .prefix("replacing-")
        .tempdir_in(&generations)?;
    if target.exists() {
        if let Ok(assets) = read_assets(&target) {
            return Ok(Some(assets));
        }
        match fs::rename(&target, quarantine.path().join("old")) {
            Ok(()) => (),
            Err(error) if error.kind() == io::ErrorKind::NotFound => (),
            Err(error) => return Err(error.into()),
        }
    }
    match fs::rename(stage.path(), &target) {
        Ok(()) => (),
        Err(error) => {
            // Another process may have published the same generation.
            if let Ok(assets) = read_assets(&target) {
                return Ok(Some(assets));
            }
            return Err(error.into());
        }
    }
    Ok(Some(read_assets(&target)?))
}
