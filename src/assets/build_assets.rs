use std::convert::TryInto;
use std::path::Path;

use syntect::highlighting::ThemeSet;
use syntect::parsing::{SyntaxSet, SyntaxSetBuilder};

use crate::assets::*;
use acknowledgements::build_acknowledgements;

mod acknowledgements;

pub fn build(
    source_dir: &Path,
    include_integrated_assets: bool,
    include_acknowledgements: bool,
    target_dir: &Path,
    current_version: &str,
) -> Result<()> {
    let theme_set = build_theme_set(source_dir, include_integrated_assets)?;

    let syntax_set_builder = build_syntax_set_builder(source_dir, include_integrated_assets)?;

    let syntax_set = syntax_set_builder.build();

    let acknowledgements = build_acknowledgements(source_dir, include_acknowledgements)?;

    print_unlinked_contexts(&syntax_set);

    write_assets(
        &theme_set,
        &syntax_set,
        &acknowledgements,
        target_dir,
        current_version,
    )
}

fn build_theme_set(source_dir: &Path, include_integrated_assets: bool) -> Result<LazyThemeSet> {
    let mut theme_set = if include_integrated_assets {
        crate::assets::get_integrated_themeset().try_into()?
    } else {
        ThemeSet::new()
    };

    let theme_dir = source_dir.join("themes");
    if theme_dir.exists() {
        let res = theme_set.add_from_folder(&theme_dir);
        if let Err(err) = res {
            println!(
                "Failed to load one or more themes from '{}' (reason: '{err}')",
                theme_dir.to_string_lossy(),
            );
        }
    } else {
        println!(
            "No themes were found in '{}', using the default set",
            theme_dir.to_string_lossy()
        );
    }

    theme_set.try_into()
}

fn build_syntax_set_builder(
    source_dir: &Path,
    include_integrated_assets: bool,
) -> Result<SyntaxSetBuilder> {
    let mut syntax_set_builder = if !include_integrated_assets {
        let mut builder = syntect::parsing::SyntaxSetBuilder::new();
        builder.add_plain_text_syntax();
        builder
    } else {
        from_binary::<SyntaxSet>(get_serialized_integrated_syntaxset(), COMPRESS_SYNTAXES)
            .into_builder()
    };

    let syntax_dir = source_dir.join("syntaxes");
    if syntax_dir.exists() {
        syntax_set_builder.add_from_folder(syntax_dir, true)?;
    } else {
        println!(
            "No syntaxes were found in '{}', using the default set.",
            syntax_dir.to_string_lossy()
        );
    }

    Ok(syntax_set_builder)
}

fn print_unlinked_contexts(syntax_set: &SyntaxSet) {
    let missing_contexts = syntax_set.find_unlinked_contexts();
    if !missing_contexts.is_empty() {
        println!("Some referenced contexts could not be found!");
        for context in missing_contexts {
            println!("- {context}");
        }
    }
}

fn write_assets(
    theme_set: &LazyThemeSet,
    syntax_set: &SyntaxSet,
    acknowledgements: &Option<String>,
    target_dir: &Path,
    current_version: &str,
) -> Result<()> {
    let _ = std::fs::create_dir_all(target_dir);
    asset_to_cache(
        theme_set,
        &target_dir.join("themes.bin"),
        "theme set",
        COMPRESS_THEMES,
    )?;
    asset_to_cache(
        syntax_set,
        &target_dir.join("syntaxes.bin"),
        "syntax set",
        COMPRESS_SYNTAXES,
    )?;

    if let Some(acknowledgements) = acknowledgements {
        asset_to_cache(
            acknowledgements,
            &target_dir.join("acknowledgements.bin"),
            "acknowledgements",
            COMPRESS_ACKNOWLEDGEMENTS,
        )?;
    }

    print!(
        "Writing metadata to folder {} ... ",
        target_dir.to_string_lossy()
    );
    crate::assets_metadata::AssetsMetadata::new(current_version).save_to_folder(target_dir)?;
    println!("okay");

    Ok(())
}

pub(crate) fn asset_to_contents<T: serde::Serialize>(
    asset: &T,
    description: &str,
    compressed: bool,
) -> Result<Vec<u8>> {
    let mut contents = vec![];
    if compressed {
        bincode::serialize_into(
            flate2::write::ZlibEncoder::new(&mut contents, flate2::Compression::best()),
            asset,
        )
    } else {
        bincode::serialize_into(&mut contents, asset)
    }
    .map_err(|_| format!("Could not serialize {description}"))?;
    Ok(contents)
}

fn asset_to_cache<T: serde::Serialize>(
    asset: &T,
    path: &Path,
    description: &str,
    compressed: bool,
) -> Result<()> {
    print!("Writing {description} to {} ... ", path.to_string_lossy());
    let contents = asset_to_contents(asset, description, compressed)?;
    std::fs::write(path, &contents[..])
        .map_err(|_| format!("Could not save {description} to {}", path.to_string_lossy()))?;
    println!("okay");
    Ok(())
}
