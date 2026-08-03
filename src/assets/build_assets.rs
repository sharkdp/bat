use std::convert::TryInto;
use std::path::Path;

use syntect::highlighting::ThemeSet;
use syntect::parsing::syntax_definition::{ContextReference, Pattern};
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
        let custom_syntax_start = syntax_set_builder.syntaxes().len();
        syntax_set_builder.add_from_folder(syntax_dir, true)?;
        reject_self_referential_scope_includes(&syntax_set_builder, custom_syntax_start)?;
    } else {
        println!(
            "No syntaxes were found in '{}', using the default set.",
            syntax_dir.to_string_lossy()
        );
    }

    Ok(syntax_set_builder)
}

fn reject_self_referential_scope_includes(
    syntax_set_builder: &SyntaxSetBuilder,
    custom_syntax_start: usize,
) -> Result<()> {
    let syntaxes = syntax_set_builder.syntaxes();

    for (syntax_index, syntax) in syntaxes.iter().enumerate().skip(custom_syntax_start) {
        for context in syntax.contexts.values() {
            for pattern in &context.patterns {
                let Pattern::Include(ContextReference::ByScope { scope, .. }) = pattern else {
                    continue;
                };
                if syntaxes
                    .iter()
                    .rposition(|candidate| candidate.scope == *scope)
                    == Some(syntax_index)
                {
                    return Err(format!(
                        "Syntax '{}' contains a recursive 'include: scope:' reference",
                        syntax.name
                    )
                    .into());
                }
            }
        }
    }

    Ok(())
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

#[cfg(test)]
mod tests {
    use tempfile::TempDir;

    use super::*;

    #[test]
    fn rejects_self_referential_scope_include() {
        let source_dir = TempDir::new().expect("temporary source directory can be created");
        let target_dir = TempDir::new().expect("temporary target directory can be created");
        let syntax_dir = source_dir.path().join("syntaxes");

        std::fs::create_dir(&syntax_dir).expect("syntax directory can be created");
        std::fs::write(
            syntax_dir.join("loop.sublime-syntax"),
            r#"%YAML 1.2
---
name: Loopy Python
file_extensions: [loopy]
scope: source.python
contexts:
  main:
    - include: scope:source.python
"#,
        )
        .expect("recursive syntax definition can be written");

        let error = build(
            source_dir.path(),
            false,
            false,
            target_dir.path(),
            env!("CARGO_PKG_VERSION"),
        )
        .expect_err("self-referential scope include must be rejected");

        assert_eq!(
            error.to_string(),
            "Syntax 'Loopy Python' contains a recursive 'include: scope:' reference"
        );
        assert!(!target_dir.path().join("syntaxes.bin").exists());
    }

    #[test]
    fn accepts_include_from_another_scope() {
        let source_dir = TempDir::new().expect("temporary source directory can be created");
        let target_dir = TempDir::new().expect("temporary target directory can be created");
        let syntax_dir = source_dir.path().join("syntaxes");

        std::fs::create_dir(&syntax_dir).expect("syntax directory can be created");
        std::fs::write(
            syntax_dir.join("Base.sublime-syntax"),
            r#"%YAML 1.2
---
name: Base
file_extensions: [base]
scope: source.base
contexts:
  main:
    - match: .
"#,
        )
        .expect("base syntax definition can be written");
        std::fs::write(
            syntax_dir.join("Wrapper.sublime-syntax"),
            r#"%YAML 1.2
---
name: Wrapper
file_extensions: [wrapper]
scope: source.wrapper
contexts:
  main:
    - include: scope:source.base
"#,
        )
        .expect("wrapper syntax definition can be written");

        build(
            source_dir.path(),
            false,
            false,
            target_dir.path(),
            env!("CARGO_PKG_VERSION"),
        )
        .expect("include from another scope can be cached");

        assert!(target_dir.path().join("syntaxes.bin").exists());
    }
}
