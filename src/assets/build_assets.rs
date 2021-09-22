use std::collections::HashMap;
use std::path::Path;
use syntect::highlighting::ThemeSet;
use syntect::parsing::syntax_definition::{
    ContextReference, MatchOperation, MatchPattern, Pattern, SyntaxDefinition,
};
use syntect::parsing::{Scope, SyntaxSet, SyntaxSetBuilder};

use crate::assets::*;

mod graphviz_utils;

type SyntaxName = String;

/// Used to look up which [SyntaxDefinition] corresponds to a given [OtherSyntax]
type OtherSyntaxLookup<'a> = HashMap<OtherSyntax, &'a SyntaxDefinition>;

/// Used to look up what dependencies a given [SyntaxDefinition] has
type SyntaxToDependencies = HashMap<SyntaxName, Vec<OtherSyntax>>;

/// Used to look up what other [SyntaxDefinition]s depend on a given [SyntaxDefinition]
type SyntaxToDependents<'a> = HashMap<SyntaxName, Vec<OtherSyntax>>;

/// Represents some other `*.sublime-syntax` file, i.e. another [SyntaxDefinition].
#[derive(Debug, Eq, PartialEq, PartialOrd, Ord, Clone, Hash)]
pub(crate) enum OtherSyntax {
    /// By name. Example YAML: `include: C.sublime-syntax` (name is `"C"`)
    ByName(String),

    /// By scope. Example YAML: `embed: scope:source.c` (scope is `"source.c"`)
    ByScope(Scope),
}

pub fn build(
    source_dir: &Path,
    include_integrated_assets: bool,
    target_dir: &Path,
    current_version: &str,
) -> Result<()> {
    let theme_set = build_theme_set(source_dir, include_integrated_assets);

    let syntax_set_builder = build_syntax_set_builder(source_dir, include_integrated_assets)?;

    let minimal_syntaxes = build_minimal_syntaxes(&syntax_set_builder, include_integrated_assets)?;

    let syntax_set = syntax_set_builder.build();

    print_unlinked_contexts(&syntax_set);

    write_assets(
        &theme_set,
        &syntax_set,
        &minimal_syntaxes,
        target_dir,
        current_version,
    )
}

fn build_theme_set(source_dir: &Path, include_integrated_assets: bool) -> ThemeSet {
    let mut theme_set = if include_integrated_assets {
        crate::assets::get_integrated_themeset()
    } else {
        ThemeSet::new()
    };

    let theme_dir = source_dir.join("themes");
    if theme_dir.exists() {
        let res = theme_set.add_from_folder(&theme_dir);
        if let Err(err) = res {
            println!(
                "Failed to load one or more themes from '{}' (reason: '{}')",
                theme_dir.to_string_lossy(),
                err,
            );
        }
    } else {
        println!(
            "No themes were found in '{}', using the default set",
            theme_dir.to_string_lossy()
        );
    }

    theme_set
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
            println!("- {}", context);
        }
    }
}

fn write_assets(
    theme_set: &ThemeSet,
    syntax_set: &SyntaxSet,
    minimal_syntaxes: &MinimalSyntaxes,
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
    asset_to_cache(
        minimal_syntaxes,
        &target_dir.join("minimal_syntaxes.bin"),
        "minimal syntax sets",
        COMPRESS_MINIMAL_SYNTAXES,
    )?;

    print!(
        "Writing metadata to folder {} ... ",
        target_dir.to_string_lossy()
    );
    crate::assets_metadata::AssetsMetadata::new(current_version).save_to_folder(target_dir)?;
    println!("okay");

    Ok(())
}

fn print_syntax_set_names(syntax_set: &SyntaxSet) {
    let names = syntax_set
        .syntaxes()
        .iter()
        .map(|syntax| &syntax.name)
        .collect::<Vec<_>>();
    println!("{:?}", names);
}

fn build_minimal_syntaxes(
    syntax_set_builder: &'_ SyntaxSetBuilder,
    include_integrated_assets: bool,
) -> Result<MinimalSyntaxes> {
    let mut minimal_syntaxes = MinimalSyntaxes {
        by_name: HashMap::new(),
        serialized_syntax_sets: vec![],
    };

    if include_integrated_assets {
        // Dependency info is not present in integrated assets, so we can't
        // calculate minimal syntax sets. Return early without any data filled
        // in. This means that no minimal syntax sets will be available to use, and
        // the full, slow-to-deserialize, fallback syntax set will be used instead.
        return Ok(minimal_syntaxes);
    }

    let minimal_syntax_sets_to_serialize = build_minimal_syntax_sets(syntax_set_builder)
        // For now, only store syntax sets with one syntax, otherwise
        // the binary grows by several megs
        .filter(|syntax_set| syntax_set.syntaxes().len() == 1);

    for minimal_syntax_set in minimal_syntax_sets_to_serialize {
        // Remember what index it is found at
        let current_index = minimal_syntaxes.serialized_syntax_sets.len();

        for syntax in minimal_syntax_set.syntaxes() {
            minimal_syntaxes
                .by_name
                .insert(syntax.name.to_ascii_lowercase().clone(), current_index);
        }

        let serialized_syntax_set = asset_to_contents(
            &minimal_syntax_set,
            &format!("failed to serialize minimal syntax set {}", current_index),
            COMPRESS_SERIALIZED_MINIMAL_SYNTAXES,
        )?;

        // Add last so that it ends up at `current_index`
        minimal_syntaxes
            .serialized_syntax_sets
            .push(serialized_syntax_set);
    }

    Ok(minimal_syntaxes)
}

/// Analyzes dependencies between syntaxes in a [SyntaxSetBuilder].
/// From that, it builds minimal [SyntaxSet]s.
fn build_minimal_syntax_sets(
    syntax_set_builder: &'_ SyntaxSetBuilder,
) -> impl Iterator<Item = SyntaxSet> + '_ {
    let syntaxes = syntax_set_builder.syntaxes();

    // Build the data structures we need for dependency resolution
    let (other_syntax_lookup, syntax_to_dependencies, syntax_to_dependents) =
        generate_maps(syntaxes);

    maybe_write_syntax_dependencies_to_graphviz_dot_file(
        &other_syntax_lookup,
        &syntax_to_dependencies,
    );

    // Create one minimal SyntaxSet from each (non-hidden) SyntaxDefinition
    syntaxes.iter().filter_map(move |syntax| {
        if syntax.hidden {
            return None;
        }

        let mut builder = SyntaxSetDependencyBuilder::new();
        builder.add_with_dependencies(
            syntax,
            &other_syntax_lookup,
            &syntax_to_dependencies,
            &syntax_to_dependents,
        );
        let syntax_set = builder.build();

        if std::env::var("BAT_PRINT_SYNTAX_DEPENDENCIES").is_ok() {
            // To trigger this code, run:
            // BAT_PRINT_SYNTAX_DEPENDENCIES=1 cargo run -- cache --build --source assets --blank --target /tmp
            print_syntax_set_names(&syntax_set);
        }

        Some(syntax_set)
    })
}

/// In order to analyze dependencies, we need three key pieces of data.
///
///  * When we have a [OtherSyntax], we need to know what [SyntaxDefinition]
///    that corresponds to
///  * When we have a [SyntaxDefinition], we need to know what dependencies it
///    has
///  * When we have a [SyntaxDefinition], we need to know what other syntaxes
///    depend on it
///
/// This functions generates that data for each syntax.
fn generate_maps(
    syntaxes: &[SyntaxDefinition],
) -> (OtherSyntaxLookup, SyntaxToDependencies, SyntaxToDependents) {
    let mut other_syntax_lookup = HashMap::new();
    let mut syntax_to_dependencies = HashMap::new();
    let mut syntax_to_dependents = HashMap::new();

    for syntax in syntaxes {
        other_syntax_lookup.insert(OtherSyntax::ByName(syntax.name.clone()), syntax);
        other_syntax_lookup.insert(OtherSyntax::ByScope(syntax.scope), syntax);
    }

    for syntax in syntaxes {
        let dependencies = dependencies_for_syntax(syntax);

        for dependency in &dependencies {
            if let Some(dependency) = other_syntax_lookup.get(dependency) {
                syntax_to_dependents
                    .entry(dependency.name.clone())
                    .or_insert_with(Vec::new)
                    .push(OtherSyntax::ByName(syntax.name.clone()));
            }
        }

        syntax_to_dependencies.insert(syntax.name.clone(), dependencies);
    }

    (
        other_syntax_lookup,
        syntax_to_dependencies,
        syntax_to_dependents,
    )
}

/// Gets what external dependencies a given [SyntaxDefinition] has.
/// An external dependency is another `.sublime-syntax` file.
/// It does that by looking for variants of the following YAML patterns:
/// - `include: C.sublime-syntax`
/// - `embed: scope:source.c`
fn dependencies_for_syntax(syntax: &SyntaxDefinition) -> Vec<OtherSyntax> {
    let mut dependencies: Vec<OtherSyntax> = syntax
        .contexts
        .values()
        .flat_map(|context| &context.patterns)
        .flat_map(dependencies_from_pattern)
        .collect();

    // No need to track a dependency more than once
    dependencies.sort();
    dependencies.dedup();

    dependencies
}

fn dependencies_from_pattern(pattern: &Pattern) -> Vec<OtherSyntax> {
    match *pattern {
        Pattern::Match(MatchPattern {
            operation: MatchOperation::Push(ref context_references),
            ..
        }) => context_references
            .iter()
            .map(dependency_from_context_reference)
            .collect(),
        Pattern::Include(ref context_reference) => {
            vec![dependency_from_context_reference(context_reference)]
        }
        _ => vec![],
    }
    .into_iter()
    .flatten()
    .collect()
}

/// To generate a Graphviz dot file of syntax dependencies, do this:
/// ```bash
/// sudo apt install graphviz
/// BAT_SYNTAX_DEPENDENCIES_TO_GRAPHVIZ_DOT_FILE=/tmp/bat-syntax-dependencies.dot cargo run -- cache  --build --source assets --blank --target /tmp
/// dot /tmp/bat-syntax-dependencies.dot -Tpng -o /tmp/bat-syntax-dependencies.png
/// open /tmp/bat-syntax-dependencies.png
/// ```
fn maybe_write_syntax_dependencies_to_graphviz_dot_file(
    other_syntax_lookup: &OtherSyntaxLookup,
    syntax_to_dependencies: &SyntaxToDependencies,
) {
    if let Ok(dot_file_path) = std::env::var("BAT_SYNTAX_DEPENDENCIES_TO_GRAPHVIZ_DOT_FILE") {
        graphviz_utils::try_syntax_dependencies_to_graphviz_dot_file(
            other_syntax_lookup,
            syntax_to_dependencies,
            &dot_file_path,
        );
    }
}

/// Removes any context name from the syntax reference.
///
/// When we track dependencies between syntaxes, we are not interested in
/// dependencies on specific contexts inside other syntaxes. We only care about
/// the dependency on the syntax itself.
///
/// For example, if a syntax includes another syntax like this:
/// ```yaml
///   - include: scope:source.c++#unique-variables
/// ```
/// we only want to track the dependency on `source.c++`.
fn remove_explicit_context(scope: Scope) -> Scope {
    if let Some(without_context) = scope.build_string().split('#').next() {
        Scope::new(without_context).expect("removing context reference must never fail")
    } else {
        scope
    }
}

fn dependency_from_context_reference(context_reference: &ContextReference) -> Option<OtherSyntax> {
    match &context_reference {
        ContextReference::File { ref name, .. } => Some(OtherSyntax::ByName(name.clone())),
        ContextReference::ByScope { ref scope, .. } => {
            Some(OtherSyntax::ByScope(remove_explicit_context(*scope)))
        }
        _ => None,
    }
}

/// Helper to construct a [SyntaxSetBuilder] that contains only [SyntaxDefinition]s
/// that have dependencies among them.
struct SyntaxSetDependencyBuilder {
    syntax_set_builder: SyntaxSetBuilder,
}

impl SyntaxSetDependencyBuilder {
    fn new() -> Self {
        SyntaxSetDependencyBuilder {
            syntax_set_builder: SyntaxSetBuilder::new(),
        }
    }

    /// Add a [SyntaxDefinition] to the underlying [SyntaxSetBuilder].
    /// Also resolve any dependencies it has and add those [SyntaxDefinition]s too.
    /// This is a recursive process.
    fn add_with_dependencies(
        &mut self,
        syntax: &SyntaxDefinition,
        other_syntax_lookup: &OtherSyntaxLookup,
        syntax_to_dependencies: &SyntaxToDependencies,
        syntax_to_dependents: &SyntaxToDependents,
    ) {
        let name = &syntax.name;
        if self.is_syntax_already_added(name) {
            return;
        }

        self.syntax_set_builder.add(syntax.clone());

        let mut syntaxes_to_add = vec![];
        if let Some(dependencies) = syntax_to_dependencies.get(name) {
            syntaxes_to_add.extend(dependencies);
        }
        if let Some(dependents) = syntax_to_dependents.get(name) {
            // This will later be enabled intelligently
            if std::env::var("BAT_INCLUDE_SYNTAX_DEPENDENTS").is_ok() {
                syntaxes_to_add.extend(dependents);
            }
        }
        for syntax_to_add in syntaxes_to_add {
            if let Some(syntax_to_add) = other_syntax_lookup.get(syntax_to_add) {
                self.add_with_dependencies(
                    syntax_to_add,
                    other_syntax_lookup,
                    syntax_to_dependencies,
                    syntax_to_dependents,
                )
            }
        }
    }

    fn is_syntax_already_added(&self, name: &str) -> bool {
        self.syntax_set_builder
            .syntaxes()
            .iter()
            .any(|syntax| syntax.name == name)
    }

    fn build(self) -> SyntaxSet {
        self.syntax_set_builder.build()
    }
}

fn asset_to_contents<T: serde::Serialize>(
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
    .map_err(|_| format!("Could not serialize {}", description))?;
    Ok(contents)
}

fn asset_to_cache<T: serde::Serialize>(
    asset: &T,
    path: &Path,
    description: &str,
    compressed: bool,
) -> Result<()> {
    print!("Writing {} to {} ... ", description, path.to_string_lossy());
    let contents = asset_to_contents(asset, description, compressed)?;
    std::fs::write(path, &contents[..]).map_err(|_| {
        format!(
            "Could not save {} to {}",
            description,
            path.to_string_lossy()
        )
    })?;
    println!("okay");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn remove_explicit_context_sanity() {
        // Example from Objective-C++.sublime-syntax
        let scope = Scope::new("source.c++#unique-variables").unwrap();
        let expected = Scope::new("source.c++").unwrap();
        assert_eq!(remove_explicit_context(scope), expected);
    }
}
