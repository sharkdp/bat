use std::collections::HashMap;
use std::path::Path;
use syntect::dumps::from_binary;
use syntect::highlighting::ThemeSet;
use syntect::parsing::syntax_definition::{
    ContextReference, MatchOperation, MatchPattern, Pattern, SyntaxDefinition,
};
use syntect::parsing::{Scope, SyntaxSet, SyntaxSetBuilder};

use crate::error::*;

type SyntaxName = String;

/// Used to look up what dependencies a given [SyntaxDefinition] has
type SyntaxToDependencies = HashMap<SyntaxName, Vec<Dependency>>;

/// Used to look up which [SyntaxDefinition] corresponds to a given [Dependency]
type DependencyToSyntax<'a> = HashMap<Dependency, &'a SyntaxDefinition>;

/// Represents a dependency on an external `.sublime-syntax` file.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
enum Dependency {
    /// By name. Example YAML: `include: C.sublime-syntax`
    ByName(String),

    /// By scope. Example YAML: `embed: scope:source.c`
    ByScope(Scope),
}

pub fn build_assets(
    source_dir: &Path,
    include_integrated_assets: bool,
    target_dir: &Path,
    current_version: &str,
) -> Result<()> {
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

    let mut syntax_set_builder = if !include_integrated_assets {
        let mut builder = syntect::parsing::SyntaxSetBuilder::new();
        builder.add_plain_text_syntax();
        builder
    } else {
        from_binary::<SyntaxSet>(crate::assets::get_serialized_integrated_syntaxset())
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

    if std::env::var("BAT_PRINT_SYNTAX_DEPENDENCIES").is_ok() {
        // To trigger this code, run:
        // BAT_PRINT_SYNTAX_DEPENDENCIES=1 cargo run -- cache --build --source assets --blank --target /tmp
        print_syntax_dependencies(&syntax_set_builder);
    }

    let syntax_set = syntax_set_builder.build();
    let missing_contexts = syntax_set.find_unlinked_contexts();
    if !missing_contexts.is_empty() {
        println!("Some referenced contexts could not be found!");
        for context in missing_contexts {
            println!("- {}", context);
        }
    }

    let _ = std::fs::create_dir_all(target_dir);
    asset_to_cache(&theme_set, &target_dir.join("themes.bin"), "theme set")?;
    asset_to_cache(&syntax_set, &target_dir.join("syntaxes.bin"), "syntax set")?;

    print!(
        "Writing metadata to folder {} ... ",
        target_dir.to_string_lossy()
    );
    crate::assets_metadata::AssetsMetadata::new(current_version).save_to_folder(target_dir)?;
    println!("okay");

    Ok(())
}

/// Generates independent [SyntaxSet]s after analyzing dependencies between syntaxes
/// in a [SyntaxSetBuilder], and then prints the reults.
fn print_syntax_dependencies(syntax_set_builder: &SyntaxSetBuilder) {
    println!("Constructing independent SyntaxSets...");
    let independent_syntax_sets = build_independent_syntax_sets(syntax_set_builder);

    println!("Independent SyntaxSets:");
    for syntax_set in independent_syntax_sets {
        let names = syntax_set
            .syntaxes()
            .iter()
            .map(|syntax| &syntax.name)
            .collect::<Vec<_>>();
        println!("{:?}", names);
    }
}

/// Analyzes dependencies between syntaxes in a [SyntaxSetBuilder].
/// From that, it builds independent [SyntaxSet]s.
fn build_independent_syntax_sets(
    syntax_set_builder: &'_ SyntaxSetBuilder,
) -> impl Iterator<Item = SyntaxSet> + '_ {
    let syntaxes = syntax_set_builder.syntaxes();

    // Build the data structures we need for dependency resolution
    let (syntax_to_dependencies, dependency_to_syntax) = generate_maps(syntaxes);

    // Create one independent SyntaxSet from each (non-hidden) SyntaxDefinition
    syntaxes.iter().filter_map(move |syntax| {
        if syntax.hidden {
            return None;
        }

        let mut builder = SyntaxSetDependencyBuilder::new();
        builder.add_with_dependencies(syntax, &syntax_to_dependencies, &dependency_to_syntax);
        Some(builder.build())
    })
}

/// In order to analyze dependencies, we need two key pieces of data.
/// First, when we have a [Dependency], we need to know what [SyntaxDefinition] that
/// corresponds to. Second, when we have a [SyntaxDefinition], we need to know
/// what dependencies it has. This functions generates that data for each syntax.
fn generate_maps(syntaxes: &[SyntaxDefinition]) -> (SyntaxToDependencies, DependencyToSyntax) {
    let mut syntax_to_dependencies = HashMap::new();
    let mut dependency_to_syntax = HashMap::new();

    for syntax in syntaxes {
        syntax_to_dependencies.insert(syntax.name.clone(), dependencies_for_syntax(syntax));

        dependency_to_syntax.insert(Dependency::ByName(syntax.name.clone()), syntax);
        dependency_to_syntax.insert(Dependency::ByScope(syntax.scope), syntax);
    }

    (syntax_to_dependencies, dependency_to_syntax)
}

/// Gets what external dependencies a given [SyntaxDefinition] has.
/// An external dependency is another `.sublime-syntax` file.
/// It does that by looking for variants of the following YAML patterns:
/// - `include: C.sublime-syntax`
/// - `embed: scope:source.c`
fn dependencies_for_syntax(syntax: &SyntaxDefinition) -> Vec<Dependency> {
    let mut dependencies: Vec<Dependency> = syntax
        .contexts
        .values()
        .flat_map(|context| &context.patterns)
        .flat_map(dependencies_from_pattern)
        .collect();

    // No need to track a dependency more than once
    dependencies.dedup();

    dependencies
}

fn dependencies_from_pattern(pattern: &Pattern) -> Vec<Dependency> {
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

fn dependency_from_context_reference(context_reference: &ContextReference) -> Option<Dependency> {
    match &context_reference {
        ContextReference::File { ref name, .. } => Some(Dependency::ByName(name.clone())),
        ContextReference::ByScope { ref scope, .. } => Some(Dependency::ByScope(*scope)),
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
        syntax_to_dependencies: &SyntaxToDependencies,
        dependency_to_syntax: &DependencyToSyntax,
    ) {
        let name = &syntax.name;
        if self.is_syntax_already_added(name) {
            return;
        }

        self.syntax_set_builder.add(syntax.clone());

        let dependencies = syntax_to_dependencies.get(name);
        if dependencies.is_none() {
            eprintln!("ERROR: Unknown dependencies for {}", name);
            return;
        }

        for dependency in dependencies.unwrap() {
            if let Some(syntax_definition_dependency) = dependency_to_syntax.get(dependency) {
                self.add_with_dependencies(
                    syntax_definition_dependency,
                    syntax_to_dependencies,
                    dependency_to_syntax,
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

fn asset_to_cache<T: serde::Serialize>(asset: &T, path: &Path, description: &str) -> Result<()> {
    print!("Writing {} to {} ... ", description, path.to_string_lossy());
    syntect::dumps::dump_to_file(asset, &path).map_err(|_| {
        format!(
            "Could not save {} to {}",
            description,
            path.to_string_lossy()
        )
    })?;
    println!("okay");
    Ok(())
}
