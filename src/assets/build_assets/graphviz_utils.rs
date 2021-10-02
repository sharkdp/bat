use super::*;

pub(crate) fn try_syntax_dependencies_to_graphviz_dot_file(
    other_syntax_lookup: &OtherSyntaxLookup,
    syntax_to_dependencies: &SyntaxToDependencies,
    dot_file_path: &str,
) {
    match syntax_dependencies_to_graphviz_dot_file(
        other_syntax_lookup,
        syntax_to_dependencies,
        dot_file_path,
    ) {
        Ok(_) => println!("Wrote graphviz dot file to {}", dot_file_path),
        Err(e) => eprintln!(
            "Failed to write graphviz dot file to {}: {}",
            dot_file_path, e
        ),
    };
}

fn syntax_dependencies_to_graphviz_dot_file(
    other_syntax_lookup: &OtherSyntaxLookup,
    syntax_to_dependencies: &SyntaxToDependencies,
    dot_file_path: &str,
) -> Result<()> {
    use std::io::Write;

    let mut dot_file = std::fs::File::create(dot_file_path)?;

    writeln!(dot_file, "digraph BatSyntaxDependencies {{")?;
    for (key, dependencies) in syntax_to_dependencies {
        for dependency in dependencies {
            if let Some(dep) = other_syntax_lookup.get(dependency) {
                writeln!(dot_file, "    \"{}\" -> \"{}\"", key, dep.name)?;
            }
        }
    }
    writeln!(dot_file, "}}")?;

    Ok(())
}
