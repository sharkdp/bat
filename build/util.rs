#![allow(dead_code)]

use std::{collections::HashMap, fs, path::Path};

/// Generates a file from a template.
pub fn render_template(
    variables: &HashMap<&str, String>,
    in_file: &str,
    out_file: impl AsRef<Path>,
) -> anyhow::Result<()> {
    let mut content = fs::read_to_string(in_file)?;

    for (variable_name, value) in variables {
        // Replace {{variable_name}} by the value
        let pattern = format!("{{{{{variable_name}}}}}");
        content = content.replace(&pattern, value);
    }

    fs::write(out_file, content)?;
    Ok(())
}
