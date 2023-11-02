use std::{env, fs, path::PathBuf};

use crate::util::render_template;

/// Generate manpage and shell completions for the bat application.
pub fn gen_man_and_comp() -> anyhow::Result<()> {
    // Read environment variables.
    let project_name = env::var("PROJECT_NAME").unwrap_or("bat".into());
    let executable_name = env::var("PROJECT_EXECUTABLE").unwrap_or(project_name.clone());
    let executable_name_uppercase = executable_name.to_uppercase();
    let project_version = env::var("CARGO_PKG_VERSION")?;

    let variables = [
        ("PROJECT_NAME", project_name),
        ("PROJECT_EXECUTABLE", executable_name),
        ("PROJECT_EXECUTABLE_UPPERCASE", executable_name_uppercase),
        ("PROJECT_VERSION", project_version),
    ]
    .into_iter()
    .collect();

    let Some(out_dir) = env::var_os("BAT_ASSETS_GEN_DIR")
        .or_else(|| env::var_os("OUT_DIR"))
        .map(PathBuf::from)
    else {
        anyhow::bail!("BAT_ASSETS_GEN_DIR or OUT_DIR should be set for build.rs");
    };

    fs::create_dir_all(out_dir.join("assets/manual")).unwrap();
    fs::create_dir_all(out_dir.join("assets/completions")).unwrap();

    render_template(
        &variables,
        "assets/manual/bat.1.in",
        out_dir.join("assets/manual/bat.1"),
    )?;
    render_template(
        &variables,
        "assets/completions/bat.bash.in",
        out_dir.join("assets/completions/bat.bash"),
    )?;
    render_template(
        &variables,
        "assets/completions/bat.fish.in",
        out_dir.join("assets/completions/bat.fish"),
    )?;
    render_template(
        &variables,
        "assets/completions/_bat.ps1.in",
        out_dir.join("assets/completions/_bat.ps1"),
    )?;
    render_template(
        &variables,
        "assets/completions/bat.zsh.in",
        out_dir.join("assets/completions/bat.zsh"),
    )?;

    Ok(())
}
