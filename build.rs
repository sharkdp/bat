// TODO: Re-enable generation of shell completion files (below) when clap 3 is out.
// For more details, see https://github.com/sharkdp/bat/issues/372

// For bat-as-a-library, no build script is required. The build script is for
// the manpage and completions, which are only relevant to the bat application.
#[cfg(not(feature = "application"))]
fn main() {}

#[cfg(feature = "application")]
fn main() -> Result<(), Box<dyn std::error::Error>> {
    use std::error::Error;
    use std::fs;
    use std::path::Path;

    use lazy_static::lazy_static;

    static PROJECT_VERSION: &str = env!("CARGO_PKG_VERSION");

    // Read environment variables.
    lazy_static! {
        static ref PROJECT_NAME: &'static str = option_env!("PROJECT_NAME").unwrap_or("bat");
        static ref EXECUTABLE_NAME: &'static str =
            option_env!("PROJECT_EXECUTABLE").unwrap_or(*PROJECT_NAME);
    }

    /// Generates a file from a liquid template.
    fn template(
        variables: &liquid::Object,
        in_file: &str,
        out_file: impl AsRef<Path>,
    ) -> Result<(), Box<dyn Error>> {
        let template = liquid::ParserBuilder::with_stdlib()
            .build()?
            .parse(&fs::read_to_string(in_file)?)?;

        fs::write(out_file, template.render(variables)?)?;
        Ok(())
    }

    let variables = liquid::object!({
        "PROJECT_NAME": PROJECT_NAME.to_owned(),
        "PROJECT_EXECUTABLE": EXECUTABLE_NAME.to_owned(),
        "PROJECT_VERSION": PROJECT_VERSION.to_owned(),
    });

    let out_dir_env = std::env::var_os("OUT_DIR").expect("OUT_DIR to be set in build.rs");
    let out_dir = Path::new(&out_dir_env);

    fs::create_dir_all(out_dir.join("assets/manual")).unwrap();
    fs::create_dir_all(out_dir.join("assets/completions")).unwrap();

    template(
        &variables,
        "assets/manual/bat.1.in",
        out_dir.join("assets/manual/bat.1"),
    )?;
    template(
        &variables,
        "assets/completions/bat.fish.in",
        out_dir.join("assets/completions/bat.fish"),
    )?;

    Ok(())
}

// #[macro_use]
// extern crate clap;

// use clap::Shell;
// use std::fs;

// include!("src/clap_app.rs");

// const BIN_NAME: &str = "bat";

// fn main() {
//     let outdir = std::env::var_os("SHELL_COMPLETIONS_DIR").or(std::env::var_os("OUT_DIR"));

//     let outdir = match outdir {
//         None => return,
//         Some(outdir) => outdir,
//     };

//     fs::create_dir_all(&outdir).unwrap();

//     let mut app = build_app(true);
//     app.gen_completions(BIN_NAME, Shell::Bash, &outdir);
//     app.gen_completions(BIN_NAME, Shell::Fish, &outdir);
//     app.gen_completions(BIN_NAME, Shell::Zsh, &outdir);
//     app.gen_completions(BIN_NAME, Shell::PowerShell, &outdir);
// }
