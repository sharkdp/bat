// TODO: Re-enable generation of shell completion files (below) when clap 3 is out.
// For more details, see https://github.com/sharkdp/bat/issues/372

#[macro_use]
extern crate lazy_static;
extern crate liquid;

use std::error::Error;
use std::fs;

// Read environment variables.
lazy_static! {
    pub static ref PROJECT_NAME: &'static str = option_env!("PROJECT_NAME").unwrap_or("bat");
    pub static ref PROJECT_VERSION: &'static str = option_env!("CARGO_PKG_VERSION").unwrap();
    pub static ref EXECUTABLE_NAME: &'static str = option_env!("PROJECT_EXECUTABLE")
        .or(option_env!("PROJECT_NAME"))
        .unwrap_or("bat");
}

fn init_template() -> liquid::value::Object {
    let mut globals = liquid::value::Object::new();

    globals.insert(
        "PROJECT_NAME".into(),
        liquid::value::Value::scalar(PROJECT_NAME.to_owned()),
    );

    globals.insert(
        "PROJECT_EXECUTABLE".into(),
        liquid::value::Value::scalar(EXECUTABLE_NAME.to_owned()),
    );

    globals.insert(
        "PROJECT_VERSION".into(),
        liquid::value::Value::scalar(PROJECT_VERSION.to_owned()),
    );

    globals
}

/// Generates a file from a liquid template.
fn template(
    variables: &liquid::value::Object,
    in_file: &str,
    out_file: &str,
) -> Result<(), Box<dyn Error>> {
    let template = liquid::ParserBuilder::with_liquid()
        .build()?
        .parse(&fs::read_to_string(in_file)?)?;

    fs::write(out_file, template.render(variables)?)?;
    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    let variables = init_template();

    template(&variables, "assets/manual/bat.1.in", "assets/manual/bat.1")?;
    template(
        &variables,
        "assets/completions/bat.fish.in",
        "assets/completions/bat.fish",
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
