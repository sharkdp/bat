#[macro_use]
extern crate clap;

use clap::Shell;

include!("src/clap_app.rs");

const BIN_NAME: &str = "bat";

fn main() {

    let outdir = match std::env::var_os("OUT_DIR") {
        None => return,
        Some(outdir) => outdir,
    };
    let mut app = build_app(true);
    app.gen_completions(BIN_NAME, Shell::Bash, &outdir);
    app.gen_completions(BIN_NAME, Shell::Fish, &outdir);
    app.gen_completions(BIN_NAME, Shell::Zsh, &outdir);
    app.gen_completions(BIN_NAME, Shell::PowerShell, &outdir);
}