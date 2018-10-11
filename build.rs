#[macro_use]
extern crate clap;

use clap::Shell;
use std::fs;

include!("src/clap_app.rs");

const BIN_NAME: &str = "bat";

fn main() {
    let outdir = std::env::var_os("SHELL_COMPLETIONS_DIR").or(std::env::var_os("OUT_DIR"));

    let outdir = match outdir {
        None => return,
        Some(outdir) => outdir,
    };

    fs::create_dir_all(&outdir).unwrap();

    let mut app = build_app(true);
    app.gen_completions(BIN_NAME, Shell::Bash, &outdir);
    app.gen_completions(BIN_NAME, Shell::Fish, &outdir);
    app.gen_completions(BIN_NAME, Shell::Zsh, &outdir);
    app.gen_completions(BIN_NAME, Shell::PowerShell, &outdir);
}
