#[cfg(feature = "application")]
mod application;
mod syntax_mapping;
mod util;

fn main() -> anyhow::Result<()> {
    // only watch manually-designated files
    // see: https://doc.rust-lang.org/cargo/reference/build-scripts.html#rerun-if-changed
    println!("cargo:rerun-if-changed=build/");

    syntax_mapping::build_static_mappings()?;

    #[cfg(feature = "application")]
    application::gen_man_and_comp()?;

    Ok(())
}
