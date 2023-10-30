#[cfg(feature = "application")]
mod application;
mod util;

fn main() -> anyhow::Result<()> {
    #[cfg(feature = "application")]
    application::gen_man_and_comp()?;

    Ok(())
}
