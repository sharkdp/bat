#[macro_export]
macro_rules! bat_warning {
    ($($arg:tt)*) => ({
        let style = anstyle::AnsiColor::Yellow.on_default();
        eprintln!("{}[bat warning]{}: {}", style.render(), style.render_reset(), format!($($arg)*));
    })
}
