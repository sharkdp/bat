#[macro_export]
macro_rules! bat_warning {
    ($($arg:tt)*) => ({
        eprintln!(
            "{}: {}",
            $crate::error::paint_prefix(nu_ansi_term::Color::Yellow, "[bat warning]"),
            format!($($arg)*)
        );
    })
}
