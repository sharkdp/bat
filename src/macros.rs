#[macro_export]
macro_rules! bat_warning {
    ($($arg:tt)*) => ({
        use ansi_term::Colour::Yellow;
        eprintln!("{}: {}", Yellow.paint("[bat warning]"), format!($($arg)*));
    })
}
