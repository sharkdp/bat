#[macro_export]
macro_rules! bat_warning {
    ($($arg:tt)*) => ({
        use nu_ansi_term::Color::Yellow;
        eprintln!("{}: {}", Yellow.paint("[bat warning]"), format!($($arg)*));
    })
}
