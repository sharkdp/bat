/// How to print non-printable characters with
/// [crate::config::Config::show_nonprintable]
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum NonprintableNotation {
    /// Use caret notation (^G, ^J, ^@, ..)
    Caret,

    /// Use unicode notation (␇, ␊, ␀, ..)
    #[default]
    Unicode,

    /// Use pictographic symbols for common controls (⇥, ⏎, ⌫, ⎋, ...).
    Symbols,

    /// Use periods for spaces, ASCII controls, and invalid UTF-8 bytes.
    Period,

    /// Use symbols for tabs, line endings, and escapes; periods for other controls.
    Binary,
}

/// How to treat binary content
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum BinaryBehavior {
    /// Do not print any binary content
    #[default]
    NoPrinting,

    /// Treat binary content as normal text
    AsText,
}
