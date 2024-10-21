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
