/// How to print non-printable characters with
/// [crate::config::Config::show_nonprintable]
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum NonprintableNotation {
    /// Use caret notation (^G, ^J, ^@, ..)
    Caret,

    /// Use unicode notation (␇, ␊, ␀, ..)
    #[default]
    Unicode,
}
