#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum PagingMode {
    Always,
    QuitIfOneScreen,
    #[default]
    Never,
}
