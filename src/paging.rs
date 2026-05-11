#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum PagingMode {
    Always,
    QuitIfOneScreen,
    #[default]
    Never,
}
