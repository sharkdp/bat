#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PagingMode {
    Always,
    QuitIfOneScreen,
    Never,
}

impl Default for PagingMode {
    fn default() -> Self {
        PagingMode::Never
    }
}
