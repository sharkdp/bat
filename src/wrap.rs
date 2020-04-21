#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WrappingMode {
    Character,
    NoWrapping,
}

impl Default for WrappingMode {
    fn default() -> Self {
        WrappingMode::NoWrapping
    }
}
