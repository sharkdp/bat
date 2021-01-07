#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WrappingMode {
    Character,
    NoWrapping(bool), // explicitly opted in or not
}

impl Default for WrappingMode {
    fn default() -> Self {
        WrappingMode::NoWrapping(false)
    }
}
