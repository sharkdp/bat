#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub enum OutputWrap {
    Character,
    None,
}

impl Default for OutputWrap {
    fn default() -> Self {
        OutputWrap::None
    }
}
