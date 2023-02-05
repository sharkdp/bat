#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NonprintingNotation {
    Caret,
    Unicode,
}

impl Default for NonprintingNotation {
    fn default() -> Self {
        NonprintingNotation::Unicode
    }
}
