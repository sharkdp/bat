#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NonprintableNotation {
    Caret,
    Unicode,
}

impl Default for NonprintableNotation {
    fn default() -> Self {
        NonprintableNotation::Unicode
    }
}
