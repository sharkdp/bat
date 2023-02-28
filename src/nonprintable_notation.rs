#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum NonprintableNotation {
    Caret,

    #[default]
    Unicode,
}
