use super::super::NodeShape;

/// Four corner glyphs used to distinguish otherwise rectangular node shapes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CornerChars {
    /// Glyph at the upper-left corner.
    pub top_left: char,
    /// Glyph at the upper-right corner.
    pub top_right: char,
    /// Glyph at the lower-left corner.
    pub bottom_left: char,
    /// Glyph at the lower-right corner.
    pub bottom_right: char,
}

impl CornerChars {
    const fn new(top_left: char, top_right: char, bottom_left: char, bottom_right: char) -> Self {
        Self {
            top_left,
            top_right,
            bottom_left,
            bottom_right,
        }
    }
}

/// Return the ASCII or Unicode corner glyphs for a node shape.
pub const fn corners(shape: NodeShape, use_ascii: bool) -> CornerChars {
    match (shape, use_ascii) {
        (NodeShape::Rectangle, false) => CornerChars::new('┌', '┐', '└', '┘'),
        (NodeShape::Rectangle, true) => CornerChars::new('+', '+', '+', '+'),
        (NodeShape::Rounded, false) => CornerChars::new('╭', '╮', '╰', '╯'),
        (NodeShape::Rounded, true) => CornerChars::new('.', '.', '\'', '\''),
        (NodeShape::Circle, false) => CornerChars::new('◯', '◯', '◯', '◯'),
        (NodeShape::Circle, true) => CornerChars::new('o', 'o', 'o', 'o'),
        (NodeShape::DoubleCircle, false) => CornerChars::new('◎', '◎', '◎', '◎'),
        (NodeShape::DoubleCircle, true) => CornerChars::new('@', '@', '@', '@'),
        (NodeShape::Diamond, false) => CornerChars::new('◇', '◇', '◇', '◇'),
        (NodeShape::Diamond, true) => CornerChars::new('<', '>', '<', '>'),
        (NodeShape::Hexagon, false) => CornerChars::new('⌜', '⌝', '⌞', '⌟'),
        (NodeShape::Hexagon, true) => CornerChars::new('*', '*', '*', '*'),
        (NodeShape::Stadium, _) => CornerChars::new('(', ')', '(', ')'),
        (NodeShape::Subroutine, false) => CornerChars::new('╟', '╢', '╟', '╢'),
        (NodeShape::Subroutine, true) => CornerChars::new('|', '|', '|', '|'),
        (NodeShape::Cylinder, false) => CornerChars::new('╭', '╮', '╰', '╯'),
        (NodeShape::Cylinder, true) => CornerChars::new('.', '.', '\'', '\''),
        (NodeShape::Asymmetric, false) => CornerChars::new('▷', '┐', '▷', '┘'),
        (NodeShape::Asymmetric, true) => CornerChars::new('>', '+', '>', '+'),
        (NodeShape::Trapezoid, false) => CornerChars::new('/', '\\', '└', '┘'),
        (NodeShape::Trapezoid, true) => CornerChars::new('/', '\\', '+', '+'),
        (NodeShape::TrapezoidAlt, false) => CornerChars::new('┌', '┐', '\\', '/'),
        (NodeShape::TrapezoidAlt, true) => CornerChars::new('+', '+', '\\', '/'),
        (NodeShape::StateStart, false) => CornerChars::new('●', '●', '●', '●'),
        (NodeShape::StateStart, true) => CornerChars::new('*', '*', '*', '*'),
        (NodeShape::StateEnd, false) => CornerChars::new('◉', '◉', '◉', '◉'),
        (NodeShape::StateEnd, true) => CornerChars::new('@', '@', '@', '@'),
    }
}
