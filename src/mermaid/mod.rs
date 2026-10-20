//! Mermaid → ASCII/Unicode renderer.
//!
//! Ported from oh-my-pi (<https://github.com/can1357/oh-my-pi>).
//! Copyright (c) 2025 Mario Zechner
//! Copyright (c) 2025-2026 Can Bölük
//! Copyright (c) 2026 Stencil Labs, Inc.
//! Licensed under the MIT License.
//!
//! Supported diagram types:
//!
//! - Flowcharts (`graph TD` / `flowchart LR`) and state diagrams
//!   (`stateDiagram-v2`): Sugiyama-style layering onto a grid with A* edge
//!   routing.
//! - Sequence diagrams: column-based timeline layout.
//! - Class diagrams: level-based UML layout.
//! - ER diagrams: grid layout with crow's-foot notation.
//! - XY charts: bar/line plots.

pub mod ansi;
pub mod canvas;
pub mod class;
pub mod er;
pub mod flowchart;
pub mod lex;
pub mod sequence;
pub mod text;
pub mod xychart;

pub use ansi::{ColorMode, Theme};
pub use canvas::Canvas;
pub use text::display_width;

/// Graph direction as written in the source header (`graph TD`).
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum Direction {
    #[default]
    TD,
    TB,
    LR,
    BT,
    RL,
}

impl Direction {
    /// Parse a header/subgraph direction token (case-sensitive, as Mermaid).
    pub fn parse(token: &str) -> Option<Self> {
        Some(match token {
            "TD" => Self::TD,
            "TB" => Self::TB,
            "LR" => Self::LR,
            "BT" => Self::BT,
            "RL" => Self::RL,
            _ => return None,
        })
    }

    /// Grid layout direction: `LR`/`RL` lay out left-to-right, everything
    /// else top-down.
    pub const fn layout(self) -> LayoutDirection {
        match self {
            Self::LR | Self::RL => LayoutDirection::LR,
            Self::TD | Self::TB | Self::BT => LayoutDirection::TD,
        }
    }
}

/// Layout direction after normalization: `BT` lays out as `TD` and is
/// flipped afterwards; `RL` is treated as `LR`.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum LayoutDirection {
    #[default]
    TD,
    LR,
}

/// Diagram source that cannot be rendered (empty input, unknown header).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParseError(pub String);

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for ParseError {}

/// Rendering options; `Default` matches the reference renderer's defaults.
#[derive(Clone, Debug)]
pub struct RenderOptions {
    /// `+-|>` instead of Unicode box-drawing characters.
    pub use_ascii: bool,
    /// Horizontal spacing between nodes.
    pub padding_x: i32,
    /// Vertical spacing between nodes.
    pub padding_y: i32,
    /// Padding inside node boxes.
    pub box_border_padding: i32,
    /// Force the layout direction of flowcharts and state diagrams,
    /// overriding the direction in the source. Other diagram types ignore it.
    pub direction: Option<Direction>,
    /// `None` auto-detects from the terminal environment.
    pub color_mode: Option<ColorMode>,
    pub theme: Theme,
}

impl Default for RenderOptions {
    fn default() -> Self {
        Self {
            use_ascii: false,
            padding_x: 5,
            padding_y: 5,
            box_border_padding: 1,
            direction: None,
            color_mode: None,
            theme: Theme::default(),
        }
    }
}

/// Diagram type, detected from the header line.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DiagramKind {
    Flowchart,
    Sequence,
    Class,
    Er,
    XyChart,
}

/// Detect the diagram type from the first non-blank line of the source.
/// Anything unrecognized is treated as a flowchart (which also covers state
/// diagrams).
pub fn detect_diagram_kind(text: &str) -> DiagramKind {
    let first = text
        .trim()
        .lines()
        .next()
        .unwrap_or("")
        .trim()
        .to_lowercase();
    let starts_with_word = |prefix: &str| {
        first
            .strip_prefix(prefix)
            .is_some_and(|rest| rest.chars().next().is_none_or(|c| !word_char(c)))
    };
    if starts_with_word("xychart-beta") || starts_with_word("xychart") {
        DiagramKind::XyChart
    } else if first == "sequencediagram" {
        DiagramKind::Sequence
    } else if first == "classdiagram" {
        DiagramKind::Class
    } else if first == "erdiagram" {
        DiagramKind::Er
    } else {
        DiagramKind::Flowchart
    }
}

/// JavaScript `\w` as used by the header regex `\b` boundary.
const fn word_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

/// Render Mermaid source to a multi-line ASCII/Unicode string, dispatching
/// on the detected diagram type.
///
/// # Errors
/// Flowchart/state sources with an empty body or an unrecognized header.
pub fn render(text: &str, options: &RenderOptions) -> Result<String, ParseError> {
    let config = flowchart::AsciiConfig {
        use_ascii: options.use_ascii,
        padding_x: options.padding_x,
        padding_y: options.padding_y,
        box_border_padding: options.box_border_padding,
        direction: LayoutDirection::TD,
    };
    let mode = options.color_mode.unwrap_or_else(ansi::detect_color_mode);
    let theme = &options.theme;
    Ok(match detect_diagram_kind(text) {
        DiagramKind::XyChart => xychart::render(text, &config, mode, theme),
        DiagramKind::Sequence => sequence::render(text, &config, mode, theme),
        DiagramKind::Class => class::render(text, &config, mode, theme),
        DiagramKind::Er => er::render(text, &config, mode, theme),
        DiagramKind::Flowchart => flowchart::render(text, config, options.direction, mode, theme)?,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bundled_edges_stay_aligned_with_mixed_width_borders() {
        let source =
            "graph TD\nA --> B[Order Service]\nA --> C\nB --> D[[Queue]]\nB --> E\nD --> E";
        let options = RenderOptions {
            color_mode: Some(ColorMode::None),
            ..RenderOptions::default()
        };
        let art = render(source, &options).unwrap();
        assert!(
            art.contains("Order Service") && art.contains("Queue"),
            "{art}"
        );
        assert!(
            !art.contains("│▼"),
            "arrowhead must align with its incoming line:\n{art}"
        );
        assert!(
            !art.contains("┬┬"),
            "shared exits must attach at the same border column:\n{art}"
        );
    }
}
