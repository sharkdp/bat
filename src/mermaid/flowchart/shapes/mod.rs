//! Node-shape sizing, rendering, and edge attachment points.

use crate::mermaid::{
    canvas::{Canvas, DrawingCoord},
    flowchart::{Dir, NodeShape},
};

pub mod circle;
pub mod corners;
pub mod diamond;
pub mod hexagon;
pub mod rectangle;
pub mod rounded;
pub mod special;
pub mod stadium;
pub mod state;

use circle::Circle;
use diamond::Diamond;
use hexagon::Hexagon;
use rectangle::Rectangle;
use rounded::Rounded;
use special::{Asymmetric, Cylinder, DoubleCircle, Subroutine, Trapezoid, TrapezoidAlt};
use stadium::Stadium;
use state::{StateEnd, StateStart};

/// Bounds reserved for a shape's visible label.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LabelArea {
    /// Left edge relative to the shape canvas.
    pub x: i32,
    /// Top edge relative to the shape canvas.
    pub y: i32,
    /// Widest label line in terminal columns.
    pub width: i32,
    /// Number of label lines.
    pub height: i32,
}

/// Canvas and logical-grid dimensions calculated for one shape.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ShapeDimensions {
    /// Total canvas width, including borders.
    pub width: i32,
    /// Total canvas height, including borders.
    pub height: i32,
    /// Region occupied by the label.
    pub label_area: LabelArea,
    /// Widths of the left, center, and right logical-grid columns.
    pub grid_columns: [i32; 3],
    /// Heights of the top, middle, and bottom logical-grid rows.
    pub grid_rows: [i32; 3],
}

/// Display options used for shape sizing and rendering.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ShapeRenderOptions {
    /// Select plain ASCII rather than Unicode box-drawing glyphs.
    pub use_ascii: bool,
    /// Blank columns and rows placed around the label.
    pub padding: i32,
}

/// Pluggable sizing, drawing, and edge-attachment behavior for a node shape.
pub trait ShapeRenderer {
    /// Calculate the shape dimensions needed for `label`.
    fn dimensions(&self, label: &str, opts: &ShapeRenderOptions) -> ShapeDimensions;
    /// Draw `label` into a canvas with the precomputed dimensions.
    fn render(&self, label: &str, dims: &ShapeDimensions, opts: &ShapeRenderOptions) -> Canvas;
    /// Locate the shape boundary in `dir` relative to `base`.
    fn attachment_point(
        &self,
        dir: Dir,
        dims: &ShapeDimensions,
        base: DrawingCoord,
    ) -> DrawingCoord;
}

static RECTANGLE: Rectangle = Rectangle;
static ROUNDED: Rounded = Rounded;
static DIAMOND: Diamond = Diamond;
static STADIUM: Stadium = Stadium;
static CIRCLE: Circle = Circle;
static SUBROUTINE: Subroutine = Subroutine;
static DOUBLE_CIRCLE: DoubleCircle = DoubleCircle;
static HEXAGON: Hexagon = Hexagon;
static CYLINDER: Cylinder = Cylinder;
static ASYMMETRIC: Asymmetric = Asymmetric;
static TRAPEZOID: Trapezoid = Trapezoid;
static TRAPEZOID_ALT: TrapezoidAlt = TrapezoidAlt;
static STATE_START: StateStart = StateStart;
static STATE_END: StateEnd = StateEnd;

/// Return the renderer registered for a node shape.
pub fn renderer(shape: NodeShape) -> &'static dyn ShapeRenderer {
    match shape {
        NodeShape::Rectangle => &RECTANGLE,
        NodeShape::Rounded => &ROUNDED,
        NodeShape::Diamond => &DIAMOND,
        NodeShape::Stadium => &STADIUM,
        NodeShape::Circle => &CIRCLE,
        NodeShape::Subroutine => &SUBROUTINE,
        NodeShape::DoubleCircle => &DOUBLE_CIRCLE,
        NodeShape::Hexagon => &HEXAGON,
        NodeShape::Cylinder => &CYLINDER,
        NodeShape::Asymmetric => &ASYMMETRIC,
        NodeShape::Trapezoid => &TRAPEZOID,
        NodeShape::TrapezoidAlt => &TRAPEZOID_ALT,
        NodeShape::StateStart => &STATE_START,
        NodeShape::StateEnd => &STATE_END,
    }
}

/// Calculate dimensions for a shape and label.
pub fn shape_dimensions(
    shape: NodeShape,
    label: &str,
    opts: &ShapeRenderOptions,
) -> ShapeDimensions {
    renderer(shape).dimensions(label, opts)
}

/// Render a shape using dimensions calculated for the same label and options.
pub fn render_shape(
    shape: NodeShape,
    label: &str,
    dims: &ShapeDimensions,
    opts: &ShapeRenderOptions,
) -> Canvas {
    renderer(shape).render(label, dims, opts)
}

/// Calculate an edge attachment point on a shape boundary.
pub fn shape_attachment_point(
    shape: NodeShape,
    dir: Dir,
    dims: &ShapeDimensions,
    base: DrawingCoord,
) -> DrawingCoord {
    renderer(shape).attachment_point(dir, dims, base)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn rendered(shape: NodeShape, label: &str, use_ascii: bool) -> String {
        let opts = ShapeRenderOptions {
            use_ascii,
            padding: 1,
        };
        let dims = shape_dimensions(shape, label, &opts);
        render_shape(shape, label, &dims, &opts).to_plain_string()
    }

    #[test]
    fn renders_every_shape_in_unicode_and_ascii() {
        let cases = [
            (
                NodeShape::Rectangle,
                "┌───┐\n│   │\n│ X │\n│   │\n└───┘",
                "+---+\n|   |\n| X |\n|   |\n+---+",
            ),
            (
                NodeShape::Rounded,
                "╭───╮\n│   │\n│ X │\n│   │\n╰───╯",
                ".---.\n|   |\n| X |\n|   |\n'---'",
            ),
            (
                NodeShape::Diamond,
                "◇───◇\n│   │\n│ X │\n│   │\n◇───◇",
                "<--->\n|   |\n| X |\n|   |\n<--->",
            ),
            (
                NodeShape::Stadium,
                "╭─────╮\n│     │\n│  X  │\n│     │\n╰─────╯",
                "(-----)\n(     )\n(  X  )\n(     )\n(-----)",
            ),
            (
                NodeShape::Circle,
                "◯───◯\n│   │\n│ X │\n│   │\n◯───◯",
                "o---o\n|   |\n| X |\n|   |\no---o",
            ),
            (
                NodeShape::Subroutine,
                "┌┬───┬┐\n││   ││\n││ X ││\n││   ││\n└┴───┴┘",
                "++---++\n||   ||\n|| X ||\n||   ||\n++---++",
            ),
            (
                NodeShape::DoubleCircle,
                "◎───◎\n│   │\n│ X │\n│   │\n◎───◎",
                "@---@\n|   |\n| X |\n|   |\n@---@",
            ),
            (
                NodeShape::Hexagon,
                "⌜───⌝\n│   │\n│ X │\n│   │\n⌞───⌟",
                "*---*\n|   |\n| X |\n|   |\n*---*",
            ),
            (
                NodeShape::Cylinder,
                "╭───╮\n│───│\n│   │\n│ X │\n│   │\n│───│\n╰───╯",
                ".---.\n|---|\n|   |\n| X |\n|   |\n|---|\n'---'",
            ),
            (
                NodeShape::Asymmetric,
                "▷───┐\n│   │\n│ X │\n│   │\n▷───┘",
                ">---+\n|   |\n| X |\n|   |\n>---+",
            ),
            (
                NodeShape::Trapezoid,
                "/───\\\n│   │\n│ X │\n│   │\n└───┘",
                "/---\\\n|   |\n| X |\n|   |\n+---+",
            ),
            (
                NodeShape::TrapezoidAlt,
                "┌───┐\n│   │\n│ X │\n│   │\n\\───/",
                "+---+\n|   |\n| X |\n|   |\n\\---/",
            ),
            (
                NodeShape::StateStart,
                "╭───╮\n│ ● │\n╰───╯",
                ".---.\n| * |\n'---'",
            ),
            (
                NodeShape::StateEnd,
                "╔═══╗\n║ ◎ ║\n╚═══╝",
                "#===#\n# * #\n#===#",
            ),
        ];

        for (shape, unicode, ascii) in cases {
            assert_eq!(rendered(shape, "X", false), unicode, "Unicode {shape:?}");
            assert_eq!(rendered(shape, "X", true), ascii, "ASCII {shape:?}");
        }
    }

    #[test]
    fn attachment_points_follow_box_and_state_rules() {
        let opts = ShapeRenderOptions {
            use_ascii: false,
            padding: 1,
        };
        let dims = shape_dimensions(NodeShape::Rectangle, "X", &opts);
        let base = DrawingCoord::new(10, 20);
        let expected = [
            (Dir::Up, DrawingCoord::new(12, 20)),
            (Dir::Down, DrawingCoord::new(12, 24)),
            (Dir::Left, DrawingCoord::new(10, 22)),
            (Dir::Right, DrawingCoord::new(14, 22)),
            (Dir::UpperLeft, DrawingCoord::new(10, 20)),
            (Dir::UpperRight, DrawingCoord::new(14, 20)),
            (Dir::LowerLeft, DrawingCoord::new(10, 24)),
            (Dir::LowerRight, DrawingCoord::new(14, 24)),
            (Dir::Middle, DrawingCoord::new(12, 22)),
        ];
        for (dir, point) in expected {
            assert_eq!(
                shape_attachment_point(NodeShape::Rectangle, dir, &dims, base),
                point,
            );
        }

        let state_dims = shape_dimensions(NodeShape::StateStart, "", &opts);
        assert_eq!(
            shape_attachment_point(NodeShape::StateStart, Dir::UpperLeft, &state_dims, base),
            DrawingCoord::new(12, 21),
        );
    }

    #[test]
    fn centers_multiline_labels_with_reference_flooring() {
        let unicode = "┌────┐\n│    │\n│    │\n│ A  │\n│ BB │\n│    │\n└────┘";
        let ascii = "+----+\n|    |\n|    |\n| A  |\n| BB |\n|    |\n+----+";
        assert_eq!(rendered(NodeShape::Rectangle, "A\nBB", false), unicode);
        assert_eq!(rendered(NodeShape::Rectangle, "A\nBB", true), ascii);
    }

    #[test]
    fn centers_wide_cjk_and_emoji_by_display_columns() {
        let unicode = "┌──────┐\n│      │\n│ 界🙂 │\n│      │\n└──────┘";
        let ascii = "+------+\n|      |\n| 界🙂 |\n|      |\n+------+";
        assert_eq!(rendered(NodeShape::Rectangle, "界🙂", false), unicode);
        assert_eq!(rendered(NodeShape::Rectangle, "界🙂", true), ascii);
    }
}
