//! Flowchart and state-diagram pipeline: parse → layering → grid placement
//! → A* edge routing → canvas drawing.
//!
//! [`layering`] assigns every node a rank (longest path, cycles broken at DFS
//! back edges), an in-rank order (barycenter crossing minimization) and a
//! cross-axis slot (relaxed toward neighbours). Nodes then occupy 3×3 blocks
//! on a logical grid at `(slot·4, rank·4)`; the center cell holds the node
//! and the ring around it hosts edge attachment points. Column widths and row
//! heights are computed per grid line, then grid coordinates map to canvas
//! coordinates for drawing.

use std::collections::HashMap;

use super::{
    ansi::{ColorMode, Theme},
    canvas::{Canvas, DrawingCoord, RoleCanvas},
    ParseError,
};
pub use super::{Direction, LayoutDirection};

pub mod bundling;
pub mod converter;
pub mod draw;
pub mod grid;
pub mod layering;
pub mod parser;
pub mod pathfinder;
pub mod routing;
pub mod shapes;

/// Node shape as written in the diagram source.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub enum NodeShape {
    /// `[text]`
    #[default]
    Rectangle,
    /// `(text)`
    Rounded,
    /// `{text}`
    Diamond,
    /// `([text])`
    Stadium,
    /// `((text))`
    Circle,
    /// `[[text]]` — double-bordered rectangle.
    Subroutine,
    /// `(((text)))` — concentric circles.
    DoubleCircle,
    /// `{{text}}` — six-sided polygon.
    Hexagon,
    /// `[(text)]` — database cylinder.
    Cylinder,
    /// `>text]` — flag/banner.
    Asymmetric,
    /// `[/text\]` — wider bottom.
    Trapezoid,
    /// `[\text/]` — wider top.
    TrapezoidAlt,
    /// State-diagram start pseudostate (filled circle).
    StateStart,
    /// State-diagram end pseudostate (bullseye).
    StateEnd,
}

/// Edge line style.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum EdgeStyle {
    #[default]
    Solid,
    /// `-.->`
    Dotted,
    /// `==>`
    Thick,
}

/// Logical grid coordinate; nodes occupy 3×3 blocks.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct GridCoord {
    pub x: i32,
    pub y: i32,
}

impl GridCoord {
    pub const fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }

    /// Move into the 3×3 block by a direction offset.
    pub const fn offset(self, dir: Dir) -> Self {
        Self {
            x: self.x + dir.dx(),
            y: self.y + dir.dy(),
        }
    }
}

/// Position within a node's 3×3 grid block, also used as a travel direction.
///
/// ```text
/// UpperLeft  Up     UpperRight
/// Left       Middle Right
/// LowerLeft  Down   LowerRight
/// ```
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub enum Dir {
    Up,
    Down,
    Left,
    Right,
    UpperRight,
    /// Offset `(0, 0)`: the zero value of a direction.
    #[default]
    UpperLeft,
    LowerRight,
    LowerLeft,
    Middle,
}

impl Dir {
    /// Every direction, in the order layout code probes them.
    pub const ALL: [Self; 9] = [
        Self::Up,
        Self::Down,
        Self::Left,
        Self::Right,
        Self::UpperRight,
        Self::UpperLeft,
        Self::LowerRight,
        Self::LowerLeft,
        Self::Middle,
    ];

    /// Column offset within the 3×3 block (0..=2).
    pub const fn dx(self) -> i32 {
        match self {
            Self::Left | Self::UpperLeft | Self::LowerLeft => 0,
            Self::Up | Self::Down | Self::Middle => 1,
            Self::Right | Self::UpperRight | Self::LowerRight => 2,
        }
    }

    /// Row offset within the 3×3 block (0..=2).
    pub const fn dy(self) -> i32 {
        match self {
            Self::Up | Self::UpperLeft | Self::UpperRight => 0,
            Self::Left | Self::Right | Self::Middle => 1,
            Self::Down | Self::LowerLeft | Self::LowerRight => 2,
        }
    }

    /// Direction with the given block offsets, if any.
    pub const fn from_offset(dx: i32, dy: i32) -> Option<Self> {
        Some(match (dx, dy) {
            (1, 0) => Self::Up,
            (1, 2) => Self::Down,
            (0, 1) => Self::Left,
            (2, 1) => Self::Right,
            (2, 0) => Self::UpperRight,
            (0, 0) => Self::UpperLeft,
            (2, 2) => Self::LowerRight,
            (0, 2) => Self::LowerLeft,
            (1, 1) => Self::Middle,
            _ => return None,
        })
    }

    /// Opposite side of the block; diagonals and `Middle` map to themselves.
    pub const fn opposite(self) -> Self {
        match self {
            Self::Up => Self::Down,
            Self::Down => Self::Up,
            Self::Left => Self::Right,
            Self::Right => Self::Left,
            other => other,
        }
    }
}

/// Index of a node in [`AsciiGraph::nodes`].
pub type NodeId = usize;
/// Index of an edge in [`AsciiGraph::edges`].
pub type EdgeId = usize;
/// Index of a subgraph in [`AsciiGraph::subgraphs`].
pub type SubgraphId = usize;
/// Index of a bundle in [`AsciiGraph::bundles`].
pub type BundleId = usize;

/// A node placed on the layout grid.
#[derive(Clone, Debug)]
pub struct AsciiNode {
    /// Unique identity — the node id from the source (`A`, `B`).
    pub name: String,
    /// Label rendered inside the shape; may contain newlines.
    pub label: String,
    pub shape: NodeShape,
    /// Position in [`AsciiGraph::nodes`], also the parse order.
    pub index: NodeId,
    pub grid_coord: Option<GridCoord>,
    pub drawing_coord: Option<DrawingCoord>,
    /// Rendered shape, once drawn.
    pub drawing: Option<Canvas>,
    pub drawn: bool,
}

/// An edge with its routed grid path.
#[derive(Clone, Debug)]
pub struct AsciiEdge {
    pub from: NodeId,
    pub to: NodeId,
    /// Edge label (empty when none).
    pub text: String,
    pub path: Vec<GridCoord>,
    pub label_line: Vec<GridCoord>,
    pub start_dir: Dir,
    pub end_dir: Dir,
    pub style: EdgeStyle,
    pub has_arrow_start: bool,
    pub has_arrow_end: bool,
    /// Bundle this edge belongs to, set during bundling analysis.
    pub bundle: Option<BundleId>,
    /// For bundled edges: path between the non-shared node and the junction.
    /// The full visual path is `path_to_junction + bundle.shared_path` for
    /// fan-in, or `bundle.shared_path + path_to_junction` for fan-out.
    pub path_to_junction: Vec<GridCoord>,
}

/// A subgraph container with its canvas bounding box.
#[derive(Clone, Debug)]
pub struct AsciiSubgraph {
    pub name: String,
    pub nodes: Vec<NodeId>,
    pub parent: Option<SubgraphId>,
    pub children: Vec<SubgraphId>,
    pub min_x: i32,
    pub min_y: i32,
    pub max_x: i32,
    pub max_y: i32,
    /// Direction override for layout within this subgraph.
    pub direction: Option<LayoutDirection>,
}

/// Fan-in (`A & B --> C`) or fan-out (`A --> B & C`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BundleKind {
    FanIn,
    FanOut,
}

/// Edges sharing a source or target, visually merged before the shared node.
#[derive(Clone, Debug)]
pub struct EdgeBundle {
    pub kind: BundleKind,
    pub edges: Vec<EdgeId>,
    /// Target for fan-in, source for fan-out.
    pub shared_node: NodeId,
    /// Sources for fan-in, targets for fan-out.
    pub other_nodes: Vec<NodeId>,
    /// Where edges merge/split; set during routing.
    pub junction_point: Option<GridCoord>,
    /// Path from junction to shared node, drawn once.
    pub shared_path: Vec<GridCoord>,
    /// Direction entering/exiting the junction.
    pub junction_dir: Dir,
    /// Direction entering/exiting the shared node.
    pub shared_node_dir: Dir,
}

/// Layout configuration.
#[derive(Clone, Debug)]
pub struct AsciiConfig {
    /// `+-|` instead of box-drawing characters.
    pub use_ascii: bool,
    /// Horizontal spacing between nodes.
    pub padding_x: i32,
    /// Vertical spacing between nodes.
    pub padding_y: i32,
    /// Padding inside node boxes.
    pub box_border_padding: i32,
    pub direction: LayoutDirection,
}

/// Full layout and rendering state for one flowchart.
#[derive(Debug)]
pub struct AsciiGraph {
    pub nodes: Vec<AsciiNode>,
    pub edges: Vec<AsciiEdge>,
    pub canvas: Canvas,
    pub role_canvas: RoleCanvas,
    /// Grid occupancy: which node reserved each grid cell.
    pub grid: HashMap<GridCoord, NodeId>,
    pub column_width: HashMap<i32, i32>,
    pub row_height: HashMap<i32, i32>,
    pub subgraphs: Vec<AsciiSubgraph>,
    pub config: AsciiConfig,
    /// Offset applied to all drawing coords to make room for subgraph borders.
    pub offset_x: i32,
    pub offset_y: i32,
    pub bundles: Vec<EdgeBundle>,
}

/// Render a flowchart or state diagram. `direction` overrides the source's
/// own direction before layout, exactly as if the source had authored it.
pub fn render(
    text: &str,
    config: AsciiConfig,
    direction: Option<Direction>,
    mode: ColorMode,
    theme: &Theme,
) -> Result<String, ParseError> {
    let mut parsed = parser::parse_flowchart(text)?;
    if let Some(direction) = direction {
        parsed.direction = direction;
    }
    let config = AsciiConfig {
        direction: parsed.direction.layout(),
        ..config
    };
    let mut graph = converter::convert(&parsed, config);
    grid::create_mapping(&mut graph);
    draw::draw_graph(&mut graph);
    // BT lays out as TD; flipping the finished canvas runs the flow upward.
    if parsed.direction == Direction::BT {
        graph.canvas.flip_vertical_remapped();
        graph.role_canvas.flip_vertical();
    }
    Ok(graph
        .canvas
        .render_with_roles(Some(&graph.role_canvas), mode, theme))
}

impl AsciiGraph {
    /// Sum of all column widths.
    pub fn total_width(&self) -> i32 {
        self.column_width.values().sum()
    }

    /// Sum of all row heights.
    pub fn total_height(&self) -> i32 {
        self.row_height.values().sum()
    }

    /// Grow both canvases to cover every grid column and row.
    pub fn size_canvases_to_grid(&mut self) {
        let (w, h) = (self.total_width(), self.total_height());
        self.canvas.ensure_size(w, h);
        self.role_canvas.ensure_size(w, h);
    }
}
