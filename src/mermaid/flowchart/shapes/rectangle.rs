use super::{corners::corners, LabelArea, ShapeDimensions, ShapeRenderOptions, ShapeRenderer};
use crate::mermaid::{
    canvas::{to_cells, Canvas, Cell, DrawingCoord},
    flowchart::{Dir, NodeShape},
    text::{display_width, split_lines},
};

/// Calculate the dimensions shared by rectangular and corner-decorated shapes.
pub fn box_dimensions(label: &str, opts: &ShapeRenderOptions) -> ShapeDimensions {
    let lines: Vec<&str> = split_lines(label).collect();
    let max_line_width = lines
        .iter()
        .map(|line| display_width(line) as i32)
        .max()
        .unwrap_or(0);
    let line_count = lines.len() as i32;
    let inner_width = 2 * opts.padding + max_line_width;
    let width = inner_width + 2;
    let raw_inner_height = line_count + 2 * opts.padding;
    let inner_height = if raw_inner_height % 2 == 0 {
        raw_inner_height + 1
    } else {
        raw_inner_height
    };

    ShapeDimensions {
        width,
        height: inner_height + 2,
        label_area: LabelArea {
            x: 1 + opts.padding,
            y: 1 + opts.padding,
            width: max_line_width,
            height: line_count,
        },
        grid_columns: [1, inner_width, 1],
        grid_rows: [1, inner_height, 1],
    }
}

/// Render the common rectangular frame and centered label.
pub fn render_box(
    label: &str,
    dims: &ShapeDimensions,
    corner_chars: super::corners::CornerChars,
    use_ascii: bool,
) -> Canvas {
    let mut canvas = Canvas::new(dims.width, dims.height);
    let right = dims.width - 1;
    let bottom = dims.height - 1;
    let horizontal = if use_ascii { '-' } else { '─' };
    let vertical = if use_ascii { '|' } else { '│' };

    for x in 1..right {
        canvas.set(x, 0, Cell::from(horizontal));
        canvas.set(x, bottom, Cell::from(horizontal));
    }
    for y in 1..bottom {
        canvas.set(0, y, Cell::from(vertical));
        canvas.set(right, y, Cell::from(vertical));
    }
    canvas.set(0, 0, Cell::from(corner_chars.top_left));
    canvas.set(right, 0, Cell::from(corner_chars.top_right));
    canvas.set(0, bottom, Cell::from(corner_chars.bottom_left));
    canvas.set(right, bottom, Cell::from(corner_chars.bottom_right));

    let lines: Vec<&str> = split_lines(label).collect();
    let w = dims.width - 1;
    let center_y = (dims.height - 1) / 2;
    let start_y = center_y - (lines.len() as i32 - 1) / 2;
    for (i, line) in lines.into_iter().enumerate() {
        let cells = to_cells(line);
        let text_x = w / 2 - (cells.len() as i32 + 1) / 2 + 1;
        for (j, cell) in cells.into_iter().enumerate() {
            let x = text_x + j as i32;
            let y = start_y + i as i32;
            if x >= 0 && x < dims.width && y >= 0 && y < dims.height {
                canvas.set(x, y, cell);
            }
        }
    }
    canvas
}

/// Calculate an edge attachment point on a rectangular shape.
pub const fn box_attachment_point(
    dir: Dir,
    dims: &ShapeDimensions,
    base: DrawingCoord,
) -> DrawingCoord {
    let center_x = base.x + dims.width / 2;
    let center_y = base.y + dims.height / 2;
    match dir {
        Dir::Up => DrawingCoord::new(center_x, base.y),
        Dir::Down => DrawingCoord::new(center_x, base.y + dims.height - 1),
        Dir::Left => DrawingCoord::new(base.x, center_y),
        Dir::Right => DrawingCoord::new(base.x + dims.width - 1, center_y),
        Dir::UpperLeft => DrawingCoord::new(base.x, base.y),
        Dir::UpperRight => DrawingCoord::new(base.x + dims.width - 1, base.y),
        Dir::LowerLeft => DrawingCoord::new(base.x, base.y + dims.height - 1),
        Dir::LowerRight => DrawingCoord::new(base.x + dims.width - 1, base.y + dims.height - 1),
        Dir::Middle => DrawingCoord::new(center_x, center_y),
    }
}

/// Standard rectangular node renderer.
pub struct Rectangle;

impl ShapeRenderer for Rectangle {
    fn dimensions(&self, label: &str, opts: &ShapeRenderOptions) -> ShapeDimensions {
        box_dimensions(label, opts)
    }

    fn render(&self, label: &str, dims: &ShapeDimensions, opts: &ShapeRenderOptions) -> Canvas {
        render_box(
            label,
            dims,
            corners(NodeShape::Rectangle, opts.use_ascii),
            opts.use_ascii,
        )
    }

    fn attachment_point(
        &self,
        dir: Dir,
        dims: &ShapeDimensions,
        base: DrawingCoord,
    ) -> DrawingCoord {
        box_attachment_point(dir, dims, base)
    }
}
