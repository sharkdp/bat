use super::{
    corners::corners,
    rectangle::{box_attachment_point, box_dimensions, render_box},
    LabelArea, ShapeDimensions, ShapeRenderOptions, ShapeRenderer,
};
use crate::mermaid::{
    canvas::{to_cells, Canvas, Cell, DrawingCoord},
    flowchart::{Dir, NodeShape},
    text::{display_width, split_lines},
};

/// Double-side-bordered subroutine node renderer.
pub struct Subroutine;

impl ShapeRenderer for Subroutine {
    fn dimensions(&self, label: &str, opts: &ShapeRenderOptions) -> ShapeDimensions {
        let lines: Vec<&str> = split_lines(label).collect();
        let max_line_width = lines
            .iter()
            .map(|line| display_width(line) as i32)
            .max()
            .unwrap_or(0);
        let line_count = lines.len() as i32;
        let inner_width = 2 * opts.padding + max_line_width;
        let inner_height = line_count + 2 * opts.padding;
        ShapeDimensions {
            width: inner_width + 4,
            height: inner_height + 2,
            label_area: LabelArea {
                x: 2 + opts.padding,
                y: 1 + opts.padding,
                width: max_line_width,
                height: line_count,
            },
            grid_columns: [2, inner_width, 2],
            grid_rows: [1, inner_height, 1],
        }
    }

    fn render(&self, label: &str, dims: &ShapeDimensions, opts: &ShapeRenderOptions) -> Canvas {
        let mut canvas = Canvas::new(dims.width, dims.height);
        let horizontal = if opts.use_ascii { '-' } else { '─' };
        let vertical = if opts.use_ascii { '|' } else { '│' };
        let right = dims.width - 1;
        let inner_right = dims.width - 2;
        let bottom = dims.height - 1;

        canvas.set(0, 0, Cell::from(if opts.use_ascii { '+' } else { '┌' }));
        canvas.set(1, 0, Cell::from(if opts.use_ascii { '+' } else { '┬' }));
        canvas.set(
            inner_right,
            0,
            Cell::from(if opts.use_ascii { '+' } else { '┬' }),
        );
        canvas.set(right, 0, Cell::from(if opts.use_ascii { '+' } else { '┐' }));
        for x in 2..inner_right {
            canvas.set(x, 0, Cell::from(horizontal));
        }
        for y in 1..bottom {
            canvas.set(0, y, Cell::from(vertical));
            canvas.set(1, y, Cell::from(vertical));
            canvas.set(inner_right, y, Cell::from(vertical));
            canvas.set(right, y, Cell::from(vertical));
        }
        canvas.set(
            0,
            bottom,
            Cell::from(if opts.use_ascii { '+' } else { '└' }),
        );
        canvas.set(
            1,
            bottom,
            Cell::from(if opts.use_ascii { '+' } else { '┴' }),
        );
        canvas.set(
            inner_right,
            bottom,
            Cell::from(if opts.use_ascii { '+' } else { '┴' }),
        );
        canvas.set(
            right,
            bottom,
            Cell::from(if opts.use_ascii { '+' } else { '┘' }),
        );
        for x in 2..inner_right {
            canvas.set(x, bottom, Cell::from(horizontal));
        }

        let lines: Vec<&str> = split_lines(label).collect();
        let start_y = dims.height / 2 - (lines.len() as i32 - 1) / 2;
        for (i, line) in lines.into_iter().enumerate() {
            let cells = to_cells(line);
            let text_x = dims.width / 2 - cells.len() as i32 / 2;
            for (j, cell) in cells.into_iter().enumerate() {
                let x = text_x + j as i32;
                let y = start_y + i as i32;
                if x > 1 && x < inner_right && y > 0 && y < bottom {
                    canvas.set(x, y, cell);
                }
            }
        }
        canvas
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

/// Database cylinder node renderer.
pub struct Cylinder;

impl ShapeRenderer for Cylinder {
    fn dimensions(&self, label: &str, opts: &ShapeRenderOptions) -> ShapeDimensions {
        let lines: Vec<&str> = split_lines(label).collect();
        let max_line_width = lines
            .iter()
            .map(|line| display_width(line) as i32)
            .max()
            .unwrap_or(0);
        let line_count = lines.len() as i32;
        let inner_width = 2 * opts.padding + max_line_width;
        let inner_height = line_count + 2 * opts.padding + 2;
        ShapeDimensions {
            width: inner_width + 2,
            height: inner_height + 2,
            label_area: LabelArea {
                x: 1 + opts.padding,
                y: 2 + opts.padding,
                width: max_line_width,
                height: line_count,
            },
            grid_columns: [1, inner_width, 1],
            grid_rows: [2, inner_height - 2, 2],
        }
    }

    fn render(&self, label: &str, dims: &ShapeDimensions, opts: &ShapeRenderOptions) -> Canvas {
        let mut canvas = Canvas::new(dims.width, dims.height);
        let horizontal = if opts.use_ascii { '-' } else { '─' };
        let vertical = if opts.use_ascii { '|' } else { '│' };
        let right = dims.width - 1;
        let bottom = dims.height - 1;

        canvas.set(0, 0, Cell::from(if opts.use_ascii { '.' } else { '╭' }));
        canvas.set(right, 0, Cell::from(if opts.use_ascii { '.' } else { '╮' }));
        canvas.set(0, 1, Cell::from(vertical));
        canvas.set(right, 1, Cell::from(vertical));
        canvas.set(0, bottom - 1, Cell::from(vertical));
        canvas.set(right, bottom - 1, Cell::from(vertical));
        canvas.set(
            0,
            bottom,
            Cell::from(if opts.use_ascii { '\'' } else { '╰' }),
        );
        canvas.set(
            right,
            bottom,
            Cell::from(if opts.use_ascii { '\'' } else { '╯' }),
        );
        for x in 1..right {
            canvas.set(x, 0, Cell::from(horizontal));
            canvas.set(x, 1, Cell::from(horizontal));
            canvas.set(x, bottom - 1, Cell::from(horizontal));
            canvas.set(x, bottom, Cell::from(horizontal));
        }
        for y in 2..bottom - 1 {
            canvas.set(0, y, Cell::from(vertical));
            canvas.set(right, y, Cell::from(vertical));
        }

        let lines: Vec<&str> = split_lines(label).collect();
        let start_y = dims.height / 2 - (lines.len() as i32 - 1) / 2;
        for (i, line) in lines.into_iter().enumerate() {
            let cells = to_cells(line);
            let text_x = dims.width / 2 - cells.len() as i32 / 2;
            for (j, cell) in cells.into_iter().enumerate() {
                let x = text_x + j as i32;
                let y = start_y + i as i32;
                if x > 0 && x < right && y > 1 && y < bottom - 1 {
                    canvas.set(x, y, cell);
                }
            }
        }
        canvas
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

macro_rules! corner_renderer {
	($(#[$meta:meta])* $name:ident, $shape:expr) => {
		$(#[$meta])*
		pub struct $name;

		impl ShapeRenderer for $name {
			fn dimensions(&self, label: &str, opts: &ShapeRenderOptions) -> ShapeDimensions {
				box_dimensions(label, opts)
			}

			fn render(
				&self,
				label: &str,
				dims: &ShapeDimensions,
				opts: &ShapeRenderOptions,
			) -> Canvas {
				render_box(label, dims, corners($shape, opts.use_ascii), opts.use_ascii)
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
	};
}

corner_renderer!(
    /// Double-circle-decorated node renderer.
    DoubleCircle,
    NodeShape::DoubleCircle
);
corner_renderer!(
    /// Left-pointed asymmetric flag node renderer.
    Asymmetric,
    NodeShape::Asymmetric
);
corner_renderer!(
    /// Bottom-wide trapezoid node renderer.
    Trapezoid,
    NodeShape::Trapezoid
);
corner_renderer!(
    /// Top-wide trapezoid node renderer.
    TrapezoidAlt,
    NodeShape::TrapezoidAlt
);
