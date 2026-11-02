use super::{
    rectangle::box_attachment_point, LabelArea, ShapeDimensions, ShapeRenderOptions, ShapeRenderer,
};
use crate::mermaid::{
    canvas::{to_cells, Canvas, Cell, DrawingCoord},
    flowchart::Dir,
    text::{display_width, split_lines},
};

/// Pill-shaped stadium node renderer.
pub struct Stadium;

impl ShapeRenderer for Stadium {
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
            height: (inner_height + 2).max(3),
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
        let center_y = dims.height / 2;
        let right = dims.width - 1;
        let bottom = dims.height - 1;
        let horizontal = if opts.use_ascii { '-' } else { '─' };

        if dims.height == 3 {
            canvas.set(0, center_y, Cell::from('('));
            canvas.set(right, center_y, Cell::from(')'));
        } else if !opts.use_ascii {
            canvas.set(0, 0, Cell::from('╭'));
            canvas.set(right, 0, Cell::from('╮'));
            for x in 1..right {
                canvas.set(x, 0, Cell::from(horizontal));
            }
            for y in 1..bottom {
                canvas.set(0, y, Cell::from('│'));
                canvas.set(right, y, Cell::from('│'));
            }
            canvas.set(0, bottom, Cell::from('╰'));
            canvas.set(right, bottom, Cell::from('╯'));
            for x in 1..right {
                canvas.set(x, bottom, Cell::from(horizontal));
            }
        } else {
            for y in 0..dims.height {
                canvas.set(0, y, Cell::from('('));
                canvas.set(right, y, Cell::from(')'));
            }
            for x in 1..right {
                canvas.set(x, 0, Cell::from(horizontal));
                canvas.set(x, bottom, Cell::from(horizontal));
            }
        }

        let lines: Vec<&str> = split_lines(label).collect();
        let start_y = center_y - (lines.len() as i32 - 1) / 2;
        for (i, line) in lines.into_iter().enumerate() {
            let cells = to_cells(line);
            let text_x = dims.width / 2 - cells.len() as i32 / 2;
            for (j, cell) in cells.into_iter().enumerate() {
                let x = text_x + j as i32;
                let y = start_y + i as i32;
                if x > 0 && x < right && y >= 0 && y < dims.height {
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
