use super::{LabelArea, ShapeDimensions, ShapeRenderOptions, ShapeRenderer};
use crate::mermaid::{
    canvas::{Canvas, Cell, DrawingCoord},
    flowchart::Dir,
};

const fn dimensions() -> ShapeDimensions {
    ShapeDimensions {
        width: 5,
        height: 3,
        label_area: LabelArea {
            x: 2,
            y: 1,
            width: 1,
            height: 1,
        },
        grid_columns: [1, 3, 1],
        grid_rows: [1, 1, 1],
    }
}

const fn attachment_point(dir: Dir, dims: &ShapeDimensions, base: DrawingCoord) -> DrawingCoord {
    let center_x = base.x + dims.width / 2;
    let center_y = base.y + dims.height / 2;
    match dir {
        Dir::Up => DrawingCoord::new(center_x, base.y),
        Dir::Down => DrawingCoord::new(center_x, base.y + dims.height - 1),
        Dir::Left => DrawingCoord::new(base.x, center_y),
        Dir::Right => DrawingCoord::new(base.x + dims.width - 1, center_y),
        _ => DrawingCoord::new(center_x, center_y),
    }
}

fn render_state(dims: &ShapeDimensions, use_ascii: bool, end: bool) -> Canvas {
    let mut canvas = Canvas::new(dims.width, dims.height);
    let (top_left, top_right, bottom_left, bottom_right, horizontal, vertical, symbol) =
        match (end, use_ascii) {
            (false, false) => ('╭', '╮', '╰', '╯', '─', '│', '●'),
            (false, true) => ('.', '.', '\'', '\'', '-', '|', '*'),
            (true, false) => ('╔', '╗', '╚', '╝', '═', '║', '◎'),
            (true, true) => ('#', '#', '#', '#', '=', '#', '*'),
        };
    let right = dims.width - 1;
    let bottom = dims.height - 1;
    canvas.set(0, 0, Cell::from(top_left));
    canvas.set(right, 0, Cell::from(top_right));
    canvas.set(0, bottom, Cell::from(bottom_left));
    canvas.set(right, bottom, Cell::from(bottom_right));
    for x in 1..right {
        canvas.set(x, 0, Cell::from(horizontal));
        canvas.set(x, bottom, Cell::from(horizontal));
    }
    canvas.set(0, 1, Cell::from(vertical));
    canvas.set(dims.width / 2, 1, Cell::from(symbol));
    canvas.set(right, 1, Cell::from(vertical));
    canvas
}

/// UML initial pseudo-state renderer.
pub struct StateStart;

impl ShapeRenderer for StateStart {
    fn dimensions(&self, _label: &str, _opts: &ShapeRenderOptions) -> ShapeDimensions {
        dimensions()
    }

    fn render(&self, _label: &str, dims: &ShapeDimensions, opts: &ShapeRenderOptions) -> Canvas {
        render_state(dims, opts.use_ascii, false)
    }

    fn attachment_point(
        &self,
        dir: Dir,
        dims: &ShapeDimensions,
        base: DrawingCoord,
    ) -> DrawingCoord {
        attachment_point(dir, dims, base)
    }
}

/// UML final pseudo-state renderer.
pub struct StateEnd;

impl ShapeRenderer for StateEnd {
    fn dimensions(&self, _label: &str, _opts: &ShapeRenderOptions) -> ShapeDimensions {
        dimensions()
    }

    fn render(&self, _label: &str, dims: &ShapeDimensions, opts: &ShapeRenderOptions) -> Canvas {
        render_state(dims, opts.use_ascii, true)
    }

    fn attachment_point(
        &self,
        dir: Dir,
        dims: &ShapeDimensions,
        base: DrawingCoord,
    ) -> DrawingCoord {
        attachment_point(dir, dims, base)
    }
}
