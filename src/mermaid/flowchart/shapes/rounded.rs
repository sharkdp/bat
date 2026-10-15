use super::{
    corners::corners,
    rectangle::{box_attachment_point, box_dimensions, render_box},
    ShapeDimensions, ShapeRenderOptions, ShapeRenderer,
};
use crate::mermaid::{
    canvas::{Canvas, DrawingCoord},
    flowchart::{Dir, NodeShape},
};

/// Rounded rectangular node renderer.
pub struct Rounded;

impl ShapeRenderer for Rounded {
    fn dimensions(&self, label: &str, opts: &ShapeRenderOptions) -> ShapeDimensions {
        box_dimensions(label, opts)
    }

    fn render(&self, label: &str, dims: &ShapeDimensions, opts: &ShapeRenderOptions) -> Canvas {
        render_box(
            label,
            dims,
            corners(NodeShape::Rounded, opts.use_ascii),
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
