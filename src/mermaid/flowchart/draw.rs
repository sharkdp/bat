use std::collections::HashSet;

use super::{
    grid::{grid_to_drawing_coord, line_to_drawing},
    routing::determine_direction,
    shapes::{corners::corners, shape_attachment_point, LabelArea, ShapeDimensions},
    AsciiEdge, AsciiGraph, BundleKind, Dir, EdgeBundle, EdgeStyle, GridCoord, NodeId, NodeShape,
    SubgraphId,
};
use crate::mermaid::{
    ansi::CharRole,
    canvas::{to_cells, Canvas, Cell, CellSurface, DrawingCoord, Layer, RoleCanvas},
    text::{display_width, split_lines, LABEL_SPACE},
};

/// Draw a node in the dimensions allocated to its grid cell.
pub fn draw_node(graph: &AsciiGraph, node: NodeId) -> Canvas {
    let Some(node) = graph.nodes.get(node) else {
        return Canvas::new(1, 1);
    };
    let Some(gc) = node.grid_coord else {
        return Canvas::new(1, 1);
    };
    let use_ascii = graph.config.use_ascii;
    let width = (0..2)
        .map(|i| graph.column_width.get(&(gc.x + i)).copied().unwrap_or(0))
        .sum();
    let height = (0..2)
        .map(|i| graph.row_height.get(&(gc.y + i)).copied().unwrap_or(0))
        .sum();
    let mut canvas = Canvas::new(width + 1, height + 1);

    let shape_corners = corners(node.shape, use_ascii);
    let state_start = node.shape == NodeShape::StateStart;
    let state_end = node.shape == NodeShape::StateEnd;
    let (horizontal, vertical) = if use_ascii {
        (
            if state_end { '=' } else { '-' },
            if state_end { '‖' } else { '|' },
        )
    } else {
        (
            if state_end { '═' } else { '─' },
            if state_end { '║' } else { '│' },
        )
    };
    let (top_left, top_right, bottom_left, bottom_right) = if state_end {
        if use_ascii {
            ('#', '#', '#', '#')
        } else {
            ('╔', '╗', '╚', '╝')
        }
    } else if state_start {
        if use_ascii {
            ('+', '+', '+', '+')
        } else {
            ('╭', '╮', '╰', '╯')
        }
    } else {
        (
            shape_corners.top_left,
            shape_corners.top_right,
            shape_corners.bottom_left,
            shape_corners.bottom_right,
        )
    };

    for x in 1..width {
        canvas.set(x, 0, Cell::from(horizontal));
        canvas.set(x, height, Cell::from(horizontal));
    }
    for y in 1..height {
        canvas.set(0, y, Cell::from(vertical));
        canvas.set(width, y, Cell::from(vertical));
    }
    canvas.set(0, 0, Cell::from(top_left));
    canvas.set(width, 0, Cell::from(top_right));
    canvas.set(0, height, Cell::from(bottom_left));
    canvas.set(width, height, Cell::from(bottom_right));

    let fallback;
    let label = if node.label.is_empty() {
        fallback = if state_start {
            if use_ascii {
                "*"
            } else {
                "●"
            }
        } else if state_end {
            if use_ascii {
                "*"
            } else {
                "◎"
            }
        } else {
            ""
        };
        fallback
    } else {
        &node.label
    };
    let lines: Vec<&str> = split_lines(label).collect();
    let start_y = height / 2 - (lines.len() as i32 - 1).div_euclid(2);
    for (i, line) in lines.iter().enumerate() {
        let cells = to_cells(line);
        let text_x = width / 2 - (cells.len() as i32 + 1) / 2 + 1;
        for (j, cell) in cells.into_iter().enumerate() {
            let x = text_x + j as i32;
            let y = start_y + i as i32;
            if x >= 0 && x < canvas.width() && y >= 0 && y < canvas.height() {
                canvas.set(x, y, cell);
            }
        }
    }
    canvas
}

/// Outer `(width, height)` of a multi-section box: one column of padding
/// each side of the widest line, and one divider row between sections.
pub fn multi_box_size(sections: &[Vec<String>]) -> (i32, i32) {
    let max_text_width = sections
        .iter()
        .flatten()
        .map(|line| display_width(line) as i32)
        .max()
        .unwrap_or(0);
    let total_lines: i32 = sections
        .iter()
        .map(|section| section.len().max(1) as i32)
        .sum();
    (
        max_text_width + 4,
        total_lines + sections.len() as i32 - 1 + 2,
    )
}

/// Write `cell` with `role` when `(x, y)` lies on the canvas.
pub fn set_cell(
    canvas: &mut Canvas,
    roles: &mut RoleCanvas,
    x: i32,
    y: i32,
    cell: Cell,
    role: CharRole,
) {
    if canvas.in_bounds(x, y) {
        canvas.set(x, y, cell);
        roles.set_role(x, y, role);
    }
}

/// Draw a box split into left-aligned text sections with its top-left corner
/// at `(x, y)`. Spaces inside the box are left untouched.
pub fn stamp_multi_box(
    canvas: &mut Canvas,
    roles: &mut RoleCanvas,
    sections: &[Vec<String>],
    x: i32,
    y: i32,
    use_ascii: bool,
) {
    let (box_width, box_height) = multi_box_size(sections);
    let (
        horizontal,
        vertical,
        top_left,
        top_right,
        bottom_left,
        bottom_right,
        divider_left,
        divider_right,
    ) = if use_ascii {
        ('-', '|', '+', '+', '+', '+', '+', '+')
    } else {
        ('─', '│', '┌', '┐', '└', '┘', '├', '┤')
    };
    let (right, bottom) = (box_width - 1, box_height - 1);
    let mut cell_at = |dx: i32, dy: i32, cell: Cell, role: CharRole| {
        set_cell(canvas, roles, x + dx, y + dy, cell, role);
    };
    let mut border = |dx: i32, dy: i32, ch: char| cell_at(dx, dy, Cell::from(ch), CharRole::Border);
    border(0, 0, top_left);
    border(right, 0, top_right);
    border(0, bottom, bottom_left);
    border(right, bottom, bottom_right);
    for dx in 1..right {
        border(dx, 0, horizontal);
        border(dx, bottom, horizontal);
    }
    for dy in 1..bottom {
        border(0, dy, vertical);
        border(right, dy, vertical);
    }
    let mut row = 1;
    for (section_index, section) in sections.iter().enumerate() {
        if section_index > 0 {
            border(0, row, divider_left);
            border(right, row, divider_right);
            for dx in 1..right {
                border(dx, row, horizontal);
            }
            row += 1;
        }
        row += section.len().max(1) as i32;
    }

    let mut row = 1;
    for section in sections {
        for line in section {
            for (i, cell) in to_cells(line).into_iter().enumerate() {
                if cell.is_space() {
                    continue;
                }
                // Border glyphs inside member text (`+id`, `-x`, `a|b`) take the
                // border role: box cells have always been classified by glyph.
                let role = match cell.as_char() {
                    Some(
                        '┌' | '┐' | '└' | '┘' | '├' | '┤' | '┬' | '┴' | '┼' | '│' | '─' | '╭' | '╮'
                        | '╰' | '╯' | '+' | '-' | '|',
                    ) => CharRole::Border,
                    _ => CharRole::Text,
                };
                cell_at(2 + i as i32, row, cell, role);
            }
            row += 1;
        }
        // Empty sections still occupy a blank row; every section is followed
        // by a divider (or the bottom border).
        row += i32::from(section.is_empty()) + 1;
    }
}

const fn drawing_direction(from: DrawingCoord, to: DrawingCoord) -> Dir {
    if from.x == to.x {
        if from.y < to.y {
            Dir::Down
        } else {
            Dir::Up
        }
    } else if from.y == to.y {
        if from.x < to.x {
            Dir::Right
        } else {
            Dir::Left
        }
    } else if from.x < to.x {
        if from.y < to.y {
            Dir::LowerRight
        } else {
            Dir::UpperRight
        }
    } else if from.y < to.y {
        Dir::LowerLeft
    } else {
        Dir::UpperLeft
    }
}

const fn line_chars(style: EdgeStyle, use_ascii: bool) -> (char, char) {
    match (style, use_ascii) {
        (EdgeStyle::Solid, false) => ('─', '│'),
        (EdgeStyle::Solid, true) => ('-', '|'),
        (EdgeStyle::Dotted, false) => ('┄', '┆'),
        (EdgeStyle::Dotted, true) => ('.', ':'),
        (EdgeStyle::Thick, false) => ('━', '┃'),
        (EdgeStyle::Thick, true) => ('=', '‖'),
    }
}

/// Draw an orthogonal line and return every coordinate written.
pub fn draw_line(
    canvas: &mut impl CellSurface,
    from: DrawingCoord,
    to: DrawingCoord,
    offset_from: i32,
    offset_to: i32,
    use_ascii: bool,
    style: EdgeStyle,
) -> Vec<DrawingCoord> {
    let dir = drawing_direction(from, to);
    let (horizontal, vertical) = line_chars(style, use_ascii);
    let mut drawn = Vec::new();
    let mut put = |x: i32, y: i32, c: char| {
        drawn.push(DrawingCoord::new(x, y));
        canvas.put(x, y, Cell::from(c));
    };

    match dir {
        Dir::Up => {
            let mut y = from.y - offset_from;
            while y >= to.y - offset_to {
                put(from.x, y, vertical);
                y -= 1;
            }
        }
        Dir::Down => {
            let mut y = from.y + offset_from;
            while y <= to.y + offset_to {
                put(from.x, y, vertical);
                y += 1;
            }
        }
        Dir::Left => {
            let mut x = from.x - offset_from;
            while x >= to.x - offset_to {
                put(x, from.y, horizontal);
                x -= 1;
            }
        }
        Dir::Right => {
            let mut x = from.x + offset_from;
            while x <= to.x + offset_to {
                put(x, from.y, horizontal);
                x += 1;
            }
        }
        Dir::UpperLeft | Dir::UpperRight | Dir::LowerLeft | Dir::LowerRight => {
            if dir == Dir::LowerRight && to.x - from.x <= 1 {
                let mut y = from.y + offset_from;
                while y <= to.y + offset_to {
                    put(from.x, y, vertical);
                    y += 1;
                }
                return drawn;
            }
            let right = matches!(dir, Dir::UpperRight | Dir::LowerRight);
            let down = matches!(dir, Dir::LowerLeft | Dir::LowerRight);
            let mut x = if right {
                from.x + offset_from
            } else {
                from.x - offset_from
            };
            while if right { x <= to.x } else { x >= to.x } {
                put(x, from.y, horizontal);
                x += if right { 1 } else { -1 };
            }
            let mut y = if down { from.y + 1 } else { from.y - 1 };
            let end = to.y + if down { offset_to } else { -offset_to };
            while if down { y <= end } else { y >= end } {
                put(to.x, y, vertical);
                y += if down { 1 } else { -1 };
            }
        }
        Dir::Middle => {}
    }
    drawn
}

/// Draw an edge into six independently composited layers.
pub fn draw_arrow(graph: &AsciiGraph, edge: &AsciiEdge) -> [Layer; 6] {
    if edge.path.is_empty() {
        return std::array::from_fn(|_| Layer::over(&graph.canvas));
    }
    let label_canvas = draw_arrow_label(graph, edge);
    let (path_canvas, lines_drawn, line_dirs) = draw_path(graph, &edge.path, edge.style);
    let source_shape = graph
        .nodes
        .get(edge.from)
        .map(|node| node.shape)
        .unwrap_or_default();
    let box_start_canvas = draw_box_start(graph, &edge.path, source_shape);
    let arrow_end_canvas = if edge.has_arrow_end {
        match (lines_drawn.last(), line_dirs.last()) {
            (Some(line), Some(&dir)) => draw_arrow_head(graph, line, dir),
            _ => Layer::over(&graph.canvas),
        }
    } else {
        Layer::over(&graph.canvas)
    };
    let arrow_start_canvas = if edge.has_arrow_start {
        match (lines_drawn.first(), line_dirs.first().copied()) {
            (Some(line), Some(dir)) if !line.is_empty() => {
                let first = line[0];
                let mut arrow_position = first;
                match dir {
                    Dir::Right => arrow_position.x -= 1,
                    Dir::Left => arrow_position.x += 1,
                    Dir::Down => arrow_position.y -= 1,
                    Dir::Up => arrow_position.y += 1,
                    _ => {}
                }
                draw_arrow_head(graph, &[first, arrow_position], reverse_direction(dir))
            }
            _ => Layer::over(&graph.canvas),
        }
    } else {
        Layer::over(&graph.canvas)
    };
    let corners_canvas = draw_corners(graph, &edge.path);
    [
        path_canvas,
        box_start_canvas,
        arrow_end_canvas,
        arrow_start_canvas,
        corners_canvas,
        label_canvas,
    ]
}

const fn reverse_direction(dir: Dir) -> Dir {
    match dir {
        Dir::Up => Dir::Down,
        Dir::Down => Dir::Up,
        Dir::Left => Dir::Right,
        Dir::Right => Dir::Left,
        Dir::UpperLeft => Dir::LowerRight,
        Dir::UpperRight => Dir::LowerLeft,
        Dir::LowerLeft => Dir::UpperRight,
        Dir::LowerRight => Dir::UpperLeft,
        Dir::Middle => Dir::Middle,
    }
}

fn draw_path(
    graph: &AsciiGraph,
    path: &[GridCoord],
    style: EdgeStyle,
) -> (Layer, Vec<Vec<DrawingCoord>>, Vec<Dir>) {
    let mut canvas = Layer::over(&graph.canvas);
    let mut lines = Vec::new();
    let mut directions = Vec::new();
    let Some(&first) = path.first() else {
        return (canvas, lines, directions);
    };
    let mut previous = first;
    for &next in &path[1..] {
        let previous_drawing = grid_to_drawing_coord(graph, previous);
        let next_drawing = grid_to_drawing_coord(graph, next);
        if previous_drawing == next_drawing {
            previous = next;
            continue;
        }
        let dir = determine_direction(previous, next);
        let mut segment = draw_line(
            &mut canvas,
            previous_drawing,
            next_drawing,
            1,
            -1,
            graph.config.use_ascii,
            style,
        );
        if segment.is_empty() {
            segment.push(previous_drawing);
        }
        lines.push(segment);
        directions.push(dir);
        previous = next;
    }
    (canvas, lines, directions)
}

/// Mark where an edge leaves its source box with a tee on the border. The
/// path's first cell is the box's border cell, so the tee goes exactly there
/// even when the first segment is too short to draw any line cell.
fn draw_box_start(graph: &AsciiGraph, path: &[GridCoord], source_shape: NodeShape) -> Layer {
    let mut canvas = Layer::over(&graph.canvas);
    if graph.config.use_ascii || matches!(source_shape, NodeShape::StateStart | NodeShape::StateEnd)
    {
        return canvas;
    }
    let Some((&first, &second)) = path.first().zip(path.get(1)) else {
        return canvas;
    };
    let from = grid_to_drawing_coord(graph, first);
    let tee = match determine_direction(first, second) {
        Dir::Up => '┴',
        Dir::Down => '┬',
        Dir::Left => '┤',
        Dir::Right => '├',
        _ => return canvas,
    };
    canvas.set(from.x, from.y, Cell::from(tee));
    canvas
}

const fn arrowhead_char(dir: Dir, fallback: Dir, use_ascii: bool) -> char {
    if use_ascii {
        match dir {
            Dir::Up => '^',
            Dir::Down => 'v',
            Dir::Left => '<',
            Dir::Right => '>',
            _ => match fallback {
                Dir::Up => '^',
                Dir::Down => 'v',
                Dir::Left => '<',
                Dir::Right => '>',
                _ => '*',
            },
        }
    } else {
        match dir {
            Dir::Up => '▲',
            Dir::Down => '▼',
            Dir::Left => '◄',
            Dir::Right => '►',
            Dir::UpperRight => '◥',
            Dir::UpperLeft => '◤',
            Dir::LowerRight => '◢',
            Dir::LowerLeft => '◣',
            Dir::Middle => match fallback {
                Dir::Up => '▲',
                Dir::Down => '▼',
                Dir::Left => '◄',
                Dir::Right => '►',
                Dir::UpperRight => '◥',
                Dir::UpperLeft => '◤',
                Dir::LowerRight => '◢',
                Dir::LowerLeft => '◣',
                Dir::Middle => '●',
            },
        }
    }
}

fn draw_arrow_head(graph: &AsciiGraph, line: &[DrawingCoord], fallback: Dir) -> Layer {
    let mut canvas = Layer::over(&graph.canvas);
    let (Some(&from), Some(&last)) = (line.first(), line.last()) else {
        return canvas;
    };
    let mut dir = drawing_direction(from, last);
    if line.len() == 1 || dir == Dir::Middle {
        dir = fallback;
    }
    canvas.set(
        last.x,
        last.y,
        Cell::from(arrowhead_char(dir, fallback, graph.config.use_ascii)),
    );
    canvas
}

const fn corner_char(previous: Dir, next: Dir, use_ascii: bool) -> char {
    if use_ascii {
        return '+';
    }
    match (previous, next) {
        (Dir::Right, Dir::Down) | (Dir::Up, Dir::Left) => '┐',
        (Dir::Right, Dir::Up) | (Dir::Down, Dir::Left) => '┘',
        (Dir::Left, Dir::Down) | (Dir::Up, Dir::Right) => '┌',
        (Dir::Left, Dir::Up) | (Dir::Down, Dir::Right) => '└',
        _ => '+',
    }
}

fn draw_corners(graph: &AsciiGraph, path: &[GridCoord]) -> Layer {
    let mut canvas = Layer::over(&graph.canvas);
    for points in path.windows(3) {
        let drawing = grid_to_drawing_coord(graph, points[1]);
        let previous = determine_direction(points[0], points[1]);
        let next = determine_direction(points[1], points[2]);
        canvas.set(
            drawing.x,
            drawing.y,
            Cell::from(corner_char(previous, next, graph.config.use_ascii)),
        );
    }
    canvas
}

fn draw_arrow_label(graph: &AsciiGraph, edge: &AsciiEdge) -> Layer {
    let mut canvas = Layer::over(&graph.canvas);
    if edge.text.is_empty() {
        return canvas;
    }
    let line = line_to_drawing(graph, &edge.label_line);
    let upward = match (edge.path.first(), edge.path.last()) {
        (Some(start), Some(end)) if edge.path.len() >= 2 && end.y < start.y => Some(true),
        (Some(start), Some(end)) if edge.path.len() >= 2 && end.y > start.y => Some(false),
        _ => None,
    };
    draw_text_on_line(&mut canvas, &line, &edge.text, upward);
    canvas
}

fn draw_text_on_line(canvas: &mut Layer, line: &[DrawingCoord], label: &str, upward: Option<bool>) {
    let (Some(first), Some(second)) = (line.first(), line.get(1)) else {
        return;
    };
    let min_x = first.x.min(second.x);
    let max_x = first.x.max(second.x);
    let min_y = first.y.min(second.y);
    let max_y = first.y.max(second.y);
    let middle_x = min_x + (max_x - min_x).div_euclid(2);
    let mut middle_y = min_y + (max_y - min_y).div_euclid(2);
    if let Some(upward) = upward.filter(|_| min_x == max_x) {
        let offset = 1.max((max_y - min_y).div_euclid(4));
        middle_y += if upward { offset } else { -offset };
    }
    let lines: Vec<&str> = split_lines(label).collect();
    let start_y = middle_y - (lines.len() as i32 - 1).div_euclid(2);
    for (i, line) in lines.iter().enumerate() {
        let x = middle_x - (display_width(line) as i32).div_euclid(2);
        let protected = line.replace(' ', &LABEL_SPACE.to_string());
        canvas.draw_text(DrawingCoord::new(x, start_y + i as i32), &protected, false);
    }
}

fn node_attachment_point(graph: &AsciiGraph, node: NodeId, dir: Dir) -> Option<DrawingCoord> {
    let node = graph.nodes.get(node)?;
    let gc = node.grid_coord?;
    let width: i32 = (0..2)
        .map(|i| graph.column_width.get(&(gc.x + i)).copied().unwrap_or(0))
        .sum();
    let height: i32 = (0..2)
        .map(|i| graph.row_height.get(&(gc.y + i)).copied().unwrap_or(0))
        .sum();
    let dimensions = ShapeDimensions {
        width: width + 1,
        height: height + 1,
        label_area: LabelArea {
            x: 0,
            y: 0,
            width: 0,
            height: 0,
        },
        grid_columns: [0, 0, 0],
        grid_rows: [0, 0, 0],
    };
    let mut point = shape_attachment_point(node.shape, dir, &dimensions, node.drawing_coord?);
    // Edge paths run through grid cell centers; when the box is an even
    // number of cells wide (2-wide border columns), its midpoint rounds away
    // from the center of the middle column, so snap cardinal attachments to
    // the column/row the path actually uses.
    let middle = grid_to_drawing_coord(graph, GridCoord::new(gc.x + 1, gc.y + 1));
    match dir {
        Dir::Up | Dir::Down => point.x = middle.x,
        Dir::Left | Dir::Right => point.y = middle.y,
        _ => {}
    }
    Some(point)
}

fn draw_bundled_edge_segment(
    graph: &AsciiGraph,
    edge: &AsciiEdge,
    bundle: &EdgeBundle,
) -> [Layer; 6] {
    if edge.path_to_junction.is_empty() {
        return std::array::from_fn(|_| Layer::over(&graph.canvas));
    }
    let mut path_canvas = Layer::over(&graph.canvas);
    let drawing_path: Vec<DrawingCoord> = edge
        .path_to_junction
        .iter()
        .enumerate()
        .map(|(index, &coord)| {
            if bundle.kind == BundleKind::FanIn && index == 0 {
                node_attachment_point(graph, edge.from, edge.start_dir)
                    .unwrap_or_else(|| grid_to_drawing_coord(graph, coord))
            } else if bundle.kind == BundleKind::FanOut && index + 1 == edge.path_to_junction.len()
            {
                node_attachment_point(graph, edge.to, edge.end_dir)
                    .unwrap_or_else(|| grid_to_drawing_coord(graph, coord))
            } else {
                grid_to_drawing_coord(graph, coord)
            }
        })
        .collect();
    for pair in drawing_path.windows(2) {
        if pair[0] != pair[1] {
            draw_line(
                &mut path_canvas,
                pair[0],
                pair[1],
                1,
                -1,
                graph.config.use_ascii,
                edge.style,
            );
        }
    }

    let mut corners_canvas = Layer::over(&graph.canvas);
    for points in edge.path_to_junction.windows(3) {
        let dc = grid_to_drawing_coord(graph, points[1]);
        let c = corner_char(
            determine_direction(points[0], points[1]),
            determine_direction(points[1], points[2]),
            graph.config.use_ascii,
        );
        corners_canvas.set(dc.x, dc.y, Cell::from(c));
    }

    let mut box_start_canvas = Layer::over(&graph.canvas);
    if bundle.kind == BundleKind::FanIn
        && edge.path_to_junction.len() >= 2
        && !graph.config.use_ascii
    {
        if let Some(&point) = drawing_path.first() {
            let dir = determine_direction(edge.path_to_junction[0], edge.path_to_junction[1]);
            let c = match dir {
                Dir::Up => Some('┴'),
                Dir::Down => Some('┬'),
                Dir::Left => Some('┤'),
                Dir::Right => Some('├'),
                _ => None,
            };
            if let Some(c) = c {
                box_start_canvas.set(point.x, point.y, Cell::from(c));
            }
        }
    }
    [
        path_canvas,
        box_start_canvas,
        Layer::over(&graph.canvas),
        Layer::over(&graph.canvas),
        corners_canvas,
        Layer::over(&graph.canvas),
    ]
}

fn draw_bundle_shared_path(graph: &AsciiGraph, bundle: &EdgeBundle) -> (Layer, Layer) {
    let mut path_canvas = Layer::over(&graph.canvas);
    let mut corners_canvas = Layer::over(&graph.canvas);
    if bundle.shared_path.len() < 2 {
        return (path_canvas, corners_canvas);
    }
    let style = bundle
        .edges
        .first()
        .and_then(|&edge| graph.edges.get(edge))
        .map_or(EdgeStyle::Solid, |edge| edge.style);
    let last_index = bundle.shared_path.len() - 1;
    let drawing_path: Vec<DrawingCoord> = bundle
        .shared_path
        .iter()
        .enumerate()
        .map(|(index, &coord)| {
            if bundle.kind == BundleKind::FanIn && index == last_index {
                let entry = if graph.config.direction == super::LayoutDirection::TD {
                    Dir::Up
                } else {
                    Dir::Left
                };
                node_attachment_point(graph, bundle.shared_node, entry)
                    .unwrap_or_else(|| grid_to_drawing_coord(graph, coord))
            } else if bundle.kind == BundleKind::FanOut && index == 0 {
                let exit = if graph.config.direction == super::LayoutDirection::TD {
                    Dir::Down
                } else {
                    Dir::Right
                };
                node_attachment_point(graph, bundle.shared_node, exit)
                    .unwrap_or_else(|| grid_to_drawing_coord(graph, coord))
            } else {
                grid_to_drawing_coord(graph, coord)
            }
        })
        .collect();
    for pair in drawing_path.windows(2) {
        if pair[0] != pair[1] {
            draw_line(
                &mut path_canvas,
                pair[0],
                pair[1],
                1,
                -1,
                graph.config.use_ascii,
                style,
            );
        }
    }
    for points in bundle.shared_path.windows(3) {
        let dc = grid_to_drawing_coord(graph, points[1]);
        let c = corner_char(
            determine_direction(points[0], points[1]),
            determine_direction(points[1], points[2]),
            graph.config.use_ascii,
        );
        corners_canvas.set(dc.x, dc.y, Cell::from(c));
    }
    (path_canvas, corners_canvas)
}

/// Arrowhead just outside `node`'s entry side, pointing along the last step of
/// `path` (a bundle's shared path or a fan-out edge's path to the junction).
fn draw_bundle_arrowhead(graph: &AsciiGraph, path: &[GridCoord], node: NodeId) -> Layer {
    let mut canvas = Layer::over(&graph.canvas);
    let [.., before, last] = path else {
        return canvas;
    };
    let td = graph.config.direction == super::LayoutDirection::TD;
    let Some(mut dc) = node_attachment_point(graph, node, if td { Dir::Up } else { Dir::Left })
    else {
        return canvas;
    };
    if td {
        dc.y -= 1;
    } else {
        dc.x -= 1;
    }
    // Bundle arrowheads only use the four cardinal glyphs; anything else
    // (diagonal or degenerate step) points down.
    let direction = match determine_direction(*before, *last) {
        dir @ (Dir::Up | Dir::Down | Dir::Left | Dir::Right) => dir,
        _ => Dir::Down,
    };
    let c = arrowhead_char(direction, Dir::Down, graph.config.use_ascii);
    canvas.set(dc.x, dc.y, Cell::from(c));
    canvas
}

fn draw_junction_character(graph: &AsciiGraph, bundle: &EdgeBundle) -> Layer {
    let mut canvas = Layer::over(&graph.canvas);
    let Some(junction) = bundle.junction_point else {
        return canvas;
    };
    let dc = grid_to_drawing_coord(graph, junction);
    let (mut up, mut down, mut left, mut right) = (false, false, false, false);
    if bundle.shared_path.len() >= 2 {
        let (junction_index, adjacent_index) = if bundle.kind == BundleKind::FanIn {
            (0, 1)
        } else {
            (bundle.shared_path.len() - 1, bundle.shared_path.len() - 2)
        };
        match determine_direction(
            bundle.shared_path[junction_index],
            bundle.shared_path[adjacent_index],
        ) {
            Dir::Up => up = true,
            Dir::Down => down = true,
            Dir::Left => left = true,
            Dir::Right => right = true,
            _ => {}
        }
    }
    for &edge_id in &bundle.edges {
        let Some(edge) = graph.edges.get(edge_id) else {
            continue;
        };
        if edge.path_to_junction.len() < 2 {
            continue;
        }
        let (junction_index, adjacent_index) = if bundle.kind == BundleKind::FanIn {
            (
                edge.path_to_junction.len() - 1,
                edge.path_to_junction.len() - 2,
            )
        } else {
            (0, 1)
        };
        match determine_direction(
            edge.path_to_junction[adjacent_index],
            edge.path_to_junction[junction_index],
        ) {
            Dir::Down => up = true,
            Dir::Up => down = true,
            Dir::Right => left = true,
            Dir::Left => right = true,
            _ => {}
        }
    }
    let c = if graph.config.use_ascii {
        '+'
    } else {
        match (up, down, left, right) {
            (true, true, true, true) => '┼',
            (false, true, true, true) => '┬',
            (true, false, true, true) => '┴',
            (true, true, false, true) => '├',
            (true, true, true, false) => '┤',
            (_, _, true, true) => '─',
            (true, true, ..) => '│',
            (false, true, false, true) => '┌',
            (false, true, true, false) => '┐',
            (true, false, false, true) => '└',
            (true, false, true, false) => '┘',
            _ => '┼',
        }
    };
    canvas.set(dc.x, dc.y, Cell::from(c));
    canvas
}

/// Draw a subgraph border relative to its bounding-box origin.
pub fn draw_subgraph_box(graph: &AsciiGraph, sg: SubgraphId) -> Canvas {
    let Some(sg) = graph.subgraphs.get(sg) else {
        return Canvas::new(1, 1);
    };
    let width = sg.max_x - sg.min_x;
    let height = sg.max_y - sg.min_y;
    if width <= 0 || height <= 0 {
        return Canvas::new(1, 1);
    }
    let mut canvas = Canvas::new(width + 1, height + 1);
    let (horizontal, vertical, top_left, top_right, bottom_left, bottom_right) =
        if graph.config.use_ascii {
            ('-', '|', '+', '+', '+', '+')
        } else {
            ('─', '│', '┌', '┐', '└', '┘')
        };
    for x in 1..width {
        canvas.set(x, 0, Cell::from(horizontal));
        canvas.set(x, height, Cell::from(horizontal));
    }
    for y in 1..height {
        canvas.set(0, y, Cell::from(vertical));
        canvas.set(width, y, Cell::from(vertical));
    }
    canvas.set(0, 0, Cell::from(top_left));
    canvas.set(width, 0, Cell::from(top_right));
    canvas.set(0, height, Cell::from(bottom_left));
    canvas.set(width, height, Cell::from(bottom_right));
    canvas
}

/// Draw a subgraph label and return its placement offset.
pub fn draw_subgraph_label(graph: &AsciiGraph, sg: SubgraphId) -> (Canvas, DrawingCoord) {
    let Some(sg) = graph.subgraphs.get(sg) else {
        return (Canvas::new(1, 1), DrawingCoord::new(0, 0));
    };
    let width = sg.max_x - sg.min_x;
    let height = sg.max_y - sg.min_y;
    if width <= 0 || height <= 0 {
        return (Canvas::new(1, 1), DrawingCoord::new(0, 0));
    }
    let mut canvas = Canvas::new(width + 1, height + 1);
    for (i, line) in split_lines(&sg.name).enumerate() {
        let y = 1 + i as i32;
        let x = (width.div_euclid(2) - (display_width(line) as i32).div_euclid(2)).max(1);
        let cells = to_cells(line);
        for (j, cell) in cells.iter().enumerate() {
            if cell.is_wide_pad() {
                continue;
            }
            let cx = x + j as i32;
            let wide = cells.get(j + 1).is_some_and(Cell::is_wide_pad);
            if cx + i32::from(wide) >= width || y >= height {
                continue;
            }
            canvas.set(cx, y, cell.clone());
            if wide {
                canvas.set(cx + 1, y, Cell::WIDE_PAD);
            }
        }
    }
    (canvas, DrawingCoord::new(sg.min_x, sg.min_y))
}

fn subgraph_depth(graph: &AsciiGraph, mut sg: SubgraphId) -> usize {
    let mut depth = 0;
    let mut seen = HashSet::new();
    while seen.insert(sg) {
        let Some(parent) = graph.subgraphs.get(sg).and_then(|item| item.parent) else {
            break;
        };
        depth += 1;
        sg = parent;
    }
    depth
}

fn fill_roles_from_canvas(
    roles: &mut RoleCanvas,
    canvas: &Canvas,
    offset: DrawingCoord,
    role: CharRole,
) {
    for x in 0..canvas.width() {
        for y in 0..canvas.height() {
            if canvas.get(x, y).is_some_and(|cell| !cell.is_space()) {
                let rx = x + offset.x;
                let ry = y + offset.y;
                if rx >= 0 && ry >= 0 {
                    roles.set_role(rx, ry, role);
                }
            }
        }
    }
}

fn fill_roles_from_layers(roles: &mut RoleCanvas, layers: &[Layer], role: CharRole) {
    for layer in layers {
        for (x, y, cell) in layer.cells() {
            if !cell.is_space() {
                roles.set_role(x, y, role);
            }
        }
    }
}

const fn is_border_char(c: char) -> bool {
    matches!(
        c,
        '┌' | '┐'
            | '└'
            | '┘'
            | '├'
            | '┤'
            | '┬'
            | '┴'
            | '┼'
            | '│'
            | '─'
            | '╭'
            | '╮'
            | '╰'
            | '╯'
            | '+'
            | '-'
            | '|'
            | '.'
            | '\''
            | ':'
    )
}

const fn is_state_end_border_char(c: char) -> bool {
    matches!(c, '╔' | '╗' | '╚' | '╝' | '═' | '║' | '#' | '=' | '‖')
}

fn fill_roles_for_node_box(roles: &mut RoleCanvas, canvas: &Canvas, offset: DrawingCoord) {
    let max_x = canvas.width() - 1;
    let max_y = canvas.height() - 1;
    for x in 0..canvas.width() {
        for y in 0..canvas.height() {
            let Some(cell) = canvas.get(x, y) else {
                continue;
            };
            if cell.is_space() {
                continue;
            }
            let rx = x + offset.x;
            let ry = y + offset.y;
            if rx < 0 || ry < 0 {
                continue;
            }
            let outer = x == 0 || x == max_x || y == 0 || y == max_y;
            let border = cell
                .as_char()
                .is_some_and(|c| is_border_char(c) || outer && is_state_end_border_char(c));
            roles.set_role(
                rx,
                ry,
                if border {
                    CharRole::Border
                } else {
                    CharRole::Text
                },
            );
        }
    }
}

/// Composite subgraphs, nodes, edges, labels, and role metadata into the graph
/// canvases.
pub fn draw_graph(graph: &mut AsciiGraph) {
    let use_ascii = graph.config.use_ascii;
    let mut subgraphs: Vec<SubgraphId> = (0..graph.subgraphs.len()).collect();
    subgraphs.sort_by_key(|&sg| subgraph_depth(graph, sg));
    for sg in subgraphs {
        let canvas = draw_subgraph_box(graph, sg);
        let Some(bounds) = graph.subgraphs.get(sg) else {
            continue;
        };
        let offset = DrawingCoord::new(bounds.min_x, bounds.min_y);
        graph.canvas.merge(offset, use_ascii, &[&canvas]);
        fill_roles_from_canvas(&mut graph.role_canvas, &canvas, offset, CharRole::Border);
    }

    for node in &mut graph.nodes {
        if !node.drawn {
            if let (Some(drawing), Some(coord)) = (&node.drawing, node.drawing_coord) {
                graph.canvas.merge(coord, use_ascii, &[drawing]);
                fill_roles_for_node_box(&mut graph.role_canvas, drawing, coord);
                node.drawn = true;
            }
        }
    }

    let mut lines = Vec::new();
    let mut corners = Vec::new();
    let mut arrow_ends = Vec::new();
    let mut arrow_starts = Vec::new();
    let mut box_starts = Vec::new();
    let mut labels = Vec::new();
    let mut junctions = Vec::new();
    let mut processed_bundles = HashSet::new();

    for edge in &graph.edges {
        if let Some(bundle_id) = edge.bundle {
            let Some(bundle) = graph.bundles.get(bundle_id) else {
                continue;
            };
            let [path, box_start, _, _, edge_corners, label] =
                draw_bundled_edge_segment(graph, edge, bundle);
            lines.push(path);
            corners.push(edge_corners);
            box_starts.push(box_start);
            labels.push(label);
            if processed_bundles.insert(bundle_id) {
                let (shared_path, shared_corners) = draw_bundle_shared_path(graph, bundle);
                lines.push(shared_path);
                corners.push(shared_corners);
                if bundle.kind == BundleKind::FanIn {
                    arrow_ends.push(draw_bundle_arrowhead(
                        graph,
                        &bundle.shared_path,
                        bundle.shared_node,
                    ));
                }
                junctions.push(draw_junction_character(graph, bundle));
            }
            if bundle.kind == BundleKind::FanOut && edge.has_arrow_end {
                arrow_ends.push(draw_bundle_arrowhead(
                    graph,
                    &edge.path_to_junction,
                    edge.to,
                ));
            }
        } else {
            let [path, box_start, arrow_end, arrow_start, edge_corners, label] =
                draw_arrow(graph, edge);
            lines.push(path);
            corners.push(edge_corners);
            arrow_ends.push(arrow_end);
            arrow_starts.push(arrow_start);
            box_starts.push(box_start);
            labels.push(label);
        }
    }

    // Edge layers composite by kind, each kind over every edge in order.
    for (layers, role) in [
        (&lines, CharRole::Line),
        (&corners, CharRole::Corner),
        (&junctions, CharRole::Junction),
        (&arrow_ends, CharRole::Arrow),
        (&box_starts, CharRole::Junction),
        (&arrow_starts, CharRole::Arrow),
        (&labels, CharRole::Text),
    ] {
        graph.canvas.overlay_layers(use_ascii, layers);
        fill_roles_from_layers(&mut graph.role_canvas, layers, role);
    }

    for sg in 0..graph.subgraphs.len() {
        if graph.subgraphs[sg].nodes.is_empty() {
            continue;
        }
        let (label, offset) = draw_subgraph_label(graph, sg);
        graph.canvas.merge(offset, use_ascii, &[&label]);
        fill_roles_from_canvas(&mut graph.role_canvas, &label, offset, CharRole::Text);
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::mermaid::flowchart::{AsciiConfig, LayoutDirection};

    fn rows(canvas: &Canvas) -> Vec<String> {
        canvas
            .to_plain_string()
            .split('\n')
            .map(str::to_owned)
            .collect()
    }

    #[test]
    fn multi_box_matches_two_and_three_section_outputs() {
        let draw_multi_box = |sections: &[Vec<String>], use_ascii: bool| {
            let (width, height) = multi_box_size(sections);
            let mut canvas = Canvas::new(width, height);
            let mut roles = RoleCanvas::new(width, height);
            stamp_multi_box(&mut canvas, &mut roles, sections, 0, 0, use_ascii);
            canvas
        };
        let two = vec![vec!["CUSTOMER".into()], vec!["+id: int".into()]];
        assert_eq!(
            rows(&draw_multi_box(&two, false)),
            [
                "┌──────────┐",
                "│ CUSTOMER │",
                "├──────────┤",
                "│ +id: int │",
                "└──────────┘"
            ]
        );
        assert_eq!(
            rows(&draw_multi_box(&two, true)),
            [
                "+----------+",
                "| CUSTOMER |",
                "+----------+",
                "| +id: int |",
                "+----------+"
            ]
        );

        let three = vec![
            vec!["Animal".into()],
            vec!["+name: String".into()],
            vec!["+eat: void".into()],
        ];
        assert_eq!(
            rows(&draw_multi_box(&three, false)),
            [
                "┌───────────────┐",
                "│ Animal        │",
                "├───────────────┤",
                "│ +name: String │",
                "├───────────────┤",
                "│ +eat: void    │",
                "└───────────────┘",
            ]
        );
        assert_eq!(
            rows(&draw_multi_box(&three, true)),
            [
                "+---------------+",
                "| Animal        |",
                "+---------------+",
                "| +name: String |",
                "+---------------+",
                "| +eat: void    |",
                "+---------------+",
            ]
        );
    }

    #[test]
    fn line_styles_draw_exact_horizontal_vertical_and_bend_rows_with_roles() {
        for (style, unicode_h, unicode_v, ascii_h, ascii_v) in [
            (EdgeStyle::Solid, '─', '│', '-', '|'),
            (EdgeStyle::Dotted, '┄', '┆', '.', ':'),
            (EdgeStyle::Thick, '━', '┃', '=', '‖'),
        ] {
            for (use_ascii, horizontal, vertical) in
                [(false, unicode_h, unicode_v), (true, ascii_h, ascii_v)]
            {
                let mut horizontal_canvas = Canvas::new(6, 4);
                draw_line(
                    &mut horizontal_canvas,
                    DrawingCoord::new(1, 1),
                    DrawingCoord::new(4, 1),
                    0,
                    0,
                    use_ascii,
                    style,
                );
                assert_eq!(
                    rows(&horizontal_canvas)[1],
                    format!(" {horizontal}{horizontal}{horizontal}{horizontal} ")
                );

                let mut vertical_canvas = Canvas::new(6, 4);
                draw_line(
                    &mut vertical_canvas,
                    DrawingCoord::new(2, 0),
                    DrawingCoord::new(2, 3),
                    0,
                    0,
                    use_ascii,
                    style,
                );
                assert_eq!(rows(&vertical_canvas), vec![format!("  {vertical}   "); 4]);

                let mut bend = Canvas::new(6, 4);
                draw_line(
                    &mut bend,
                    DrawingCoord::new(1, 1),
                    DrawingCoord::new(4, 3),
                    0,
                    0,
                    use_ascii,
                    style,
                );
                assert_eq!(
                    rows(&bend),
                    [
                        "      ".to_owned(),
                        format!(" {horizontal}{horizontal}{horizontal}{horizontal} "),
                        format!("    {vertical} "),
                        format!("    {vertical} "),
                    ]
                );
                let mut roles = RoleCanvas::new(1, 1);
                fill_roles_from_canvas(&mut roles, &bend, DrawingCoord::new(0, 0), CharRole::Line);
                assert_eq!(roles.get(1, 1), Some(&Some(CharRole::Line)));
                assert_eq!(roles.get(4, 3), Some(&Some(CharRole::Line)));
            }
        }
    }

    fn empty_graph(use_ascii: bool) -> AsciiGraph {
        AsciiGraph {
            nodes: Vec::new(),
            edges: Vec::new(),
            canvas: Canvas::new(5, 5),
            role_canvas: RoleCanvas::new(5, 5),
            grid: HashMap::new(),
            column_width: HashMap::new(),
            row_height: HashMap::new(),
            subgraphs: Vec::new(),
            config: AsciiConfig {
                use_ascii,
                padding_x: 0,
                padding_y: 0,
                box_border_padding: 1,
                direction: LayoutDirection::TD,
            },
            offset_x: 0,
            offset_y: 0,
            bundles: Vec::new(),
        }
    }

    #[test]
    fn arrowhead_glyphs_match_every_direction_and_mode() {
        let directions = [
            (Dir::Up, DrawingCoord::new(2, 1), '▲', '^'),
            (Dir::Down, DrawingCoord::new(2, 3), '▼', 'v'),
            (Dir::Left, DrawingCoord::new(1, 2), '◄', '<'),
            (Dir::Right, DrawingCoord::new(3, 2), '►', '>'),
            (Dir::UpperRight, DrawingCoord::new(3, 1), '◥', '*'),
            (Dir::UpperLeft, DrawingCoord::new(1, 1), '◤', '*'),
            (Dir::LowerRight, DrawingCoord::new(3, 3), '◢', '*'),
            (Dir::LowerLeft, DrawingCoord::new(1, 3), '◣', '*'),
        ];
        for (direction, end, unicode, ascii) in directions {
            for (use_ascii, expected) in [(false, unicode), (true, ascii)] {
                let graph = empty_graph(use_ascii);
                let layer = draw_arrow_head(&graph, &[DrawingCoord::new(2, 2), end], direction);
                assert_eq!(layer.cell(end.x, end.y), Some(&Cell::from(expected)));
            }
        }
    }
}
