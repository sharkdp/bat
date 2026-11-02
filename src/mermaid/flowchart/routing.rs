use super::{
    grid::get_node_subgraph,
    pathfinder::{get_path, merge_path},
    AsciiGraph, Dir, EdgeId, GridCoord, LayoutDirection,
};
use crate::mermaid::text::display_width;

/// Grid distance between adjacent ranks (a 3-cell node block plus one gap).
const RANK_PITCH: i32 = 4;

/// Determine the cardinal or diagonal direction from one coordinate to another.
pub const fn determine_direction(from: GridCoord, to: GridCoord) -> Dir {
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

fn self_reference_direction(graph_direction: LayoutDirection) -> (Dir, Dir, Dir, Dir) {
    if graph_direction == LayoutDirection::LR {
        (Dir::Right, Dir::Down, Dir::Down, Dir::Right)
    } else {
        (Dir::Down, Dir::Right, Dir::Right, Dir::Down)
    }
}

/// Return `(preferred start, preferred end, alternative start, alternative
/// end)` directions.
pub fn determine_start_and_end_dir(
    graph: &AsciiGraph,
    edge: EdgeId,
    graph_direction: LayoutDirection,
) -> (Dir, Dir, Dir, Dir) {
    let Some(edge) = graph.edges.get(edge) else {
        return (
            Dir::default(),
            Dir::default(),
            Dir::default(),
            Dir::default(),
        );
    };
    if edge.from == edge.to {
        return self_reference_direction(graph_direction);
    }
    let (Some(from), Some(to)) = (
        graph.nodes.get(edge.from).and_then(|node| node.grid_coord),
        graph.nodes.get(edge.to).and_then(|node| node.grid_coord),
    ) else {
        return (
            Dir::default(),
            Dir::default(),
            Dir::default(),
            Dir::default(),
        );
    };
    let direction = determine_direction(from, to);
    let is_backwards = match graph_direction {
        LayoutDirection::LR => matches!(direction, Dir::Left | Dir::UpperLeft | Dir::LowerLeft),
        LayoutDirection::TD => matches!(direction, Dir::Up | Dir::UpperLeft | Dir::UpperRight),
    };
    // A straight edge that skips over intermediate ranks would otherwise
    // share the in-between nodes' connecting cells; leave and enter on the
    // perpendicular side so it runs beside the chain with its own arrowhead.
    let skips_ranks = match graph_direction {
        LayoutDirection::LR => direction == Dir::Right && to.x - from.x > RANK_PITCH,
        LayoutDirection::TD => direction == Dir::Down && to.y - from.y > RANK_PITCH,
    };
    if skips_ranks {
        return match graph_direction {
            LayoutDirection::LR => (Dir::Down, Dir::Down, Dir::Right, Dir::Left),
            LayoutDirection::TD => (Dir::Right, Dir::Right, Dir::Down, Dir::Up),
        };
    }

    match (direction, graph_direction) {
        (Dir::LowerRight, LayoutDirection::LR) => (Dir::Down, Dir::Left, Dir::Right, Dir::Up),
        (Dir::LowerRight, LayoutDirection::TD) => (Dir::Right, Dir::Up, Dir::Down, Dir::Left),
        (Dir::UpperRight, LayoutDirection::LR) => (Dir::Up, Dir::Left, Dir::Right, Dir::Down),
        (Dir::UpperRight, LayoutDirection::TD) => (Dir::Right, Dir::Down, Dir::Up, Dir::Left),
        (Dir::LowerLeft, LayoutDirection::LR) => (Dir::Down, Dir::Down, Dir::Left, Dir::Up),
        (Dir::LowerLeft, LayoutDirection::TD) => (Dir::Left, Dir::Up, Dir::Down, Dir::Right),
        (Dir::UpperLeft, LayoutDirection::LR) => (Dir::Down, Dir::Down, Dir::Left, Dir::Down),
        (Dir::UpperLeft, LayoutDirection::TD) => (Dir::Right, Dir::Right, Dir::Up, Dir::Right),
        (Dir::Left, LayoutDirection::LR) if is_backwards => {
            (Dir::Down, Dir::Down, Dir::Left, Dir::Right)
        }
        (Dir::Up, LayoutDirection::TD) if is_backwards => {
            (Dir::Right, Dir::Right, Dir::Up, Dir::Down)
        }
        _ => (
            direction,
            direction.opposite(),
            direction,
            direction.opposite(),
        ),
    }
}

/// Route an edge using the shorter of its preferred and alternative A* paths.
pub fn determine_path(graph: &mut AsciiGraph, edge: EdgeId) {
    let Some(edge_data) = graph.edges.get(edge) else {
        return;
    };
    let (from_id, to_id) = (edge_data.from, edge_data.to);
    let source_subgraph = get_node_subgraph(graph, from_id);
    let target_subgraph = get_node_subgraph(graph, to_id);
    let effective_direction = if source_subgraph.is_some() && source_subgraph == target_subgraph {
        source_subgraph
            .and_then(|subgraph| graph.subgraphs.get(subgraph))
            .and_then(|subgraph| subgraph.direction)
            .unwrap_or(graph.config.direction)
    } else {
        graph.config.direction
    };
    let (preferred, preferred_end, alternative, alternative_end) =
        determine_start_and_end_dir(graph, edge, effective_direction);
    let (Some(from), Some(to)) = (
        graph.nodes.get(from_id).and_then(|node| node.grid_coord),
        graph.nodes.get(to_id).and_then(|node| node.grid_coord),
    ) else {
        return;
    };

    let preferred_from = from.offset(preferred);
    let preferred_to = to.offset(preferred_end);
    let alternative_from = from.offset(alternative);
    let alternative_to = to.offset(alternative_end);
    let preferred_path = get_path(&graph.grid, preferred_from, preferred_to).map(merge_path);
    let alternative_path = get_path(&graph.grid, alternative_from, alternative_to).map(merge_path);

    let (start_dir, end_dir, path) = match (preferred_path, alternative_path) {
        (Some(preferred_path), Some(alternative_path)) => {
            if preferred_path.len() <= alternative_path.len() {
                (preferred, preferred_end, preferred_path)
            } else {
                (alternative, alternative_end, alternative_path)
            }
        }
        (Some(path), None) => (preferred, preferred_end, path),
        (None, Some(path)) => (alternative, alternative_end, path),
        (None, None) => (preferred, preferred_end, vec![preferred_from, preferred_to]),
    };

    if let Some(edge) = graph.edges.get_mut(edge) {
        edge.start_dir = start_dir;
        edge.end_dir = end_dir;
        edge.path = path;
    }
}

fn calculate_line_width(graph: &AsciiGraph, line: [GridCoord; 2]) -> i32 {
    let start_x = line[0].x.min(line[1].x);
    let end_x = line[0].x.max(line[1].x);
    (start_x..=end_x)
        .map(|x| graph.column_width.get(&x).copied().unwrap_or(0))
        .sum()
}

/// Choose an edge segment for its label and widen the midpoint column to fit
/// it.
pub fn determine_label_line(graph: &mut AsciiGraph, edge: EdgeId) {
    let Some(edge_data) = graph.edges.get(edge) else {
        return;
    };
    if edge_data.text.is_empty() || edge_data.path.len() < 2 {
        return;
    }
    let label_width = i32::try_from(display_width(&edge_data.text)).unwrap_or(i32::MAX);
    let mut segments = Vec::with_capacity(edge_data.path.len() - 1);
    for index in 1..edge_data.path.len() {
        let line = [edge_data.path[index - 1], edge_data.path[index]];
        segments.push((line, calculate_line_width(graph, line), index));
    }

    let largest_line = segments
        .iter()
        .rev()
        .find(|(_, width, index)| *width >= label_width && *index > 1)
        .or_else(|| {
            segments
                .iter()
                .rev()
                .find(|(_, width, _)| *width >= label_width)
        })
        .or_else(|| {
            let mut widest = segments.first();
            for segment in segments.iter().skip(1) {
                if widest.is_none_or(|candidate| segment.1 > candidate.1) {
                    widest = Some(segment);
                }
            }
            widest
        })
        .map(|(line, ..)| *line);
    let Some(largest_line) = largest_line else {
        return;
    };

    let min_x = largest_line[0].x.min(largest_line[1].x);
    let max_x = largest_line[0].x.max(largest_line[1].x);
    let middle_x = min_x + (max_x - min_x).div_euclid(2);
    let required_width = label_width.saturating_add(2);
    graph
        .column_width
        .entry(middle_x)
        .and_modify(|width| *width = (*width).max(required_width))
        .or_insert(required_width);
    if let Some(edge) = graph.edges.get_mut(edge) {
        edge.label_line = largest_line.to_vec();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn determines_axis_and_diagonal_directions() {
        let origin = GridCoord::new(2, 2);
        assert_eq!(determine_direction(origin, GridCoord::new(2, 1)), Dir::Up);
        assert_eq!(determine_direction(origin, GridCoord::new(2, 3)), Dir::Down);
        assert_eq!(determine_direction(origin, GridCoord::new(1, 2)), Dir::Left);
        assert_eq!(
            determine_direction(origin, GridCoord::new(3, 2)),
            Dir::Right
        );
        assert_eq!(
            determine_direction(origin, GridCoord::new(3, 3)),
            Dir::LowerRight
        );
    }
}
