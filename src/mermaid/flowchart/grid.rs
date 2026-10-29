//! Logical-grid placement and conversion to character-space coordinates.

use super::{
    bundling, draw, layering, routing,
    shapes::{self, ShapeRenderOptions},
    AsciiGraph, GridCoord, LayoutDirection, NodeId, SubgraphId,
};
use crate::mermaid::canvas::DrawingCoord;

/// Convert a logical grid position to the center of its character-space cell.
pub fn grid_to_drawing_coord(graph: &AsciiGraph, target: GridCoord) -> DrawingCoord {
    let x = (0..target.x)
        .map(|column| graph.column_width.get(&column).copied().unwrap_or(0))
        .sum::<i32>();
    let y = (0..target.y)
        .map(|row| graph.row_height.get(&row).copied().unwrap_or(0))
        .sum::<i32>();
    let column_width = graph.column_width.get(&target.x).copied().unwrap_or(0);
    let row_height = graph.row_height.get(&target.y).copied().unwrap_or(0);
    DrawingCoord::new(
        x + column_width.div_euclid(2) + graph.offset_x,
        y + row_height.div_euclid(2) + graph.offset_y,
    )
}

/// Convert every logical point in an edge path to character-space coordinates.
pub fn line_to_drawing(graph: &AsciiGraph, line: &[GridCoord]) -> Vec<DrawingCoord> {
    line.iter()
        .map(|&coordinate| grid_to_drawing_coord(graph, coordinate))
        .collect()
}

/// Reserve a node's 3×3 grid block, shifting perpendicular to flow on
/// collision.
pub fn reserve_spot_in_grid(
    graph: &mut AsciiGraph,
    node: NodeId,
    mut requested: GridCoord,
) -> GridCoord {
    let direction = graph.config.direction;
    while graph.grid.contains_key(&requested) {
        match direction {
            LayoutDirection::LR => requested.y += 4,
            LayoutDirection::TD => requested.x += 4,
        }
    }

    for dx in 0..3 {
        for dy in 0..3 {
            graph
                .grid
                .insert(GridCoord::new(requested.x + dx, requested.y + dy), node);
        }
    }
    if let Some(ascii_node) = graph.nodes.get_mut(node) {
        ascii_node.grid_coord = Some(requested);
    }
    requested
}

/// Expand the three grid columns and rows occupied by a node to fit its shape.
pub fn set_column_width(graph: &mut AsciiGraph, node: NodeId) {
    let Some(ascii_node) = graph.nodes.get(node) else {
        return;
    };
    let Some(grid_coord) = ascii_node.grid_coord else {
        return;
    };
    let dimensions = shapes::shape_dimensions(
        ascii_node.shape,
        &ascii_node.label,
        &ShapeRenderOptions {
            use_ascii: graph.config.use_ascii,
            padding: graph.config.box_border_padding,
        },
    );
    let needs_subgraph_overhead = has_incoming_edge_from_outside_subgraph(graph, node);

    for (offset, width) in dimensions.grid_columns.into_iter().enumerate() {
        let coordinate = grid_coord.x + offset as i32;
        graph
            .column_width
            .entry(coordinate)
            .and_modify(|current| *current = (*current).max(width))
            .or_insert(width);
    }
    for (offset, height) in dimensions.grid_rows.into_iter().enumerate() {
        let coordinate = grid_coord.y + offset as i32;
        graph
            .row_height
            .entry(coordinate)
            .and_modify(|current| *current = (*current).max(height))
            .or_insert(height);
    }

    if grid_coord.x > 0 {
        let padding = graph.config.padding_x;
        graph
            .column_width
            .entry(grid_coord.x - 1)
            .and_modify(|current| *current = (*current).max(padding))
            .or_insert(padding);
    }
    if grid_coord.y > 0 {
        let padding = graph.config.padding_y + if needs_subgraph_overhead { 4 } else { 0 };
        graph
            .row_height
            .entry(grid_coord.y - 1)
            .and_modify(|current| *current = (*current).max(padding))
            .or_insert(padding);
    }
}

/// Add default dimensions for grid cells introduced only by an edge path.
pub fn increase_grid_size_for_path(graph: &mut AsciiGraph, path: &[GridCoord]) {
    for coordinate in path {
        graph
            .column_width
            .entry(coordinate.x)
            .or_insert_with(|| graph.config.padding_x.div_euclid(2));
        graph
            .row_height
            .entry(coordinate.y)
            .or_insert_with(|| graph.config.padding_y.div_euclid(2));
    }
}

/// Return the deepest subgraph that directly or transitively contains a node.
pub fn get_node_subgraph(graph: &AsciiGraph, node: NodeId) -> Option<SubgraphId> {
    let mut innermost = None;
    for (subgraph_id, subgraph) in graph.subgraphs.iter().enumerate() {
        if subgraph.nodes.contains(&node)
            && innermost.is_none_or(|current| is_ancestor_or_self(graph, current, subgraph_id))
        {
            innermost = Some(subgraph_id);
        }
    }
    innermost
}

fn is_ancestor_or_self(graph: &AsciiGraph, candidate: SubgraphId, target: SubgraphId) -> bool {
    let mut current = Some(target);
    while let Some(subgraph_id) = current {
        if subgraph_id == candidate {
            return true;
        }
        current = graph
            .subgraphs
            .get(subgraph_id)
            .and_then(|subgraph| subgraph.parent);
    }
    false
}

/// Return the node's innermost direction override or the graph direction.
pub fn effective_direction(graph: &AsciiGraph, node: NodeId) -> LayoutDirection {
    get_node_subgraph(graph, node)
        .and_then(|subgraph| graph.subgraphs.get(subgraph)?.direction)
        .unwrap_or(graph.config.direction)
}

fn has_incoming_edge_from_outside_subgraph(graph: &AsciiGraph, node: NodeId) -> bool {
    let Some(node_subgraph) = get_node_subgraph(graph, node) else {
        return false;
    };
    let has_external_edge = graph
        .edges
        .iter()
        .any(|edge| edge.to == node && get_node_subgraph(graph, edge.from) != Some(node_subgraph));
    if !has_external_edge {
        return false;
    }

    let Some(node_y) = graph
        .nodes
        .get(node)
        .and_then(|node| node.grid_coord)
        .map(|coord| coord.y)
    else {
        return true;
    };
    for &other_node in &graph.subgraphs[node_subgraph].nodes {
        if other_node == node {
            continue;
        }
        let Some(other_y) = graph
            .nodes
            .get(other_node)
            .and_then(|other| other.grid_coord)
            .map(|coord| coord.y)
        else {
            continue;
        };
        let other_has_external_edge = graph.edges.iter().any(|edge| {
            edge.to == other_node && get_node_subgraph(graph, edge.from) != Some(node_subgraph)
        });
        if other_has_external_edge && other_y < node_y {
            return false;
        }
    }
    true
}

fn calculate_subgraph_bounding_box(graph: &mut AsciiGraph, subgraph_id: SubgraphId) {
    let Some(subgraph) = graph.subgraphs.get(subgraph_id) else {
        return;
    };
    if subgraph.nodes.is_empty() {
        return;
    }
    let children = subgraph.children.clone();
    for child in &children {
        calculate_subgraph_bounding_box(graph, *child);
    }

    let mut min_x = 1_000_000;
    let mut min_y = 1_000_000;
    let mut max_x = -1_000_000;
    let mut max_y = -1_000_000;
    for child in children {
        let Some(child) = graph.subgraphs.get(child) else {
            continue;
        };
        if !child.nodes.is_empty() {
            min_x = min_x.min(child.min_x);
            min_y = min_y.min(child.min_y);
            max_x = max_x.max(child.max_x);
            max_y = max_y.max(child.max_y);
        }
    }

    let nodes = graph.subgraphs[subgraph_id].nodes.clone();
    for node in nodes {
        let Some(node) = graph.nodes.get(node) else {
            continue;
        };
        let (Some(coordinate), Some(drawing)) = (node.drawing_coord, node.drawing.as_ref()) else {
            continue;
        };
        min_x = min_x.min(coordinate.x);
        min_y = min_y.min(coordinate.y);
        max_x = max_x.max(coordinate.x + drawing.width() - 1);
        max_y = max_y.max(coordinate.y + drawing.height() - 1);
    }

    let subgraph_padding = 2;
    let subgraph_label_space = 2;
    let subgraph = &mut graph.subgraphs[subgraph_id];
    subgraph.min_x = min_x - subgraph_padding;
    subgraph.min_y = min_y - subgraph_padding - subgraph_label_space;
    subgraph.max_x = max_x + subgraph_padding;
    subgraph.max_y = max_y + subgraph_padding;
}

fn ensure_subgraph_spacing(graph: &mut AsciiGraph) {
    let roots: Vec<_> = graph
        .subgraphs
        .iter()
        .enumerate()
        .filter_map(|(id, subgraph)| {
            (subgraph.parent.is_none() && !subgraph.nodes.is_empty()).then_some(id)
        })
        .collect();
    let minimum_spacing = 1;
    for (position, &left_id) in roots.iter().enumerate() {
        for &right_id in &roots[position + 1..] {
            let (left, right) = if left_id < right_id {
                let (before_right, from_right) = graph.subgraphs.split_at_mut(right_id);
                (&mut before_right[left_id], &mut from_right[0])
            } else {
                let (before_left, from_left) = graph.subgraphs.split_at_mut(left_id);
                (&mut from_left[0], &mut before_left[right_id])
            };

            if left.min_x < right.max_x && left.max_x > right.min_x {
                if left.max_y >= right.min_y - minimum_spacing && left.min_y < right.min_y {
                    right.min_y = left.max_y + minimum_spacing + 1;
                } else if right.max_y >= left.min_y - minimum_spacing && right.min_y < left.min_y {
                    left.min_y = right.max_y + minimum_spacing + 1;
                }
            }
            if left.min_y < right.max_y && left.max_y > right.min_y {
                if left.max_x >= right.min_x - minimum_spacing && left.min_x < right.min_x {
                    right.min_x = left.max_x + minimum_spacing + 1;
                } else if right.max_x >= left.min_x - minimum_spacing && right.min_x < left.min_x {
                    left.min_x = right.max_x + minimum_spacing + 1;
                }
            }
        }
    }
}

/// Compute every subgraph's drawing-space bounding box and root spacing.
pub fn calculate_subgraph_bounding_boxes(graph: &mut AsciiGraph) {
    for subgraph_id in 0..graph.subgraphs.len() {
        calculate_subgraph_bounding_box(graph, subgraph_id);
    }
    ensure_subgraph_spacing(graph);
}

/// Shift subgraph and node coordinates so no subgraph border is negative.
pub fn offset_drawing_for_subgraphs(graph: &mut AsciiGraph) {
    if graph.subgraphs.is_empty() {
        return;
    }
    let min_x = graph
        .subgraphs
        .iter()
        .fold(0, |minimum, subgraph| minimum.min(subgraph.min_x));
    let min_y = graph
        .subgraphs
        .iter()
        .fold(0, |minimum, subgraph| minimum.min(subgraph.min_y));
    let offset_x = -min_x;
    let offset_y = -min_y;
    if offset_x == 0 && offset_y == 0 {
        return;
    }

    graph.offset_x = offset_x;
    graph.offset_y = offset_y;
    for subgraph in &mut graph.subgraphs {
        subgraph.min_x += offset_x;
        subgraph.min_y += offset_y;
        subgraph.max_x += offset_x;
        subgraph.max_y += offset_y;
    }
    for node in &mut graph.nodes {
        if let Some(coordinate) = &mut node.drawing_coord {
            coordinate.x += offset_x;
            coordinate.y += offset_y;
        }
    }
    graph.canvas.ensure_size(
        graph.canvas.width() + offset_x,
        graph.canvas.height() + offset_y,
    );
    graph.role_canvas.ensure_size(
        graph.role_canvas.width() + offset_x,
        graph.role_canvas.height() + offset_y,
    );
}

/// Perform node placement, edge routing, drawing-coordinate mapping, and
/// subgraph sizing.
pub fn create_mapping(graph: &mut AsciiGraph) {
    let direction = graph.config.direction;
    let layering = layering::layer(graph);
    for row in &layering.order {
        for &node in row {
            let (level, position) = (
                layering.rank[node] as i32 * 4,
                layering.slot[node] as i32 * 4,
            );
            let requested = match direction {
                LayoutDirection::LR => GridCoord::new(level, position),
                LayoutDirection::TD => GridCoord::new(position, level),
            };
            reserve_spot_in_grid(graph, node, requested);
        }
    }

    for node in 0..graph.nodes.len() {
        set_column_width(graph, node);
    }
    bundling::analyze_edge_bundles(graph);
    bundling::process_bundles(graph);
    for edge in 0..graph.edges.len() {
        let already_routed_bundle =
            graph.edges[edge].bundle.is_some() && !graph.edges[edge].path.is_empty();
        if !already_routed_bundle {
            routing::determine_path(graph, edge);
        }
        let path = graph.edges[edge].path.clone();
        increase_grid_size_for_path(graph, &path);
        routing::determine_label_line(graph, edge);
    }

    for node in 0..graph.nodes.len() {
        if let Some(grid_coordinate) = graph.nodes[node].grid_coord {
            graph.nodes[node].drawing_coord = Some(grid_to_drawing_coord(graph, grid_coordinate));
        }
    }
    let drawings: Vec<_> = (0..graph.nodes.len())
        .map(|node| draw::draw_node(graph, node))
        .collect();
    for (node, drawing) in graph.nodes.iter_mut().zip(drawings) {
        node.drawing = Some(drawing);
    }

    graph.size_canvases_to_grid();
    calculate_subgraph_bounding_boxes(graph);
    offset_drawing_for_subgraphs(graph);
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::mermaid::{
        canvas::{Canvas, RoleCanvas},
        flowchart::{AsciiConfig, AsciiNode, AsciiSubgraph, NodeShape},
    };

    fn node(index: NodeId) -> AsciiNode {
        AsciiNode {
            name: format!("N{index}"),
            label: format!("node {index}"),
            shape: NodeShape::Rectangle,
            index,
            grid_coord: None,
            drawing_coord: None,
            drawing: None,
            drawn: false,
        }
    }

    fn graph(direction: LayoutDirection, node_count: usize) -> AsciiGraph {
        AsciiGraph {
            nodes: (0..node_count).map(node).collect(),
            edges: Vec::new(),
            canvas: Canvas::new(1, 1),
            role_canvas: RoleCanvas::new(1, 1),
            grid: HashMap::new(),
            column_width: HashMap::new(),
            row_height: HashMap::new(),
            subgraphs: Vec::new(),
            config: AsciiConfig {
                use_ascii: false,
                padding_x: 5,
                padding_y: 5,
                box_border_padding: 1,
                direction,
            },
            offset_x: 0,
            offset_y: 0,
            bundles: Vec::new(),
        }
    }

    #[test]
    fn grid_coordinate_uses_prior_sizes_and_centers_target_cell() {
        let mut graph = graph(LayoutDirection::TD, 0);
        graph.column_width.extend([(0, 2), (1, 4), (2, 6)]);
        graph.row_height.extend([(0, 3), (1, 5)]);
        graph.offset_x = 1;
        graph.offset_y = 2;
        assert_eq!(
            grid_to_drawing_coord(&graph, GridCoord::new(2, 1)),
            DrawingCoord::new(10, 7)
        );
    }

    #[test]
    fn collisions_shift_perpendicular_to_each_layout_direction() {
        let mut top_down = graph(LayoutDirection::TD, 2);
        assert_eq!(
            reserve_spot_in_grid(&mut top_down, 0, GridCoord::new(0, 0)),
            GridCoord::new(0, 0)
        );
        assert_eq!(
            reserve_spot_in_grid(&mut top_down, 1, GridCoord::new(0, 0)),
            GridCoord::new(4, 0)
        );

        let mut left_right = graph(LayoutDirection::LR, 2);
        assert_eq!(
            reserve_spot_in_grid(&mut left_right, 0, GridCoord::new(0, 0)),
            GridCoord::new(0, 0)
        );
        assert_eq!(
            reserve_spot_in_grid(&mut left_right, 1, GridCoord::new(0, 0)),
            GridCoord::new(0, 4)
        );
    }

    #[test]
    fn node_subgraph_prefers_innermost_and_direction_falls_back_to_graph() {
        let mut graph = graph(LayoutDirection::TD, 2);
        graph.subgraphs = vec![
            AsciiSubgraph {
                name: "outer".into(),
                nodes: vec![0, 1],
                parent: None,
                children: vec![1],
                min_x: 0,
                min_y: 0,
                max_x: 0,
                max_y: 0,
                direction: Some(LayoutDirection::TD),
            },
            AsciiSubgraph {
                name: "inner".into(),
                nodes: vec![1],
                parent: Some(0),
                children: Vec::new(),
                min_x: 0,
                min_y: 0,
                max_x: 0,
                max_y: 0,
                direction: Some(LayoutDirection::LR),
            },
        ];
        assert_eq!(get_node_subgraph(&graph, 0), Some(0));
        assert_eq!(get_node_subgraph(&graph, 1), Some(1));
        assert_eq!(effective_direction(&graph, 0), LayoutDirection::TD);
        assert_eq!(effective_direction(&graph, 1), LayoutDirection::LR);
    }
}
