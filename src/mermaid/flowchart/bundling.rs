use super::{
    grid::get_node_subgraph,
    pathfinder::{get_path, merge_path},
    AsciiGraph, BundleId, BundleKind, Dir, EdgeBundle, EdgeId, GridCoord, LayoutDirection,
};

fn can_bundle(edges: &[EdgeId], graph: &AsciiGraph) -> bool {
    let Some(first) = edges.first().and_then(|&edge| graph.edges.get(edge)) else {
        return false;
    };
    if edges.len() < 2 {
        return false;
    }
    let first_from_subgraph = get_node_subgraph(graph, first.from);
    let first_to_subgraph = get_node_subgraph(graph, first.to);

    edges.iter().all(|&edge| {
        let Some(edge) = graph.edges.get(edge) else {
            return false;
        };
        let from_subgraph = get_node_subgraph(graph, edge.from);
        let to_subgraph = get_node_subgraph(graph, edge.to);
        edge.style == first.style
            && edge.text.is_empty()
            && from_subgraph == first_from_subgraph
            && to_subgraph == first_to_subgraph
            && from_subgraph == to_subgraph
    })
}

fn push_group(groups: &mut Vec<(usize, Vec<EdgeId>)>, node: usize, edge: EdgeId) {
    if let Some((_, edges)) = groups.iter_mut().find(|(candidate, _)| *candidate == node) {
        edges.push(edge);
    } else {
        groups.push((node, vec![edge]));
    }
}

/// Find fan-in and fan-out groups, fill `graph.bundles`, and mark their edges.
pub fn analyze_edge_bundles(graph: &mut AsciiGraph) {
    graph.bundles.clear();
    for edge in &mut graph.edges {
        edge.bundle = None;
    }
    if graph.config.direction != LayoutDirection::TD {
        return;
    }

    let mut bundled = vec![false; graph.edges.len()];
    let mut edges_by_target = Vec::new();
    for (edge_id, edge) in graph.edges.iter().enumerate() {
        if edge.from != edge.to {
            push_group(&mut edges_by_target, edge.to, edge_id);
        }
    }

    for (target, edges) in edges_by_target {
        if !can_bundle(&edges, graph) || edges.iter().any(|&edge| bundled[edge]) {
            continue;
        }
        let bundle_id = graph.bundles.len();
        let other_nodes = edges.iter().map(|&edge| graph.edges[edge].from).collect();
        graph.bundles.push(EdgeBundle {
            kind: BundleKind::FanIn,
            edges: edges.clone(),
            shared_node: target,
            other_nodes,
            junction_point: None,
            shared_path: Vec::new(),
            junction_dir: Dir::Middle,
            shared_node_dir: Dir::Middle,
        });
        for edge in edges {
            graph.edges[edge].bundle = Some(bundle_id);
            bundled[edge] = true;
        }
    }

    let mut edges_by_source = Vec::new();
    for (edge_id, edge) in graph.edges.iter().enumerate() {
        if edge.from != edge.to && !bundled[edge_id] {
            push_group(&mut edges_by_source, edge.from, edge_id);
        }
    }

    for (source, edges) in edges_by_source {
        if !can_bundle(&edges, graph) {
            continue;
        }
        let bundle_id = graph.bundles.len();
        let other_nodes = edges.iter().map(|&edge| graph.edges[edge].to).collect();
        graph.bundles.push(EdgeBundle {
            kind: BundleKind::FanOut,
            edges: edges.clone(),
            shared_node: source,
            other_nodes,
            junction_point: None,
            shared_path: Vec::new(),
            junction_dir: Dir::Middle,
            shared_node_dir: Dir::Middle,
        });
        for edge in edges {
            graph.edges[edge].bundle = Some(bundle_id);
            bundled[edge] = true;
        }
    }
}

/// Calculate the merge or split coordinate for one bundle.
pub fn calculate_junction_point(graph: &AsciiGraph, bundle: BundleId) -> GridCoord {
    let Some(bundle) = graph.bundles.get(bundle) else {
        return GridCoord::default();
    };
    let Some(shared_coord) = graph
        .nodes
        .get(bundle.shared_node)
        .and_then(|node| node.grid_coord)
    else {
        return GridCoord::default();
    };

    match (bundle.kind, graph.config.direction) {
        (BundleKind::FanIn, LayoutDirection::TD) => {
            GridCoord::new(shared_coord.x + 1, shared_coord.y - 1)
        }
        (BundleKind::FanIn, LayoutDirection::LR) => {
            GridCoord::new(shared_coord.x - 1, shared_coord.y + 1)
        }
        (BundleKind::FanOut, LayoutDirection::TD) => {
            GridCoord::new(shared_coord.x + 1, shared_coord.y + 3)
        }
        (BundleKind::FanOut, LayoutDirection::LR) => {
            GridCoord::new(shared_coord.x + 3, shared_coord.y + 1)
        }
    }
}

/// Route every edge in a bundle through its shared junction.
pub fn route_bundled_edges(graph: &mut AsciiGraph, bundle: BundleId) {
    let Some(bundle_data) = graph.bundles.get(bundle) else {
        return;
    };
    let kind = bundle_data.kind;
    let shared_node = bundle_data.shared_node;
    let edge_ids = bundle_data.edges.clone();
    let direction = graph.config.direction;
    let junction = calculate_junction_point(graph, bundle);
    let Some(shared_coord) = graph
        .nodes
        .get(shared_node)
        .and_then(|node| node.grid_coord)
    else {
        return;
    };

    match kind {
        BundleKind::FanIn => {
            let junction_dir = if direction == LayoutDirection::TD {
                Dir::Up
            } else {
                Dir::Left
            };
            let shared_node_dir = if direction == LayoutDirection::TD {
                Dir::Down
            } else {
                Dir::Right
            };
            let target_entry = if direction == LayoutDirection::TD {
                GridCoord::new(shared_coord.x + 1, shared_coord.y)
            } else {
                GridCoord::new(shared_coord.x, shared_coord.y + 1)
            };
            let shared_path = get_path(&graph.grid, junction, target_entry)
                .map_or_else(|| vec![junction, target_entry], merge_path);

            if let Some(bundle) = graph.bundles.get_mut(bundle) {
                bundle.junction_point = Some(junction);
                bundle.junction_dir = junction_dir;
                bundle.shared_node_dir = shared_node_dir;
                bundle.shared_path.clone_from(&shared_path);
            }

            for edge_id in edge_ids {
                let Some(source_coord) = graph
                    .edges
                    .get(edge_id)
                    .and_then(|edge| graph.nodes.get(edge.from))
                    .and_then(|node| node.grid_coord)
                else {
                    continue;
                };
                let source_exit = if direction == LayoutDirection::TD {
                    GridCoord::new(source_coord.x + 1, source_coord.y + 2)
                } else {
                    GridCoord::new(source_coord.x + 2, source_coord.y + 1)
                };
                let path_to_junction = get_path(&graph.grid, source_exit, junction)
                    .map_or_else(|| vec![source_exit, junction], merge_path);
                let mut path = path_to_junction.clone();
                path.extend_from_slice(shared_path.get(1..).unwrap_or_default());
                if let Some(edge) = graph.edges.get_mut(edge_id) {
                    edge.path_to_junction = path_to_junction;
                    edge.start_dir = if direction == LayoutDirection::TD {
                        Dir::Down
                    } else {
                        Dir::Right
                    };
                    edge.end_dir = if direction == LayoutDirection::TD {
                        Dir::Up
                    } else {
                        Dir::Left
                    };
                    edge.path = path;
                }
            }
        }
        BundleKind::FanOut => {
            let junction_dir = if direction == LayoutDirection::TD {
                Dir::Down
            } else {
                Dir::Right
            };
            let shared_node_dir = if direction == LayoutDirection::TD {
                Dir::Up
            } else {
                Dir::Left
            };
            let source_exit = if direction == LayoutDirection::TD {
                GridCoord::new(shared_coord.x + 1, shared_coord.y + 2)
            } else {
                GridCoord::new(shared_coord.x + 2, shared_coord.y + 1)
            };
            let shared_path = get_path(&graph.grid, source_exit, junction)
                .map_or_else(|| vec![source_exit, junction], merge_path);

            if let Some(bundle) = graph.bundles.get_mut(bundle) {
                bundle.junction_point = Some(junction);
                bundle.junction_dir = junction_dir;
                bundle.shared_node_dir = shared_node_dir;
                bundle.shared_path.clone_from(&shared_path);
            }

            for edge_id in edge_ids {
                let Some(target_coord) = graph
                    .edges
                    .get(edge_id)
                    .and_then(|edge| graph.nodes.get(edge.to))
                    .and_then(|node| node.grid_coord)
                else {
                    continue;
                };
                let target_entry = if direction == LayoutDirection::TD {
                    GridCoord::new(target_coord.x + 1, target_coord.y)
                } else {
                    GridCoord::new(target_coord.x, target_coord.y + 1)
                };
                let path_to_junction = get_path(&graph.grid, junction, target_entry)
                    .map_or_else(|| vec![junction, target_entry], merge_path);
                let mut path = shared_path.clone();
                path.extend_from_slice(path_to_junction.get(1..).unwrap_or_default());
                if let Some(edge) = graph.edges.get_mut(edge_id) {
                    edge.path_to_junction = path_to_junction;
                    edge.start_dir = if direction == LayoutDirection::TD {
                        Dir::Down
                    } else {
                        Dir::Right
                    };
                    edge.end_dir = if direction == LayoutDirection::TD {
                        Dir::Up
                    } else {
                        Dir::Left
                    };
                    edge.path = path;
                }
            }
        }
    }
}

/// Calculate junctions and route every bundle in insertion order.
pub fn process_bundles(graph: &mut AsciiGraph) {
    for bundle in 0..graph.bundles.len() {
        route_bundled_edges(graph, bundle);
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::mermaid::{
        canvas::{Canvas, RoleCanvas},
        flowchart::{AsciiConfig, AsciiEdge, AsciiNode, EdgeStyle, NodeShape},
    };

    fn graph(direction: LayoutDirection) -> AsciiGraph {
        let nodes = [("A", 0, 0), ("B", 3, 0), ("C", 1, 6)]
            .into_iter()
            .enumerate()
            .map(|(index, (name, x, y))| AsciiNode {
                name: name.into(),
                label: name.into(),
                shape: NodeShape::Rectangle,
                index,
                grid_coord: Some(GridCoord::new(x, y)),
                drawing_coord: None,
                drawing: None,
                drawn: false,
            })
            .collect();
        let edge = |from, to| AsciiEdge {
            from,
            to,
            text: String::new(),
            path: Vec::new(),
            label_line: Vec::new(),
            start_dir: Dir::default(),
            end_dir: Dir::default(),
            style: EdgeStyle::Solid,
            has_arrow_start: false,
            has_arrow_end: true,
            bundle: None,
            path_to_junction: Vec::new(),
        };
        AsciiGraph {
            nodes,
            edges: vec![edge(0, 2), edge(1, 2)],
            canvas: Canvas::new(1, 1),
            role_canvas: RoleCanvas::new(1, 1),
            grid: HashMap::new(),
            column_width: HashMap::new(),
            row_height: HashMap::new(),
            subgraphs: Vec::new(),
            config: AsciiConfig {
                use_ascii: false,
                padding_x: 1,
                padding_y: 1,
                box_border_padding: 1,
                direction,
            },
            offset_x: 0,
            offset_y: 0,
            bundles: Vec::new(),
        }
    }

    #[test]
    fn groups_two_sources_into_one_top_down_fan_in() {
        let mut graph = graph(LayoutDirection::TD);
        analyze_edge_bundles(&mut graph);
        assert_eq!(graph.bundles.len(), 1);
        assert_eq!(graph.bundles[0].kind, BundleKind::FanIn);
        assert_eq!(graph.bundles[0].edges, vec![0, 1]);
        assert_eq!(graph.edges[0].bundle, Some(0));
        assert_eq!(graph.edges[1].bundle, Some(0));
    }

    #[test]
    fn does_not_bundle_left_to_right_edges() {
        let mut graph = graph(LayoutDirection::LR);
        analyze_edge_bundles(&mut graph);
        assert!(graph.bundles.is_empty());
        assert!(graph.edges.iter().all(|edge| edge.bundle.is_none()));
    }
}
