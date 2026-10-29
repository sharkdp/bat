//! Conversion from parsed Mermaid flowcharts to renderer-owned graph state.

use std::collections::HashMap;

use super::{
    parser::{MermaidGraph, MermaidSubgraph},
    AsciiConfig, AsciiEdge, AsciiGraph, AsciiNode, AsciiSubgraph, Dir, SubgraphId,
};
use crate::mermaid::{
    canvas::{Canvas, RoleCanvas},
    text::wrap_label,
};

/// Node labels wrap to this many display columns so one long sentence does
/// not stretch its whole column; explicit line breaks are kept.
const LABEL_WRAP_WIDTH: usize = 24;

/// Convert a parsed Mermaid graph into graph state ready for layout.
pub fn convert(parsed: &MermaidGraph, config: AsciiConfig) -> AsciiGraph {
    let nodes = parsed
        .nodes
        .iter()
        .enumerate()
        .map(|(index, node)| AsciiNode {
            name: node.id.clone(),
            label: wrap_label(&node.label, LABEL_WRAP_WIDTH),
            shape: node.shape,
            index,
            grid_coord: None,
            drawing_coord: None,
            drawing: None,
            drawn: false,
        })
        .collect();

    let edges = parsed
        .edges
        .iter()
        .filter_map(|edge| {
            let from = parsed.node_index(&edge.source)?;
            let to = parsed.node_index(&edge.target)?;
            Some(AsciiEdge {
                from,
                to,
                text: edge.label.clone().unwrap_or_default(),
                path: Vec::new(),
                label_line: Vec::new(),
                start_dir: Dir::default(),
                end_dir: Dir::default(),
                style: edge.style,
                has_arrow_start: edge.has_arrow_start,
                has_arrow_end: edge.has_arrow_end,
                bundle: None,
                path_to_junction: Vec::new(),
            })
        })
        .collect();

    let mut subgraphs = Vec::new();
    for subgraph in &parsed.subgraphs {
        convert_subgraph(subgraph, None, parsed, &mut subgraphs);
    }
    deduplicate_subgraph_nodes(&parsed.subgraphs, &mut subgraphs, parsed);

    AsciiGraph {
        nodes,
        edges,
        canvas: Canvas::new(1, 1),
        role_canvas: RoleCanvas::new(1, 1),
        grid: HashMap::new(),
        column_width: HashMap::new(),
        row_height: HashMap::new(),
        subgraphs,
        config,
        offset_x: 0,
        offset_y: 0,
        bundles: Vec::new(),
    }
}

fn convert_subgraph(
    parsed_subgraph: &MermaidSubgraph,
    parent: Option<SubgraphId>,
    parsed: &MermaidGraph,
    all_subgraphs: &mut Vec<AsciiSubgraph>,
) -> SubgraphId {
    let id = all_subgraphs.len();
    let nodes = parsed_subgraph
        .node_ids
        .iter()
        .filter_map(|node_id| parsed.node_index(node_id))
        .collect();
    all_subgraphs.push(AsciiSubgraph {
        name: parsed_subgraph.label.clone(),
        nodes,
        parent,
        children: Vec::new(),
        min_x: 0,
        min_y: 0,
        max_x: 0,
        max_y: 0,
        direction: parsed_subgraph
            .direction
            .map(|direction| direction.layout()),
    });

    for child in &parsed_subgraph.children {
        let child_id = convert_subgraph(child, Some(id), parsed, all_subgraphs);
        all_subgraphs[id].children.push(child_id);
        let child_nodes = all_subgraphs[child_id].nodes.clone();
        for node in child_nodes {
            if !all_subgraphs[id].nodes.contains(&node) {
                all_subgraphs[id].nodes.push(node);
            }
        }
    }

    id
}

fn deduplicate_subgraph_nodes(
    parsed_subgraphs: &[MermaidSubgraph],
    ascii_subgraphs: &mut [AsciiSubgraph],
    parsed: &MermaidGraph,
) {
    let mut flat_parsed = Vec::new();
    build_sg_map(parsed_subgraphs, &mut flat_parsed);

    let mut owners = HashMap::new();
    for (parsed_subgraph, ascii_id) in flat_parsed.iter().copied() {
        claim_nodes(parsed_subgraph, ascii_id, &flat_parsed, &mut owners);
    }

    let parents: Vec<_> = ascii_subgraphs
        .iter()
        .map(|subgraph| subgraph.parent)
        .collect();
    for (subgraph_id, subgraph) in ascii_subgraphs.iter_mut().enumerate() {
        subgraph.nodes.retain(|&node| {
            let Some(node_id) = parsed.nodes.get(node).map(|node| node.id.as_str()) else {
                return false;
            };
            owners
                .get(node_id)
                .is_none_or(|&owner| is_ancestor_or_self(subgraph_id, owner, &parents))
        });
    }
}

fn claim_nodes<'a>(
    parsed_subgraph: &'a MermaidSubgraph,
    ascii_id: SubgraphId,
    mapping: &[(&'a MermaidSubgraph, SubgraphId)],
    owners: &mut HashMap<&'a str, SubgraphId>,
) {
    for child in &parsed_subgraph.children {
        if let Some((_, child_id)) = mapping
            .iter()
            .find(|(candidate, _)| std::ptr::eq(*candidate, child))
        {
            claim_nodes(child, *child_id, mapping, owners);
        }
    }
    for node_id in &parsed_subgraph.node_ids {
        owners.entry(node_id.as_str()).or_insert(ascii_id);
    }
}

fn is_ancestor_or_self(
    candidate: SubgraphId,
    target: SubgraphId,
    parents: &[Option<SubgraphId>],
) -> bool {
    let mut current = Some(target);
    while let Some(id) = current {
        if id == candidate {
            return true;
        }
        current = parents.get(id).copied().flatten();
    }
    false
}

fn build_sg_map<'a>(
    subgraphs: &'a [MermaidSubgraph],
    result: &mut Vec<(&'a MermaidSubgraph, SubgraphId)>,
) {
    for subgraph in subgraphs {
        let id = result.len();
        result.push((subgraph, id));
        build_sg_map(&subgraph.children, result);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mermaid::flowchart::{
        parser::{MermaidEdge, MermaidNode},
        Direction, EdgeStyle, LayoutDirection, NodeShape,
    };

    fn config() -> AsciiConfig {
        AsciiConfig {
            use_ascii: false,
            padding_x: 5,
            padding_y: 5,
            box_border_padding: 1,
            direction: LayoutDirection::TD,
        }
    }

    #[test]
    fn conversion_preserves_order_edges_and_nested_membership_ownership() {
        let parsed = MermaidGraph {
            direction: Direction::TD,
            nodes: vec![
                MermaidNode {
                    id: "A".into(),
                    label: "Alpha".into(),
                    shape: NodeShape::Rectangle,
                },
                MermaidNode {
                    id: "B".into(),
                    label: "Beta".into(),
                    shape: NodeShape::Diamond,
                },
                MermaidNode {
                    id: "C".into(),
                    label: "Gamma".into(),
                    shape: NodeShape::Circle,
                },
            ],
            edges: vec![
                MermaidEdge {
                    source: "A".into(),
                    target: "B".into(),
                    label: Some("first".into()),
                    style: EdgeStyle::Solid,
                    has_arrow_start: false,
                    has_arrow_end: true,
                },
                MermaidEdge {
                    source: "B".into(),
                    target: "C".into(),
                    label: None,
                    style: EdgeStyle::Dotted,
                    has_arrow_start: true,
                    has_arrow_end: true,
                },
            ],
            subgraphs: vec![MermaidSubgraph {
                id: "outer".into(),
                label: "Outer".into(),
                node_ids: vec!["A".into()],
                children: vec![
                    MermaidSubgraph {
                        id: "inner".into(),
                        label: "Inner".into(),
                        node_ids: vec!["B".into()],
                        children: Vec::new(),
                        direction: Some(Direction::LR),
                    },
                    MermaidSubgraph {
                        id: "other".into(),
                        label: "Other".into(),
                        node_ids: vec!["B".into(), "C".into()],
                        children: Vec::new(),
                        direction: None,
                    },
                ],
                direction: Some(Direction::TD),
            }],
            class_defs: Vec::new(),
            class_assignments: Vec::new(),
            node_styles: Vec::new(),
            link_styles: Vec::new(),
        };

        let graph = convert(&parsed, config());
        assert_eq!(
            graph
                .nodes
                .iter()
                .map(|node| node.name.as_str())
                .collect::<Vec<_>>(),
            ["A", "B", "C"]
        );
        assert_eq!((graph.edges[0].from, graph.edges[0].to), (0, 1));
        assert_eq!((graph.edges[1].from, graph.edges[1].to), (1, 2));
        assert_eq!(
            graph
                .subgraphs
                .iter()
                .map(|sg| sg.name.as_str())
                .collect::<Vec<_>>(),
            ["Outer", "Inner", "Other"]
        );
        assert_eq!(graph.subgraphs[0].parent, None);
        assert_eq!(graph.subgraphs[0].children, [1, 2]);
        assert_eq!(graph.subgraphs[1].parent, Some(0));
        assert_eq!(graph.subgraphs[2].parent, Some(0));
        assert_eq!(graph.subgraphs[0].nodes, [0, 1, 2]);
        assert_eq!(graph.subgraphs[1].nodes, [1]);
        assert_eq!(graph.subgraphs[2].nodes, [2]);
    }
}
