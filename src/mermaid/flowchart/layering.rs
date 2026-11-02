//! Rank assignment and in-rank ordering (Sugiyama-style).
//!
//! Ranks come from the longest path over a DFS-derived DAG, so cycles are
//! broken at back edges and a node always sits below every ancestor that
//! reaches it. Within a rank, nodes are ordered by barycenter sweeps to
//! minimize crossings, keeping subgraph members contiguous within each rank.

use std::collections::HashMap;

use super::{grid, AsciiGraph, LayoutDirection, NodeId, SubgraphId};

/// Ranks and cross-axis slots consumed by grid placement.
pub struct Layering {
    /// Longest forward-path distance from a root.
    pub rank: Vec<usize>,
    /// Ordered members of each rank.
    pub order: Vec<Vec<NodeId>>,
    /// Cross-axis node-pitch offsets, relaxed toward neighbouring ranks.
    pub slot: Vec<usize>,
}

/// Rank weight of an edge: `1` when it flows along the graph direction, `0`
/// when both endpoints share a subgraph whose direction runs perpendicular to
/// the graph (the edge then orders nodes within one rank instead).
fn edge_weight(graph: &AsciiGraph, from: NodeId, to: NodeId) -> usize {
    let from_subgraph = grid::get_node_subgraph(graph, from);
    let direction =
        if from_subgraph.is_some() && from_subgraph == grid::get_node_subgraph(graph, to) {
            grid::effective_direction(graph, from)
        } else {
            graph.config.direction
        };
    usize::from(direction == graph.config.direction)
}

/// Subgraph chain of a node from outermost to innermost.
fn subgraph_chain(graph: &AsciiGraph, node: NodeId) -> Vec<SubgraphId> {
    let mut chain = Vec::new();
    let mut current = grid::get_node_subgraph(graph, node);
    while let Some(id) = current {
        chain.push(id);
        current = graph.subgraphs[id].parent;
    }
    chain.reverse();
    chain
}

/// Compute ranks and in-rank order for every node.
pub fn layer(graph: &AsciiGraph) -> Layering {
    let n = graph.nodes.len();
    let mut children: Vec<Vec<(NodeId, usize)>> = vec![Vec::new(); n];
    let mut indegree = vec![0usize; n];
    for edge in &graph.edges {
        if edge.from == edge.to {
            continue;
        }
        children[edge.from].push((edge.to, edge_weight(graph, edge.from, edge.to)));
        indegree[edge.to] += 1;
    }

    // DFS from in-degree-zero roots first (declaration order), then anything
    // left over (pure cycles). Edges to a node still on the stack are back
    // edges and are dropped from the DAG.
    let mut color = vec![0u8; n];
    let mut dag: Vec<Vec<(NodeId, usize)>> = vec![Vec::new(); n];
    let mut postorder: Vec<NodeId> = Vec::with_capacity(n);
    let roots = (0..n).filter(|&i| indegree[i] == 0);
    for start in roots.chain(0..n) {
        if color[start] != 0 {
            continue;
        }
        let mut stack: Vec<(NodeId, usize)> = vec![(start, 0)];
        color[start] = 1;
        while let Some(frame) = stack.last_mut() {
            let u = frame.0;
            if let Some(&(v, w)) = children[u].get(frame.1) {
                frame.1 += 1;
                if color[v] == 1 {
                    continue;
                }
                dag[u].push((v, w));
                if color[v] == 0 {
                    color[v] = 1;
                    stack.push((v, 0));
                }
            } else {
                color[u] = 2;
                postorder.push(u);
                stack.pop();
            }
        }
    }

    let mut rank = vec![0usize; n];
    // LR graphs keep unconnected top-level nodes in the first column and push
    // subgraph-rooted chains one column right so the frame sits beside them.
    if graph.config.direction == LayoutDirection::LR {
        let in_subgraph = |i: NodeId| grid::get_node_subgraph(graph, i).is_some();
        let external_root = (0..n).any(|i| indegree[i] == 0 && !in_subgraph(i));
        let subgraph_root_with_edges =
            (0..n).any(|i| indegree[i] == 0 && in_subgraph(i) && !children[i].is_empty());
        if external_root && subgraph_root_with_edges {
            for i in 0..n {
                if indegree[i] == 0 && in_subgraph(i) {
                    rank[i] = 1;
                }
            }
        }
    }
    for &u in postorder.iter().rev() {
        for &(v, w) in &dag[u] {
            rank[v] = rank[v].max(rank[u] + w);
        }
    }
    // Perpendicular (weight-0) edges order nodes within a rank.
    let mut local = vec![0usize; n];
    for &u in postorder.iter().rev() {
        for &(v, w) in &dag[u] {
            if w == 0 {
                local[v] = local[v].max(local[u] + 1);
            }
        }
    }

    let max_rank = rank.iter().copied().max().unwrap_or(0);
    let mut order: Vec<Vec<NodeId>> = vec![Vec::new(); max_rank + 1];
    for (i, &r) in rank.iter().enumerate() {
        order[r].push(i);
    }

    let chains: Vec<Vec<SubgraphId>> = (0..n).map(|i| subgraph_chain(graph, i)).collect();
    let mut parents: Vec<Vec<NodeId>> = vec![Vec::new(); n];
    let mut successors: Vec<Vec<NodeId>> = vec![Vec::new(); n];
    for (u, list) in dag.iter().enumerate() {
        for &(v, _) in list {
            if rank[v] > rank[u] {
                parents[v].push(u);
                successors[u].push(v);
            }
        }
    }
    order_ranks(&mut order, &rank, &local, &chains, &parents, &successors);
    let slot = assign_slots(&order, &parents, &successors);
    Layering { rank, order, slot }
}

/// Assign a cross-axis slot to every node: iterative barycenter relaxation
/// pulls each node toward the mean slot of its neighbours while ranks keep
/// their order and at least one slot of separation, so chains run straight
/// and a shared child centers under its parents.
fn assign_slots(
    order: &[Vec<NodeId>],
    parents: &[Vec<NodeId>],
    successors: &[Vec<NodeId>],
) -> Vec<usize> {
    let n = parents.len();
    let mut pos = vec![0f64; n];
    for row in order {
        for (i, &v) in row.iter().enumerate() {
            pos[v] = i as f64 * SEP;
        }
    }
    for iteration in 0..10 {
        if iteration % 2 == 0 {
            for row in order {
                relax_row(row, parents, &mut pos);
            }
        } else {
            for row in order.iter().rev() {
                relax_row(row, successors, &mut pos);
            }
        }
    }
    let min = pos.iter().copied().fold(f64::INFINITY, f64::min);
    let min = if min.is_finite() { min } else { 0.0 };
    // Grid slots cannot express a half position, so an exact tie between two
    // neighbours resolves to the left one (round half down); separation of at
    // least one slot survives rounding because it is monotonic.
    pos.iter()
        .map(|&p| (p - min + 0.5 - 1e-9).floor().max(0.0) as usize)
        .collect()
}

/// Minimum slot separation between neighbours in one rank.
const SEP: f64 = 1.0;

fn relax_row(row: &[NodeId], neighbors: &[Vec<NodeId>], pos: &mut [f64]) {
    let n = row.len();
    if n == 0 {
        return;
    }
    let desired: Vec<f64> = row
        .iter()
        .map(|&v| {
            if neighbors[v].is_empty() {
                pos[v]
            } else {
                neighbors[v].iter().map(|&u| pos[u]).sum::<f64>() / neighbors[v].len() as f64
            }
        })
        .collect();
    // Left-to-right pass enforces separation from the left, right-to-left from
    // the right; averaging the two keeps the row centered on its desires.
    let mut left = vec![0f64; n];
    let mut right = vec![0f64; n];
    for i in 0..n {
        left[i] = if i == 0 {
            desired[i]
        } else {
            desired[i].max(left[i - 1] + SEP)
        };
    }
    for i in (0..n).rev() {
        right[i] = if i + 1 == n {
            desired[i]
        } else {
            desired[i].min(right[i + 1] - SEP)
        };
    }
    for i in 0..n {
        pos[row[i]] = left[i].midpoint(right[i]);
    }
    for i in 1..n {
        let min = pos[row[i - 1]] + SEP;
        if pos[row[i]] < min {
            pos[row[i]] = min;
        }
    }
}

fn positions(order: &[Vec<NodeId>], pos: &mut [usize]) {
    for row in order {
        for (i, &v) in row.iter().enumerate() {
            pos[v] = i;
        }
    }
}

fn count_crossings(
    order: &[Vec<NodeId>],
    rank: &[usize],
    successors: &[Vec<NodeId>],
    pos: &[usize],
) -> usize {
    let mut crossings = 0;
    let mut segments = Vec::new();
    for (r, row) in order.iter().enumerate() {
        segments.clear();
        for &u in row {
            for &v in &successors[u] {
                if rank[v] == r + 1 {
                    segments.push((pos[u], pos[v]));
                }
            }
        }
        for (i, a) in segments.iter().enumerate() {
            for b in &segments[i + 1..] {
                if (a.0 < b.0 && a.1 > b.1) || (a.0 > b.0 && a.1 < b.1) {
                    crossings += 1;
                }
            }
        }
    }
    crossings
}

/// Alternate downward and upward barycenter sweeps, keeping the ordering with
/// the fewest crossings seen.
fn order_ranks(
    order: &mut [Vec<NodeId>],
    rank: &[usize],
    local: &[usize],
    chains: &[Vec<SubgraphId>],
    parents: &[Vec<NodeId>],
    successors: &[Vec<NodeId>],
) {
    let n = rank.len();
    let mut pos = vec![0usize; n];
    positions(order, &mut pos);
    // Ranks are stable-sorted once even without sweeps so perpendicular
    // subgraph edges and subgraph contiguity always hold.
    for row in order.iter_mut() {
        sort_row(row, &pos, &[], local, chains);
    }
    positions(order, &mut pos);
    if order.len() < 2 {
        return;
    }

    let mut best: Vec<Vec<NodeId>> = order.to_vec();
    let mut best_crossings = count_crossings(order, rank, successors, &pos);
    for iteration in 0..8 {
        if best_crossings == 0 {
            break;
        }
        if iteration % 2 == 0 {
            for row in order.iter_mut().skip(1) {
                sort_row(row, &pos, parents, local, chains);
                for (i, &v) in row.iter().enumerate() {
                    pos[v] = i;
                }
            }
        } else {
            for row in order.iter_mut().rev().skip(1) {
                sort_row(row, &pos, successors, local, chains);
                for (i, &v) in row.iter().enumerate() {
                    pos[v] = i;
                }
            }
        }
        let crossings = count_crossings(order, rank, successors, &pos);
        if crossings < best_crossings {
            best_crossings = crossings;
            best.clone_from_slice(order);
        }
    }
    order.clone_from_slice(&best);
}

/// Sort one rank by barycenter of `neighbors` (current position when a node
/// has none), keeping each subgraph's members contiguous at every nesting
/// depth and honoring perpendicular-edge order via `local` ranks.
fn sort_row(
    row: &mut [NodeId],
    pos: &[usize],
    neighbors: &[Vec<NodeId>],
    local: &[usize],
    chains: &[Vec<SubgraphId>],
) {
    let key = |v: NodeId| -> f64 {
        match neighbors.get(v) {
            Some(list) if !list.is_empty() => {
                list.iter().map(|&u| pos[u] as f64).sum::<f64>() / list.len() as f64
            }
            _ => pos[v] as f64,
        }
    };
    let keys: HashMap<NodeId, f64> = row.iter().map(|&v| (v, key(v))).collect();
    let mut items: Vec<NodeId> = row.to_vec();
    sort_hierarchical(&mut items, 0, &keys, local, chains);
    row.copy_from_slice(&items);
}

fn sort_hierarchical(
    items: &mut Vec<NodeId>,
    depth: usize,
    keys: &HashMap<NodeId, f64>,
    local: &[usize],
    chains: &[Vec<SubgraphId>],
) {
    // Group consecutive-by-membership: a leaf (no subgraph at this depth) is
    // its own group; nodes sharing a subgraph at this depth form one group.
    let mut groups: Vec<(Option<SubgraphId>, Vec<NodeId>)> = Vec::new();
    for &v in items.iter() {
        let id = chains[v].get(depth).copied();
        match id.and_then(|id| groups.iter_mut().find(|g| g.0 == Some(id))) {
            Some(group) => group.1.push(v),
            None => groups.push((id, vec![v])),
        }
    }
    for (id, members) in &mut groups {
        if id.is_some() && members.len() > 1 {
            sort_hierarchical(members, depth + 1, keys, local, chains);
        }
    }
    let group_key = |members: &Vec<NodeId>| -> (usize, f64) {
        let min_local = members.iter().map(|&v| local[v]).min().unwrap_or(0);
        let mean = members.iter().map(|&v| keys[&v]).sum::<f64>() / members.len() as f64;
        (min_local, mean)
    };
    groups.sort_by(|a, b| {
        let (la, ka) = group_key(&a.1);
        let (lb, kb) = group_key(&b.1);
        la.cmp(&lb).then(ka.total_cmp(&kb))
    });
    items.clear();
    for (_, members) in groups {
        items.extend(members);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mermaid::flowchart::{converter, parser, AsciiConfig};

    fn layered(src: &str) -> (AsciiGraph, Layering) {
        let parsed = parser::parse_flowchart(src).unwrap();
        let config = AsciiConfig {
            use_ascii: false,
            padding_x: 5,
            padding_y: 5,
            box_border_padding: 1,
            direction: parsed.direction.layout(),
        };
        let graph = converter::convert(&parsed, config);
        let layering = layer(&graph);
        (graph, layering)
    }

    fn id(graph: &AsciiGraph, name: &str) -> NodeId {
        graph.nodes.iter().position(|n| n.name == name).unwrap()
    }

    #[test]
    fn rank_is_longest_path_regardless_of_declaration_order() {
        // C and D are declared first but are children; they must not be roots.
        let (g, l) = layered("graph TD\nC[ccc]\nD[ddd]\nA --> D\nB --> C\nD --> E\nA --> E");
        assert_eq!(l.rank[id(&g, "A")], 0);
        assert_eq!(l.rank[id(&g, "B")], 0);
        assert_eq!(l.rank[id(&g, "C")], 1);
        assert_eq!(l.rank[id(&g, "D")], 1);
        assert_eq!(l.rank[id(&g, "E")], 2, "skip edge A->E does not pull E up");
    }

    #[test]
    fn back_edges_do_not_disturb_ranks() {
        let (g, l) = layered("graph TD\nA --> B\nB --> C\nC --> A\nB --> B");
        assert_eq!(
            (
                l.rank[id(&g, "A")],
                l.rank[id(&g, "B")],
                l.rank[id(&g, "C")]
            ),
            (0, 1, 2)
        );
    }

    #[test]
    fn children_are_ordered_under_their_parents() {
        let (g, l) = layered("graph TD\nC[ccc]\nD[ddd]\nA --> D\nB --> C");
        let slot = |n: &str| l.slot[id(&g, n)];
        assert!(slot("A") < slot("B"));
        assert!(
            slot("D") < slot("C"),
            "crossing removed: D sits under A, C under B"
        );
        assert_eq!(slot("D"), slot("A"));
        assert_eq!(slot("C"), slot("B"));
    }

    #[test]
    fn shared_child_centers_and_fan_out_spreads() {
        let (g, l) = layered("graph TD\nA --> B & C & D\nB & C & D --> E");
        let slot = |n: &str| l.slot[id(&g, n)];
        assert_eq!((slot("B"), slot("C"), slot("D")), (0, 1, 2));
        assert_eq!(slot("A"), 1);
        assert_eq!(slot("E"), 1);
    }

    #[test]
    fn subgraph_members_stay_contiguous_and_perpendicular_edges_share_a_rank() {
        let (g, l) = layered(
			"graph TD\nsubgraph one [LR Group]\ndirection LR\nA --> B\nend\nX --> A\nB --> Y\nX --> \
			 Z\nZ --> Y",
		);
        assert_eq!(
            l.rank[id(&g, "A")],
            l.rank[id(&g, "B")],
            "LR edge inside a TD graph"
        );
        assert!(l.slot[id(&g, "A")] < l.slot[id(&g, "B")]);
        let row = &l.order[l.rank[id(&g, "A")]];
        let a = row.iter().position(|&n| n == id(&g, "A")).unwrap();
        let b = row.iter().position(|&n| n == id(&g, "B")).unwrap();
        assert_eq!(b, a + 1, "outsider Z must not sit between A and B: {row:?}");
    }
}
