use std::collections::HashMap;

use super::{GridCoord, NodeId};

#[derive(Clone, Copy)]
struct QueueItem {
    coord: GridCoord,
    priority: i32,
}

#[derive(Default)]
struct MinHeap {
    items: Vec<QueueItem>,
}

impl MinHeap {
    fn push(&mut self, item: QueueItem) {
        self.items.push(item);
        let mut index = self.items.len() - 1;
        while index > 0 {
            let parent = (index - 1) >> 1;
            if self.items[index].priority < self.items[parent].priority {
                self.items.swap(index, parent);
                index = parent;
            } else {
                break;
            }
        }
    }

    fn pop(&mut self) -> Option<QueueItem> {
        let top = *self.items.first()?;
        let last = self.items.pop()?;
        if !self.items.is_empty() {
            self.items[0] = last;
            let mut index = 0;
            loop {
                let mut smallest = index;
                let left = 2 * index + 1;
                let right = 2 * index + 2;
                if left < self.items.len()
                    && self.items[left].priority < self.items[smallest].priority
                {
                    smallest = left;
                }
                if right < self.items.len()
                    && self.items[right].priority < self.items[smallest].priority
                {
                    smallest = right;
                }
                if smallest == index {
                    break;
                }
                self.items.swap(index, smallest);
                index = smallest;
            }
        }
        Some(top)
    }
}

/// Manhattan distance with an extra penalty when both axes differ.
pub fn heuristic(a: GridCoord, b: GridCoord) -> i32 {
    let dx = (a.x - b.x).abs();
    let dy = (a.y - b.y).abs();
    dx + dy + i32::from(dx != 0 && dy != 0)
}

#[derive(Clone, Copy)]
struct SearchBounds {
    min_x: i32,
    max_x: i32,
    min_y: i32,
    max_y: i32,
    expansion_limit: usize,
}

const MOVE_DIRS: [GridCoord; 4] = [
    GridCoord::new(1, 0),
    GridCoord::new(-1, 0),
    GridCoord::new(0, 1),
    GridCoord::new(0, -1),
];
const MIN_ROUTING_MARGIN: i32 = 8;
const MIN_EXPANSION_BUDGET: i64 = 256;
const MAX_EXPANSION_BUDGET: i64 = 50_000;

fn search_bounds_for<S: ::std::hash::BuildHasher>(
    grid: &HashMap<GridCoord, NodeId, S>,
    from: GridCoord,
    to: GridCoord,
) -> SearchBounds {
    let mut min_x = from.x.min(to.x);
    let mut max_x = from.x.max(to.x);
    let mut min_y = from.y.min(to.y);
    let mut max_y = from.y.max(to.y);

    for coord in grid.keys() {
        min_x = min_x.min(coord.x);
        max_x = max_x.max(coord.x);
        min_y = min_y.min(coord.y);
        max_y = max_y.max(coord.y);
    }

    let width = i64::from(max_x) - i64::from(min_x) + 1;
    let height = i64::from(max_y) - i64::from(min_y) + 1;
    let margin = i64::from(MIN_ROUTING_MARGIN).max((width.max(height) + 1) / 2);
    let bounded_min_x = i64::from(min_x).saturating_sub(margin).max(0);
    let bounded_max_x = i64::from(max_x).saturating_add(margin);
    let bounded_min_y = i64::from(min_y).saturating_sub(margin).max(0);
    let bounded_max_y = i64::from(max_y).saturating_add(margin);
    let area = (bounded_max_x - bounded_min_x + 1) * (bounded_max_y - bounded_min_y + 1);
    let expansion_limit =
        MAX_EXPANSION_BUDGET.min(MIN_EXPANSION_BUDGET.max(area.saturating_mul(4))) as usize;

    SearchBounds {
        min_x: bounded_min_x.clamp(i64::from(i32::MIN), i64::from(i32::MAX)) as i32,
        max_x: bounded_max_x.clamp(i64::from(i32::MIN), i64::from(i32::MAX)) as i32,
        min_y: bounded_min_y.clamp(i64::from(i32::MIN), i64::from(i32::MAX)) as i32,
        max_y: bounded_max_y.clamp(i64::from(i32::MIN), i64::from(i32::MAX)) as i32,
        expansion_limit,
    }
}

const fn inside_bounds(coord: GridCoord, bounds: SearchBounds) -> bool {
    coord.x >= bounds.min_x
        && coord.x <= bounds.max_x
        && coord.y >= bounds.min_y
        && coord.y <= bounds.max_y
}

/// Find a four-directional A* path through unoccupied grid cells.
pub fn get_path<S: ::std::hash::BuildHasher>(
    grid: &HashMap<GridCoord, NodeId, S>,
    from: GridCoord,
    to: GridCoord,
) -> Option<Vec<GridCoord>> {
    let bounds = search_bounds_for(grid, from, to);
    let mut queue = MinHeap::default();
    queue.push(QueueItem {
        coord: from,
        priority: 0,
    });

    let mut cost_so_far = HashMap::new();
    cost_so_far.insert(from, 0);
    let mut came_from = HashMap::new();
    came_from.insert(from, None);
    let mut expansions = 0;

    while let Some(item) = queue.pop() {
        if expansions >= bounds.expansion_limit {
            return None;
        }
        expansions += 1;
        let current = item.coord;

        if current == to {
            let mut path = Vec::new();
            let mut cursor = Some(current);
            while let Some(coord) = cursor {
                path.push(coord);
                cursor = came_from.get(&coord).copied().flatten();
            }
            path.reverse();
            return Some(path);
        }

        let Some(&current_cost) = cost_so_far.get(&current) else {
            continue;
        };
        for direction in MOVE_DIRS {
            let next = GridCoord::new(
                current.x.saturating_add(direction.x),
                current.y.saturating_add(direction.y),
            );
            if !inside_bounds(next, bounds) || (grid.contains_key(&next) && next != to) {
                continue;
            }

            let new_cost = current_cost + 1;
            if cost_so_far
                .get(&next)
                .is_none_or(|&existing| new_cost < existing)
            {
                cost_so_far.insert(next, new_cost);
                queue.push(QueueItem {
                    coord: next,
                    priority: new_cost + heuristic(next, to),
                });
                came_from.insert(next, Some(current));
            }
        }
    }

    None
}

/// Remove intermediate coordinates along straight path segments.
pub fn merge_path(path: Vec<GridCoord>) -> Vec<GridCoord> {
    if path.len() <= 2 {
        return path;
    }

    let mut merged = Vec::with_capacity(path.len());
    merged.push(path[0]);
    for index in 1..path.len() - 1 {
        let previous = path[index - 1];
        let current = path[index];
        let next = path[index + 1];
        let previous_step = (current.x - previous.x, current.y - previous.y);
        let next_step = (next.x - current.x, next.y - current.y);
        if previous_step != next_step {
            merged.push(current);
        }
    }
    merged.push(*path.last().expect("path has at least three coordinates"));
    merged
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn routes_straight_across_empty_grid() {
        let grid = HashMap::new();
        assert_eq!(
            get_path(&grid, GridCoord::new(0, 0), GridCoord::new(3, 0)),
            Some(vec![
                GridCoord::new(0, 0),
                GridCoord::new(1, 0),
                GridCoord::new(2, 0),
                GridCoord::new(3, 0),
            ])
        );
    }

    #[test]
    fn routes_around_an_obstacle_with_heap_tie_breaking() {
        let grid = HashMap::from([(GridCoord::new(1, 0), 0)]);
        assert_eq!(
            get_path(&grid, GridCoord::new(0, 0), GridCoord::new(2, 0)),
            Some(vec![
                GridCoord::new(0, 0),
                GridCoord::new(0, 1),
                GridCoord::new(1, 1),
                GridCoord::new(2, 1),
                GridCoord::new(2, 0),
            ])
        );
    }

    #[test]
    fn returns_none_when_destination_is_enclosed() {
        let grid = HashMap::from([
            (GridCoord::new(0, 1), 0),
            (GridCoord::new(2, 1), 0),
            (GridCoord::new(1, 0), 0),
            (GridCoord::new(1, 2), 0),
        ]);
        assert_eq!(
            get_path(&grid, GridCoord::new(0, 0), GridCoord::new(1, 1)),
            None
        );
    }

    #[test]
    fn collapses_collinear_runs() {
        let path = vec![
            GridCoord::new(0, 0),
            GridCoord::new(1, 0),
            GridCoord::new(2, 0),
            GridCoord::new(2, 1),
            GridCoord::new(2, 2),
        ];
        assert_eq!(
            merge_path(path),
            vec![
                GridCoord::new(0, 0),
                GridCoord::new(2, 0),
                GridCoord::new(2, 2)
            ]
        );
    }
}
