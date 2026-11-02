//! Entity-relationship diagram parser and terminal renderer.

pub mod parser;

use crate::mermaid::{
    ansi::{CharRole, ColorMode, Theme},
    canvas::{to_cells, Canvas, Cell, RoleCanvas},
    flowchart::{
        draw::{multi_box_size, set_cell, stamp_multi_box},
        AsciiConfig,
    },
    text::split_lines,
};

/// Parsed entity-relationship diagram.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ErDiagram {
    /// Entities in first-declaration order.
    pub entities: Vec<ErEntity>,
    /// Relationships in source order.
    pub relationships: Vec<ErRelationship>,
}

/// Entity declaration and its attributes.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ErEntity {
    /// Mermaid identifier.
    pub id: String,
    /// Display label; currently the identifier.
    pub label: String,
    /// Declared database attributes.
    pub attributes: Vec<ErAttribute>,
}

/// Database attribute shown in an entity box.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ErAttribute {
    /// Mermaid data type.
    pub r#type: String,
    /// Attribute name.
    pub name: String,
    /// Primary, foreign, and unique constraints in source order.
    pub keys: Vec<ErKey>,
    /// Optional normalized attribute comment.
    pub comment: Option<String>,
}

/// Key constraint attached to an entity attribute.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ErKey {
    /// Primary key (`PK`).
    Primary,
    /// Foreign key (`FK`).
    Foreign,
    /// Unique key (`UK`).
    Unique,
}

impl ErKey {
    fn parse(text: &str) -> Option<Self> {
        match text.to_ascii_uppercase().as_str() {
            "PK" => Some(Self::Primary),
            "FK" => Some(Self::Foreign),
            "UK" => Some(Self::Unique),
            _ => None,
        }
    }

    const fn abbreviation(self) -> &'static str {
        match self {
            Self::Primary => "PK",
            Self::Foreign => "FK",
            Self::Unique => "UK",
        }
    }
}

/// Crow's-foot cardinality at one end of a relationship.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Cardinality {
    /// Exactly one (`||`).
    One,
    /// Zero or one (`o|` or `|o`).
    ZeroOne,
    /// One or more (`}|` or `|{`).
    Many,
    /// Zero or more (`o{` or `{o`).
    ZeroMany,
}

/// Relationship between two entities.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ErRelationship {
    /// Entity at the source end.
    pub entity1: String,
    /// Entity at the target end.
    pub entity2: String,
    /// Cardinality at `entity1`.
    pub cardinality1: Cardinality,
    /// Cardinality at `entity2`.
    pub cardinality2: Cardinality,
    /// Normalized relationship label.
    pub label: String,
    /// Solid identifying relationship rather than dashed non-identifying.
    pub identifying: bool,
}

#[derive(Clone, Debug)]
struct PlacedEntity {
    entity_index: usize,
    x: i32,
    y: i32,
    width: i32,
    height: i32,
}

/// Render Mermaid `erDiagram` source as ASCII or Unicode terminal art.
pub fn render(text: &str, config: &AsciiConfig, mode: ColorMode, theme: &Theme) -> String {
    let diagram = parser::parse_er_diagram(text);
    if diagram.entities.is_empty() {
        return String::new();
    }

    let entity_sections: Vec<Vec<Vec<String>>> =
        diagram.entities.iter().map(build_entity_sections).collect();
    let dimensions: Vec<(i32, i32)> = entity_sections
        .iter()
        .map(|sections| multi_box_size(sections))
        .collect();

    let components = connected_components(&diagram);
    let mut placed = Vec::with_capacity(diagram.entities.len());
    let mut current_y = 0;
    for component in components {
        let entity_count = component.iter().filter(|&&member| member).count();
        let max_per_row = ((entity_count as f64).sqrt().ceil() as usize).max(2);
        let mut current_x = 0;
        let mut max_row_height = 0;
        let mut column_count = 0;

        for (entity_index, is_member) in component.into_iter().enumerate() {
            if !is_member {
                continue;
            }
            let (width, height) = dimensions[entity_index];
            if column_count >= max_per_row {
                current_y += max_row_height + 4;
                current_x = 0;
                max_row_height = 0;
                column_count = 0;
            }
            placed.push(PlacedEntity {
                entity_index,
                x: current_x,
                y: current_y,
                width,
                height,
            });
            current_x += width + 6;
            max_row_height = max_row_height.max(height);
            column_count += 1;
        }
        current_y += max_row_height + 6;
    }

    let total_width = placed
        .iter()
        .map(|entity| entity.x + entity.width)
        .max()
        .unwrap_or(0)
        + 4;
    let total_height = placed
        .iter()
        .map(|entity| entity.y + entity.height)
        .max()
        .unwrap_or(0)
        + 2;
    let mut canvas = Canvas::new(total_width, total_height);
    let mut roles = RoleCanvas::new(total_width, total_height);

    for entity in &placed {
        stamp_multi_box(
            &mut canvas,
            &mut roles,
            &entity_sections[entity.entity_index],
            entity.x,
            entity.y,
            config.use_ascii,
        );
    }

    draw_relationships(&diagram, &placed, config.use_ascii, &mut canvas, &mut roles);
    canvas.render_with_roles(Some(&roles), mode, theme)
}

fn format_attribute(attribute: &ErAttribute) -> String {
    let keys = if attribute.keys.is_empty() {
        "   ".to_owned()
    } else {
        let mut text = attribute
            .keys
            .iter()
            .map(|key| key.abbreviation())
            .collect::<Vec<_>>()
            .join(",");
        text.push(' ');
        text
    };
    format!("{keys}{} {}", attribute.r#type, attribute.name)
}

fn build_entity_sections(entity: &ErEntity) -> Vec<Vec<String>> {
    let header = split_lines(&entity.label).map(str::to_owned).collect();
    let attributes: Vec<String> = entity.attributes.iter().map(format_attribute).collect();
    if attributes.is_empty() {
        vec![header]
    } else {
        vec![header, attributes]
    }
}

const fn crows_foot(cardinality: Cardinality, use_ascii: bool, is_right: bool) -> &'static str {
    match (use_ascii, cardinality, is_right) {
        (true, Cardinality::One, _) => "|",
        (true, Cardinality::ZeroOne, _) => "o|",
        (true, Cardinality::Many, true) => "<",
        (true, Cardinality::Many, false) => ">",
        (true, Cardinality::ZeroMany, true) => "o<",
        (true, Cardinality::ZeroMany, false) => ">o",
        (false, Cardinality::One, _) => "│",
        (false, Cardinality::ZeroOne, _) => "○│",
        (false, Cardinality::Many, true) => "╟",
        (false, Cardinality::Many, false) => "╢",
        (false, Cardinality::ZeroMany, true) => "○╟",
        (false, Cardinality::ZeroMany, false) => "╢○",
    }
}

fn connected_components(diagram: &ErDiagram) -> Vec<Vec<bool>> {
    let entity_count = diagram.entities.len();
    let mut neighbors = vec![Vec::new(); entity_count];
    for relationship in &diagram.relationships {
        let Some(first) = entity_index(diagram, &relationship.entity1) else {
            continue;
        };
        let Some(second) = entity_index(diagram, &relationship.entity2) else {
            continue;
        };
        if !neighbors[first].contains(&second) {
            neighbors[first].push(second);
        }
        if !neighbors[second].contains(&first) {
            neighbors[second].push(first);
        }
    }

    let mut visited = vec![false; entity_count];
    let mut components = Vec::new();
    for start in 0..entity_count {
        if visited[start] {
            continue;
        }
        let mut component = vec![false; entity_count];
        let mut stack = vec![start];
        while let Some(entity) = stack.pop() {
            if visited[entity] {
                continue;
            }
            visited[entity] = true;
            component[entity] = true;
            for &neighbor in &neighbors[entity] {
                if !visited[neighbor] {
                    stack.push(neighbor);
                }
            }
        }
        components.push(component);
    }
    components
}

fn entity_index(diagram: &ErDiagram, id: &str) -> Option<usize> {
    diagram.entities.iter().position(|entity| entity.id == id)
}

fn placed_entity<'a>(
    diagram: &ErDiagram,
    placed: &'a [PlacedEntity],
    id: &str,
) -> Option<&'a PlacedEntity> {
    let index = entity_index(diagram, id)?;
    placed.iter().find(|entity| entity.entity_index == index)
}

fn draw_relationships(
    diagram: &ErDiagram,
    placed: &[PlacedEntity],
    use_ascii: bool,
    canvas: &mut Canvas,
    roles: &mut RoleCanvas,
) {
    let horizontal = if use_ascii { '-' } else { '─' };
    let vertical = if use_ascii { '|' } else { '│' };
    let dashed_horizontal = if use_ascii { '.' } else { '╌' };
    let dashed_vertical = if use_ascii { ':' } else { '┊' };

    for relationship in &diagram.relationships {
        let Some(first) = placed_entity(diagram, placed, &relationship.entity1) else {
            continue;
        };
        let Some(second) = placed_entity(diagram, placed, &relationship.entity2) else {
            continue;
        };
        let line_horizontal = if relationship.identifying {
            horizontal
        } else {
            dashed_horizontal
        };
        let line_vertical = if relationship.identifying {
            vertical
        } else {
            dashed_vertical
        };
        let first_center_x = first.x + first.width / 2;
        let first_center_y = first.y + first.height / 2;
        let second_center_x = second.x + second.width / 2;
        let second_center_y = second.y + second.height / 2;
        let same_row = (first_center_y - second_center_y).abs() < first.height.max(second.height);

        if same_row {
            let (left, right, left_cardinality, right_cardinality) =
                if first_center_x < second_center_x {
                    (
                        first,
                        second,
                        relationship.cardinality1,
                        relationship.cardinality2,
                    )
                } else {
                    (
                        second,
                        first,
                        relationship.cardinality2,
                        relationship.cardinality1,
                    )
                };
            let start_x = left.x + left.width;
            let end_x = right.x - 1;
            let line_y = left.y + left.height / 2;
            for x in start_x..=end_x {
                set_cell(
                    canvas,
                    roles,
                    x,
                    line_y,
                    Cell::from(line_horizontal),
                    CharRole::Line,
                );
            }

            draw_marker(
                canvas,
                roles,
                start_x,
                line_y,
                crows_foot(left_cardinality, use_ascii, false),
            );
            let right_marker = crows_foot(right_cardinality, use_ascii, true);
            draw_marker(
                canvas,
                roles,
                end_x - right_marker.chars().count() as i32 + 1,
                line_y,
                right_marker,
            );

            if !relationship.label.is_empty() {
                let lines: Vec<&str> = split_lines(&relationship.label).collect();
                let gap_middle = (start_x + end_x).div_euclid(2);
                for (line_index, line) in lines.iter().enumerate() {
                    let cells = to_cells(line);
                    let label_start = start_x.max(gap_middle - (cells.len() as i32).div_euclid(2));
                    let label_y = line_y + 1 + line_index as i32;
                    canvas.ensure_size(label_start + cells.len() as i32 + 1, label_y + 2);
                    roles.ensure_size(label_start + cells.len() as i32 + 1, label_y + 2);
                    for (index, cell) in cells.iter().enumerate() {
                        if cell.is_wide_pad() {
                            continue;
                        }
                        let x = label_start + index as i32;
                        let wide = cells.get(index + 1).is_some_and(Cell::is_wide_pad);
                        if x < start_x || x + i32::from(wide) > end_x {
                            continue;
                        }
                        set_cell(canvas, roles, x, label_y, cell.clone(), CharRole::Text);
                        if wide {
                            set_cell(
                                canvas,
                                roles,
                                x + 1,
                                label_y,
                                Cell::WIDE_PAD,
                                CharRole::Text,
                            );
                        }
                    }
                }
            }
        } else {
            let (upper, lower, upper_cardinality, lower_cardinality) =
                if first_center_y < second_center_y {
                    (
                        first,
                        second,
                        relationship.cardinality1,
                        relationship.cardinality2,
                    )
                } else {
                    (
                        second,
                        first,
                        relationship.cardinality2,
                        relationship.cardinality1,
                    )
                };
            let start_y = upper.y + upper.height;
            let end_y = lower.y - 1;
            let line_x = upper.x + upper.width / 2;
            for y in start_y..=end_y {
                set_cell(
                    canvas,
                    roles,
                    line_x,
                    y,
                    Cell::from(line_vertical),
                    CharRole::Line,
                );
            }

            let lower_center_x = lower.x + lower.width / 2;
            if line_x != lower_center_x {
                let middle_y = (start_y + end_y).div_euclid(2);
                for x in line_x.min(lower_center_x)..=line_x.max(lower_center_x) {
                    set_cell(
                        canvas,
                        roles,
                        x,
                        middle_y,
                        Cell::from(line_horizontal),
                        CharRole::Line,
                    );
                }
                for y in middle_y + 1..=end_y {
                    set_cell(
                        canvas,
                        roles,
                        lower_center_x,
                        y,
                        Cell::from(line_vertical),
                        CharRole::Line,
                    );
                }
            }

            let upper_marker = crows_foot(upper_cardinality, use_ascii, false);
            draw_marker(
                canvas,
                roles,
                line_x - (upper_marker.chars().count() as i32).div_euclid(2),
                start_y,
                upper_marker,
            );
            let target_x = lower_center_x;
            let lower_marker = crows_foot(lower_cardinality, use_ascii, true);
            draw_marker(
                canvas,
                roles,
                target_x - (lower_marker.chars().count() as i32).div_euclid(2),
                end_y,
                lower_marker,
            );

            let lines: Vec<&str> = split_lines(&relationship.label).collect();
            let middle_y = (start_y + end_y).div_euclid(2);
            let label_start_y = middle_y - (lines.len() as i32 - 1).div_euclid(2);
            for (line_index, line) in lines.iter().enumerate() {
                let cells = to_cells(line);
                let label_x = line_x + 2;
                let y = label_start_y + line_index as i32;
                if y < 0 {
                    continue;
                }
                // Every cell at x >= 0 needs columns through x + 1 and rows
                // through y + 1; the last cell sets the extent.
                let last_x = label_x + cells.len() as i32 - 1;
                if !cells.is_empty() && last_x >= 0 {
                    canvas.ensure_size(last_x + 2, y + 2);
                    roles.ensure_size(last_x + 2, y + 2);
                }
                for (index, cell) in cells.into_iter().enumerate() {
                    let x = label_x + index as i32;
                    if x < 0 {
                        continue;
                    }
                    set_cell(canvas, roles, x, y, cell, CharRole::Text);
                }
            }
        }
    }
}

fn draw_marker(canvas: &mut Canvas, roles: &mut RoleCanvas, start_x: i32, y: i32, marker: &str) {
    for (offset, character) in marker.chars().enumerate() {
        set_cell(
            canvas,
            roles,
            start_x + offset as i32,
            y,
            Cell::from(character),
            CharRole::Arrow,
        );
    }
}
