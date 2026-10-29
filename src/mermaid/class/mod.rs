pub mod parser;

use std::collections::HashMap;

pub use parser::parse_class_diagram;

use crate::mermaid::{
    ansi::{CharRole, ColorMode, Theme},
    canvas::{to_cells, Canvas, Cell, RoleCanvas},
    flowchart::{
        draw::{multi_box_size, set_cell, stamp_multi_box},
        AsciiConfig,
    },
    text::{display_width, split_lines},
};

/// Parsed logical structure of a Mermaid class diagram.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ClassDiagram {
    /// Classes in first-declaration order.
    pub classes: Vec<ClassNode>,
    /// Relationships in source order.
    pub relationships: Vec<ClassRelationship>,
    /// Namespace groups in closing-brace order.
    pub namespaces: Vec<ClassNamespace>,
}

/// One class declaration and its UML compartments.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClassNode {
    /// Mermaid identifier used by relationships.
    pub id: String,
    /// Display name, including normalized generic notation.
    pub label: String,
    /// UML annotation without surrounding angle brackets.
    pub annotation: Option<String>,
    /// Field and property members.
    pub attributes: Vec<ClassMember>,
    /// Function members.
    pub methods: Vec<ClassMember>,
}

/// Visibility classifier on a class member.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum Visibility {
    /// No explicit visibility marker.
    #[default]
    None,
    /// Public (`+`).
    Public,
    /// Private (`-`).
    Private,
    /// Protected (`#`).
    Protected,
    /// Package (`~`).
    Package,
}

impl Visibility {
    const fn marker(self) -> &'static str {
        match self {
            Self::None => "",
            Self::Public => "+",
            Self::Private => "-",
            Self::Protected => "#",
            Self::Package => "~",
        }
    }
}

/// Attribute or method parsed from a class body.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClassMember {
    /// UML visibility.
    pub visibility: Visibility,
    /// Member name without a trailing classifier.
    pub name: String,
    /// Optional declared type.
    pub type_name: Option<String>,
    /// Whether the member has the Mermaid static (`$`) classifier.
    pub is_static: bool,
    /// Whether the member has the Mermaid abstract (`*`) classifier.
    pub is_abstract: bool,
    /// Whether the source member used method parentheses.
    pub is_method: bool,
    /// Raw method parameter text, when non-empty.
    pub params: Option<String>,
}

/// UML relationship line style and endpoint marker.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RelationshipType {
    /// Solid line with a hollow triangle.
    Inheritance,
    /// Solid line with a filled diamond.
    Composition,
    /// Solid line with a hollow diamond.
    Aggregation,
    /// Solid line with an open arrow.
    Association,
    /// Dashed line with an open arrow.
    Dependency,
    /// Dashed line with a hollow triangle.
    Realization,
}

/// Endpoint carrying a relationship's UML marker.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MarkerAt {
    /// Marker beside the relationship's `from` class.
    From,
    /// Marker beside the relationship's `to` class.
    To,
}

/// Relationship between two class identifiers.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClassRelationship {
    /// Source identifier as written.
    pub from: String,
    /// Target identifier as written.
    pub to: String,
    /// UML relationship kind.
    pub relationship_type: RelationshipType,
    /// Endpoint carrying the marker.
    pub marker_at: MarkerAt,
    /// Optional relationship label.
    pub label: Option<String>,
    /// Optional cardinality beside `from`.
    pub from_cardinality: Option<String>,
    /// Optional cardinality beside `to`.
    pub to_cardinality: Option<String>,
}

/// Namespace declaration and the explicitly declared classes it contains.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClassNamespace {
    /// Namespace name.
    pub name: String,
    /// Class identifiers declared while the namespace was open.
    pub class_ids: Vec<String>,
}

/// Integer point on a positioned class-relationship path.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct ClassPoint {
    /// Horizontal canvas coordinate.
    pub x: i32,
    /// Vertical canvas coordinate.
    pub y: i32,
}

/// Fully positioned class diagram suitable for a drawing backend.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PositionedClassDiagram {
    /// Diagram width.
    pub width: i32,
    /// Diagram height.
    pub height: i32,
    /// Positioned classes.
    pub classes: Vec<PositionedClassNode>,
    /// Routed relationships.
    pub relationships: Vec<PositionedClassRelationship>,
}

/// Class declaration with layout coordinates and compartment dimensions.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PositionedClassNode {
    /// Mermaid identifier.
    pub id: String,
    /// Display label.
    pub label: String,
    /// Optional UML annotation.
    pub annotation: Option<String>,
    /// Attribute compartment members.
    pub attributes: Vec<ClassMember>,
    /// Method compartment members.
    pub methods: Vec<ClassMember>,
    /// Left canvas coordinate.
    pub x: i32,
    /// Top canvas coordinate.
    pub y: i32,
    /// Box width.
    pub width: i32,
    /// Box height.
    pub height: i32,
    /// Header compartment height.
    pub header_height: i32,
    /// Attribute compartment height.
    pub attr_height: i32,
    /// Method compartment height.
    pub method_height: i32,
}

/// Routed relationship between positioned class boxes.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PositionedClassRelationship {
    /// Source class identifier.
    pub from: String,
    /// Target class identifier.
    pub to: String,
    /// UML relationship kind.
    pub relationship_type: RelationshipType,
    /// Endpoint carrying the marker.
    pub marker_at: MarkerAt,
    /// Optional relationship label.
    pub label: Option<String>,
    /// Optional source-end cardinality.
    pub from_cardinality: Option<String>,
    /// Optional target-end cardinality.
    pub to_cardinality: Option<String>,
    /// Manhattan path from source to target.
    pub points: Vec<ClassPoint>,
    /// Optional label center chosen by the layout.
    pub label_position: Option<ClassPoint>,
}

fn format_member(member: &ClassMember) -> String {
    let type_suffix = member
        .type_name
        .as_ref()
        .map_or(String::new(), |kind| format!(": {kind}"));
    format!(
        "{}{}{}",
        member.visibility.marker(),
        member.name,
        type_suffix
    )
}

fn build_class_sections(class: &ClassNode) -> Vec<Vec<String>> {
    let mut header = Vec::new();
    if let Some(annotation) = &class.annotation {
        header.push(format!("<<{annotation}>>"));
    }
    header.extend(split_lines(&class.label).map(str::to_owned));
    let attributes: Vec<String> = class.attributes.iter().map(format_member).collect();
    let methods: Vec<String> = class.methods.iter().map(format_member).collect();
    if attributes.is_empty() && methods.is_empty() {
        vec![header]
    } else if methods.is_empty() {
        vec![header, attributes]
    } else {
        vec![header, attributes, methods]
    }
}

#[derive(Clone, Copy)]
enum MarkerDirection {
    Up,
    Down,
}

const fn marker_shape(kind: RelationshipType, use_ascii: bool, direction: MarkerDirection) -> char {
    match kind {
        RelationshipType::Inheritance | RelationshipType::Realization => match direction {
            MarkerDirection::Down => {
                if use_ascii {
                    '^'
                } else {
                    '△'
                }
            }
            MarkerDirection::Up => {
                if use_ascii {
                    'v'
                } else {
                    '▽'
                }
            }
        },
        RelationshipType::Composition => {
            if use_ascii {
                '*'
            } else {
                '◆'
            }
        }
        RelationshipType::Aggregation => {
            if use_ascii {
                'o'
            } else {
                '◇'
            }
        }
        RelationshipType::Association | RelationshipType::Dependency => match direction {
            MarkerDirection::Down => {
                if use_ascii {
                    'v'
                } else {
                    '▼'
                }
            }
            MarkerDirection::Up => {
                if use_ascii {
                    '^'
                } else {
                    '▲'
                }
            }
        },
    }
}

const fn is_dashed(kind: RelationshipType) -> bool {
    matches!(
        kind,
        RelationshipType::Dependency | RelationshipType::Realization
    )
}

struct PlacedClass {
    class_index: usize,
    x: i32,
    y: i32,
    width: i32,
    height: i32,
}

fn set_char(
    canvas: &mut Canvas,
    roles: &mut RoleCanvas,
    x: i32,
    y: i32,
    character: char,
    role: CharRole,
) {
    set_cell(canvas, roles, x, y, Cell::from(character), role);
}

fn increase_size<T: Clone + Default>(grid: &mut crate::mermaid::canvas::Grid<T>, x: i32, y: i32) {
    grid.ensure_size(x + 1, y + 1);
}

fn inside_box(placed: &[Option<PlacedClass>], x: i32, y: i32, excluded: [usize; 2]) -> bool {
    placed.iter().flatten().any(|class| {
        !excluded.contains(&class.class_index)
            && x >= class.x
            && x < class.x + class.width
            && y >= class.y
            && y < class.y + class.height
    })
}

fn clear_column(
    placed: &[Option<PlacedClass>],
    start_x: i32,
    y1: i32,
    y2: i32,
    excluded: [usize; 2],
    total_width: i32,
) -> i32 {
    let clear = |x| (y1.min(y2)..=y1.max(y2)).all(|y| !inside_box(placed, x, y, excluded));
    if clear(start_x) {
        return start_x;
    }
    for offset in 1..total_width + 10 {
        let right_x = start_x + offset;
        if clear(right_x) {
            return right_x;
        }
        let left_x = start_x - offset;
        if left_x >= 0 && clear(left_x) {
            return left_x;
        }
    }
    total_width + 2
}

/// Render Mermaid class-diagram source as byte-compatible ASCII or Unicode
/// text.
pub fn render(text: &str, config: &AsciiConfig, mode: ColorMode, theme: &Theme) -> String {
    let diagram = parse_class_diagram(text);
    if diagram.classes.is_empty() {
        return String::new();
    }

    let use_ascii = config.use_ascii;
    let horizontal_gap = 4;
    let vertical_gap = 3;
    let class_sections: Vec<Vec<Vec<String>>> =
        diagram.classes.iter().map(build_class_sections).collect();
    let class_sizes: Vec<(i32, i32)> = class_sections
        .iter()
        .map(|sections| multi_box_size(sections))
        .collect();

    let class_indices: HashMap<&str, usize> = diagram
        .classes
        .iter()
        .enumerate()
        .map(|(index, class)| (class.id.as_str(), index))
        .collect();
    let mut parents = vec![Vec::<usize>::new(); diagram.classes.len()];
    let mut children = vec![Vec::<usize>::new(); diagram.classes.len()];
    for relationship in &diagram.relationships {
        let Some(&from) = class_indices.get(relationship.from.as_str()) else {
            continue;
        };
        let Some(&to) = class_indices.get(relationship.to.as_str()) else {
            continue;
        };
        let hierarchical = matches!(
            relationship.relationship_type,
            RelationshipType::Inheritance | RelationshipType::Realization
        );
        let (parent, child) = if hierarchical && relationship.marker_at == MarkerAt::To {
            (to, from)
        } else {
            (from, to)
        };
        if !parents[child].contains(&parent) {
            parents[child].push(parent);
        }
        if !children[parent].contains(&child) {
            children[parent].push(child);
        }
    }

    let mut levels = vec![None::<i32>; diagram.classes.len()];
    let mut queue = Vec::new();
    for (index, class_parents) in parents.iter().enumerate() {
        if class_parents.is_empty() {
            levels[index] = Some(0);
            queue.push(index);
        }
    }
    let level_cap = diagram.classes.len() as i32 - 1;
    let mut queue_index = 0;
    while queue_index < queue.len() {
        let parent = queue[queue_index];
        queue_index += 1;
        for &child in &children[parent] {
            let new_level = levels[parent].unwrap_or(0) + 1;
            if new_level > level_cap {
                continue;
            }
            if levels[child].is_none_or(|old_level| old_level < new_level) {
                levels[child] = Some(new_level);
                queue.push(child);
            }
        }
    }
    for level in &mut levels {
        if level.is_none() {
            *level = Some(0);
        }
    }
    let max_level = levels.iter().filter_map(|level| *level).max().unwrap_or(0);
    let mut level_groups = vec![Vec::<usize>::new(); max_level as usize + 1];
    for (index, level) in levels.iter().enumerate() {
        level_groups[level.unwrap_or(0) as usize].push(index);
    }

    let mut placed: Vec<Option<PlacedClass>> = (0..diagram.classes.len()).map(|_| None).collect();
    let mut current_y = 0;
    for group in level_groups {
        if group.is_empty() {
            continue;
        }
        let mut current_x = 0;
        let mut max_height = 0;
        for class_index in group {
            let (width, height) = class_sizes[class_index];
            placed[class_index] = Some(PlacedClass {
                class_index,
                x: current_x,
                y: current_y,
                width,
                height,
            });
            current_x += width + horizontal_gap;
            max_height = max_height.max(height);
        }
        current_y += max_height + vertical_gap;
    }

    let mut total_width = 0;
    let mut total_height = 0;
    for class in placed.iter().flatten() {
        total_width = total_width.max(class.x + class.width);
        total_height = total_height.max(class.y + class.height);
    }
    total_width += 4;
    total_height += 2;
    let mut canvas = Canvas::new(total_width, total_height);
    let mut roles = RoleCanvas::new(total_width, total_height);

    for class in placed.iter().flatten() {
        stamp_multi_box(
            &mut canvas,
            &mut roles,
            &class_sections[class.class_index],
            class.x,
            class.y,
            use_ascii,
        );
    }

    let horizontal = if use_ascii { '-' } else { '─' };
    let vertical = if use_ascii { '|' } else { '│' };
    let dashed_horizontal = if use_ascii { '.' } else { '╌' };
    let dashed_vertical = if use_ascii { ':' } else { '┊' };

    for relationship in &diagram.relationships {
        let Some(&from_index) = class_indices.get(relationship.from.as_str()) else {
            continue;
        };
        let Some(&to_index) = class_indices.get(relationship.to.as_str()) else {
            continue;
        };
        let Some(from) = &placed[from_index] else {
            continue;
        };
        let Some(to) = &placed[to_index] else {
            continue;
        };
        let line_horizontal = if is_dashed(relationship.relationship_type) {
            dashed_horizontal
        } else {
            horizontal
        };
        let line_vertical = if is_dashed(relationship.relationship_type) {
            dashed_vertical
        } else {
            vertical
        };
        let excluded = [from_index, to_index];
        let from_center_x = from.x + from.width / 2;
        let from_bottom_y = from.y + from.height - 1;
        let to_center_x = to.x + to.width / 2;
        let to_top_y = to.y;

        if from_bottom_y < to_top_y {
            let route_x = clear_column(
                &placed,
                from_center_x,
                from_bottom_y + 1,
                to_top_y - 1,
                excluded,
                total_width,
            );
            let needs_detour = route_x != from_center_x;
            if route_x >= total_width {
                increase_size(&mut canvas, route_x + 2, total_height);
            }
            if needs_detour {
                let exit_y = from_bottom_y + 1;
                let entry_y = to_top_y - 1;
                for x in from_center_x.min(route_x)..=from_center_x.max(route_x) {
                    set_char(
                        &mut canvas,
                        &mut roles,
                        x,
                        exit_y,
                        line_horizontal,
                        CharRole::Line,
                    );
                }
                if !use_ascii && exit_y < canvas.height() {
                    if from_center_x < route_x {
                        set_char(
                            &mut canvas,
                            &mut roles,
                            from_center_x,
                            exit_y,
                            '└',
                            CharRole::Corner,
                        );
                        set_char(
                            &mut canvas,
                            &mut roles,
                            route_x,
                            exit_y,
                            '┐',
                            CharRole::Corner,
                        );
                    } else {
                        set_char(
                            &mut canvas,
                            &mut roles,
                            from_center_x,
                            exit_y,
                            '┘',
                            CharRole::Corner,
                        );
                        set_char(
                            &mut canvas,
                            &mut roles,
                            route_x,
                            exit_y,
                            '┌',
                            CharRole::Corner,
                        );
                    }
                }
                for y in exit_y + 1..=entry_y {
                    set_char(
                        &mut canvas,
                        &mut roles,
                        route_x,
                        y,
                        line_vertical,
                        CharRole::Line,
                    );
                }
                if route_x != to_center_x {
                    for x in route_x.min(to_center_x)..=route_x.max(to_center_x) {
                        set_char(
                            &mut canvas,
                            &mut roles,
                            x,
                            entry_y,
                            line_horizontal,
                            CharRole::Line,
                        );
                    }
                    if !use_ascii && entry_y < canvas.height() {
                        if route_x < to_center_x {
                            set_char(
                                &mut canvas,
                                &mut roles,
                                route_x,
                                entry_y,
                                '└',
                                CharRole::Corner,
                            );
                            set_char(
                                &mut canvas,
                                &mut roles,
                                to_center_x,
                                entry_y,
                                '┐',
                                CharRole::Corner,
                            );
                        } else {
                            set_char(
                                &mut canvas,
                                &mut roles,
                                route_x,
                                entry_y,
                                '┘',
                                CharRole::Corner,
                            );
                            set_char(
                                &mut canvas,
                                &mut roles,
                                to_center_x,
                                entry_y,
                                '┌',
                                CharRole::Corner,
                            );
                        }
                    }
                }
                if relationship.marker_at == MarkerAt::To {
                    let marker = marker_shape(
                        relationship.relationship_type,
                        use_ascii,
                        MarkerDirection::Down,
                    );
                    set_char(
                        &mut canvas,
                        &mut roles,
                        to_center_x,
                        entry_y,
                        marker,
                        CharRole::Arrow,
                    );
                }
            } else {
                let middle_y = from_bottom_y + (to_top_y - from_bottom_y) / 2;
                for y in from_bottom_y + 1..=middle_y {
                    set_char(
                        &mut canvas,
                        &mut roles,
                        from_center_x,
                        y,
                        line_vertical,
                        CharRole::Line,
                    );
                }
                if from_center_x != to_center_x && middle_y < canvas.height() {
                    for x in from_center_x.min(to_center_x)..=from_center_x.max(to_center_x) {
                        set_char(
                            &mut canvas,
                            &mut roles,
                            x,
                            middle_y,
                            line_horizontal,
                            CharRole::Line,
                        );
                    }
                    if !use_ascii {
                        let (from_corner, to_corner) = if from_center_x < to_center_x {
                            ('└', '┐')
                        } else {
                            ('┘', '┌')
                        };
                        set_char(
                            &mut canvas,
                            &mut roles,
                            from_center_x,
                            middle_y,
                            from_corner,
                            CharRole::Corner,
                        );
                        set_char(
                            &mut canvas,
                            &mut roles,
                            to_center_x,
                            middle_y,
                            to_corner,
                            CharRole::Corner,
                        );
                    }
                }
                for y in middle_y + 1..to_top_y {
                    set_char(
                        &mut canvas,
                        &mut roles,
                        to_center_x,
                        y,
                        line_vertical,
                        CharRole::Line,
                    );
                }
                if relationship.marker_at == MarkerAt::To {
                    let marker = marker_shape(
                        relationship.relationship_type,
                        use_ascii,
                        MarkerDirection::Down,
                    );
                    set_char(
                        &mut canvas,
                        &mut roles,
                        to_center_x,
                        to_top_y - 1,
                        marker,
                        CharRole::Arrow,
                    );
                }
            }
            if relationship.marker_at == MarkerAt::From {
                let marker = marker_shape(
                    relationship.relationship_type,
                    use_ascii,
                    MarkerDirection::Down,
                );
                set_char(
                    &mut canvas,
                    &mut roles,
                    from_center_x,
                    from_bottom_y + 1,
                    marker,
                    CharRole::Arrow,
                );
            }
        } else if to.y + to.height - 1 < from.y {
            let from_top_y = from.y;
            let to_bottom_y = to.y + to.height - 1;
            let middle_y = to_bottom_y + (from_top_y - to_bottom_y) / 2;
            for y in (middle_y..from_top_y).rev() {
                set_char(
                    &mut canvas,
                    &mut roles,
                    from_center_x,
                    y,
                    line_vertical,
                    CharRole::Line,
                );
            }
            if from_center_x != to_center_x {
                for x in from_center_x.min(to_center_x)..=from_center_x.max(to_center_x) {
                    set_char(
                        &mut canvas,
                        &mut roles,
                        x,
                        middle_y,
                        line_horizontal,
                        CharRole::Line,
                    );
                }
                if !use_ascii && middle_y >= 0 && middle_y < total_height {
                    let (from_corner, to_corner) = if from_center_x < to_center_x {
                        ('┌', '┘')
                    } else {
                        ('┐', '└')
                    };
                    set_char(
                        &mut canvas,
                        &mut roles,
                        from_center_x,
                        middle_y,
                        from_corner,
                        CharRole::Corner,
                    );
                    set_char(
                        &mut canvas,
                        &mut roles,
                        to_center_x,
                        middle_y,
                        to_corner,
                        CharRole::Corner,
                    );
                }
            }
            if middle_y - 1 > to_bottom_y {
                for y in ((to_bottom_y + 1)..middle_y).rev() {
                    set_char(
                        &mut canvas,
                        &mut roles,
                        to_center_x,
                        y,
                        line_vertical,
                        CharRole::Line,
                    );
                }
            }
            if relationship.marker_at == MarkerAt::From {
                let marker = marker_shape(
                    relationship.relationship_type,
                    use_ascii,
                    MarkerDirection::Up,
                );
                set_char(
                    &mut canvas,
                    &mut roles,
                    from_center_x,
                    from_top_y - 1,
                    marker,
                    CharRole::Arrow,
                );
            }
            if relationship.marker_at == MarkerAt::To {
                let hierarchical = matches!(
                    relationship.relationship_type,
                    RelationshipType::Inheritance | RelationshipType::Realization
                );
                let direction = if hierarchical {
                    MarkerDirection::Down
                } else {
                    MarkerDirection::Up
                };
                let marker = marker_shape(relationship.relationship_type, use_ascii, direction);
                set_char(
                    &mut canvas,
                    &mut roles,
                    to_center_x,
                    to_bottom_y + 1,
                    marker,
                    CharRole::Arrow,
                );
            }
        } else {
            let detour_y = from_bottom_y.max(to.y + to.height - 1) + 2;
            increase_size(&mut canvas, total_width, detour_y + 1);
            increase_size(&mut roles, total_width, detour_y + 1);
            for y in from_bottom_y + 1..=detour_y {
                set_char(
                    &mut canvas,
                    &mut roles,
                    from_center_x,
                    y,
                    line_vertical,
                    CharRole::Line,
                );
            }
            for x in from_center_x.min(to_center_x)..=from_center_x.max(to_center_x) {
                set_char(
                    &mut canvas,
                    &mut roles,
                    x,
                    detour_y,
                    line_horizontal,
                    CharRole::Line,
                );
            }
            if to.y + to.height < detour_y {
                for y in (to.y + to.height..detour_y).rev() {
                    set_char(
                        &mut canvas,
                        &mut roles,
                        to_center_x,
                        y,
                        line_vertical,
                        CharRole::Line,
                    );
                }
            }
            if relationship.marker_at == MarkerAt::From {
                let marker = marker_shape(
                    relationship.relationship_type,
                    use_ascii,
                    MarkerDirection::Down,
                );
                set_char(
                    &mut canvas,
                    &mut roles,
                    from_center_x,
                    from_bottom_y + 1,
                    marker,
                    CharRole::Arrow,
                );
            }
            if relationship.marker_at == MarkerAt::To {
                let marker = marker_shape(
                    relationship.relationship_type,
                    use_ascii,
                    MarkerDirection::Up,
                );
                set_char(
                    &mut canvas,
                    &mut roles,
                    to_center_x,
                    to.y + to.height,
                    marker,
                    CharRole::Arrow,
                );
            }
        }

        if let Some(label) = &relationship.label {
            let lines: Vec<&str> = split_lines(label).collect();
            let max_label_width = lines
                .iter()
                .map(|line| display_width(line) as i32)
                .max()
                .unwrap_or(0)
                + 2;
            let (base_middle_y, ideal_middle_x) = if from_bottom_y < to_top_y {
                (
                    (from_bottom_y + 1 + to_top_y - 1) / 2,
                    i32::midpoint(from_center_x, to_center_x),
                )
            } else if to.y + to.height - 1 < from.y {
                let to_bottom_y = to.y + to.height - 1;
                (
                    (to_bottom_y + 1 + from.y - 1) / 2,
                    i32::midpoint(from_center_x, to_center_x),
                )
            } else {
                (
                    from_bottom_y.max(to.y + to.height - 1) + 2,
                    i32::midpoint(from_center_x, to_center_x),
                )
            };
            let mut label_y = base_middle_y;
            let half_height = lines.len() as i32 / 2;
            let mut label_in_box = false;
            for (line_index, _) in lines.iter().enumerate() {
                let y = label_y - half_height + line_index as i32;
                let label_start = (ideal_middle_x - max_label_width / 2).max(0);
                for x in label_start..label_start + max_label_width {
                    if inside_box(&placed, x, y, excluded) {
                        label_in_box = true;
                        break;
                    }
                }
                if label_in_box {
                    break;
                }
            }
            if label_in_box {
                let gap_top = from_bottom_y + 1;
                let gap_bottom = to_top_y - 1;
                if gap_top <= gap_bottom {
                    for y in gap_top..=gap_bottom {
                        let label_start = (ideal_middle_x - max_label_width / 2).max(0);
                        let clear = (label_start..label_start + max_label_width)
                            .all(|x| !inside_box(&placed, x, y, excluded));
                        if clear {
                            label_y = y;
                            break;
                        }
                    }
                }
            }
            let start_y = label_y - half_height;
            for (line_index, line) in lines.iter().enumerate() {
                let cells = to_cells(&format!(" {line} "));
                let label_start = (ideal_middle_x - cells.len() as i32 / 2).max(0);
                let y = start_y + line_index as i32;
                let label_end = label_start + cells.len() as i32;
                if label_end > 0 && y >= 0 {
                    increase_size(&mut canvas, label_end.max(1), (y + 1).max(1));
                    increase_size(&mut roles, label_end.max(1), (y + 1).max(1));
                }
                for (cell_index, cell) in cells.into_iter().enumerate() {
                    let x = label_start + cell_index as i32;
                    if x >= 0 && y >= 0 {
                        set_cell(&mut canvas, &mut roles, x, y, cell, CharRole::Text);
                    }
                }
            }
        }
    }

    canvas.render_with_roles(Some(&roles), mode, theme)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mermaid::flowchart::LayoutDirection;

    fn config(use_ascii: bool) -> AsciiConfig {
        AsciiConfig {
            use_ascii,
            padding_x: 5,
            padding_y: 5,
            box_border_padding: 1,
            direction: LayoutDirection::TD,
        }
    }

    fn plain_render(source: &str, use_ascii: bool) -> String {
        render(
            source,
            &config(use_ascii),
            ColorMode::None,
            &Theme::default(),
        )
    }

    #[test]
    fn parses_declarations_members_annotations_generics_and_namespaces() {
        let diagram = parse_class_diagram(
            "classDiagram\nnamespace Domain {\nclass Repository ~T~ {\n<<interface>>\n+String \
			 name$;\n#find*(key) Result\n}\nclass Value\n}\n",
        );
        assert_eq!(diagram.classes.len(), 2);
        let repository = &diagram.classes[0];
        assert_eq!(repository.label, "Repository<T>");
        assert_eq!(repository.annotation.as_deref(), Some("interface"));
        assert_eq!(repository.attributes[0].visibility, Visibility::Public);
        assert!(repository.attributes[0].is_static);
        assert_eq!(repository.methods[0].visibility, Visibility::Protected);
        assert!(repository.methods[0].is_abstract);
        assert_eq!(repository.methods[0].params.as_deref(), Some("key"));
        assert_eq!(diagram.namespaces[0].class_ids, ["Repository", "Value"]);
    }

    #[test]
    fn parses_inline_declarations_and_normalizes_relationship_text() {
        let diagram = parse_class_diagram(
            "classDiagram\nclass Shape { <<interface>> }\nShape : ~draw() void\nShape \
			 \"1<br/>owner\" --> \"*\" Canvas : draws<br>now\n",
        );
        assert_eq!(diagram.classes[0].annotation.as_deref(), Some("interface"));
        assert_eq!(
            diagram.classes[0].methods[0].visibility,
            Visibility::Package
        );
        let relationship = &diagram.relationships[0];
        assert_eq!(relationship.from_cardinality.as_deref(), Some("1\nowner"));
        assert_eq!(relationship.to_cardinality.as_deref(), Some("*"));
        assert_eq!(relationship.label.as_deref(), Some("draws\nnow"));
    }

    #[test]
    fn parses_every_relationship_arrow_and_marker_end() {
        let source =
            "classDiagram\nA <|-- B\nC --|> D\nE <|.. F\nG ..|> H\nI *-- J\nK --* L\nM o-- \
		              N\nO --o P\nQ --> R\nS <-- T\nU ..> V\nW <.. X\nY -- Z\n";
        let diagram = parse_class_diagram(source);
        let actual: Vec<(RelationshipType, MarkerAt)> = diagram
            .relationships
            .iter()
            .map(|relationship| (relationship.relationship_type, relationship.marker_at))
            .collect();
        assert_eq!(
            actual,
            vec![
                (RelationshipType::Inheritance, MarkerAt::From),
                (RelationshipType::Inheritance, MarkerAt::To),
                (RelationshipType::Realization, MarkerAt::From),
                (RelationshipType::Realization, MarkerAt::To),
                (RelationshipType::Composition, MarkerAt::From),
                (RelationshipType::Composition, MarkerAt::To),
                (RelationshipType::Aggregation, MarkerAt::From),
                (RelationshipType::Aggregation, MarkerAt::To),
                (RelationshipType::Association, MarkerAt::To),
                (RelationshipType::Association, MarkerAt::From),
                (RelationshipType::Dependency, MarkerAt::To),
                (RelationshipType::Dependency, MarkerAt::From),
                (RelationshipType::Association, MarkerAt::To),
            ]
        );
    }

    #[test]
    fn unicode_relationship_markers_point_to_the_correct_end() {
        let inheritance = plain_render("classDiagram\nAnimal <|-- Dog", false);
        assert!(inheritance.contains('△'));
        assert!(!inheritance.contains('▽'));
        assert!(inheritance.find("Animal").unwrap() < inheritance.find("Dog").unwrap());

        let association = plain_render("classDiagram\nPerson --> Address", false);
        assert!(association.contains('▼'));
        assert!(!association.contains('▲'));
        assert!(association.find("Person").unwrap() < association.find("Address").unwrap());

        let dependency = plain_render("classDiagram\nClient ..> Server", false);
        assert!(dependency.contains('▼'));
        let realization = plain_render("classDiagram\nCircle ..|> Shape", false);
        assert!(realization.contains('△'));
        assert!(realization.find("Shape").unwrap() < realization.find("Circle").unwrap());
        assert!(plain_render("classDiagram\nCar *-- Engine", false).contains('◆'));
        assert!(plain_render("classDiagram\nTeam o-- Player", false).contains('◇'));
    }

    #[test]
    fn ascii_relationship_markers_match_unicode_directions() {
        assert!(plain_render("classDiagram\nAnimal <|-- Dog", true).contains('^'));
        assert!(plain_render("classDiagram\nPerson --> Address", true).contains('v'));
        assert!(plain_render("classDiagram\nClient ..> Server", true).contains('v'));
        assert!(plain_render("classDiagram\nCar *-- Engine", true).contains('*'));
        assert!(plain_render("classDiagram\nTeam o-- Player", true).contains('o'));
    }

    #[test]
    fn relationship_chains_keep_level_order_and_arrow_count() {
        let inheritance = plain_render("classDiagram\nAnimal <|-- Mammal\nMammal <|-- Dog", false);
        assert!(inheritance.find("Animal").unwrap() < inheritance.find("Mammal").unwrap());
        assert!(inheritance.find("Mammal").unwrap() < inheritance.find("Dog").unwrap());
        assert_eq!(inheritance.matches('△').count(), 2);

        let association = plain_render("classDiagram\nA --> B\nB --> C", false);
        assert!(association.find("│ A │").unwrap() < association.find("│ B │").unwrap());
        assert!(association.find("│ B │").unwrap() < association.find("│ C │").unwrap());
        assert_eq!(association.matches('▼').count(), 2);
    }

    #[test]
    fn reversed_realization_still_points_toward_the_interface() {
        let output = plain_render("classDiagram\nShape <|.. Circle", false);
        assert!(output.find("Shape").unwrap() < output.find("Circle").unwrap());
        assert!(output.contains('△'));
    }

    #[test]
    fn cycles_and_member_boxes_preserve_visible_arrows() {
        let cycle = plain_render("classDiagram\nA --> B\nB --> C\nC ..> A", false);
        assert!(cycle.contains('▲') || cycle.contains('▼'));
        assert!(cycle.contains("│ A │"));
        assert!(cycle.contains("│ B │"));
        assert!(cycle.contains("│ C │"));

        let members = plain_render(
            "classDiagram\nclass Animal {\n+String name\n+eat() void\n}\nclass Dog {\n+bark() \
			 void\n}\nAnimal <|-- Dog",
            false,
        );
        assert!(members.contains('△'));
        assert!(members.find("Animal").unwrap() < members.find("Dog").unwrap());
    }

    #[test]
    fn mixed_relationships_render_every_uml_marker() {
        let source = "classDiagram\nA <|-- B : inheritance\nC *-- D : composition\nE o-- F : \
		              aggregation\nG --> H : association\nI ..> J : dependency\nK ..|> L : \
		              realization";
        let output = plain_render(source, false);
        assert_eq!(output.matches('△').count(), 2);
        assert_eq!(output.matches('▼').count(), 2);
        assert!(output.contains('◆'));
        assert!(output.contains('◇'));
    }
}
