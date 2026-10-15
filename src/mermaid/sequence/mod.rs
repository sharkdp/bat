//! Mermaid sequence-diagram parser and column-based terminal renderer.

use crate::mermaid::{
    ansi::{CharRole, ColorMode, Theme},
    canvas::{to_cells, Canvas, Cell, RoleCanvas},
    flowchart::AsciiConfig,
    text::{display_width, line_count, max_line_width, split_lines},
};

pub mod parser;

/// Kind of an actor declaration.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ActorKind {
    Participant,
    Actor,
}

/// Actor or participant in declaration order.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Actor {
    pub id: String,
    pub label: String,
    pub kind: ActorKind,
}

/// Complete message operator, preserving its line and arrowhead spellings.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MessageArrowKind {
    SolidFilled,
    DashedFilled,
    SolidOpen,
    DashedOpen,
    SolidCross,
    DashedCross,
}

impl MessageArrowKind {
    fn from_operator(operator: &str) -> Option<Self> {
        Some(match operator {
            "->>" => Self::SolidFilled,
            "-->>" => Self::DashedFilled,
            "->" | "-)" => Self::SolidOpen,
            "-->" | "--)" => Self::DashedOpen,
            "-x" => Self::SolidCross,
            "--x" => Self::DashedCross,
            _ => return None,
        })
    }

    const fn dashed(self) -> bool {
        matches!(
            self,
            Self::DashedFilled | Self::DashedOpen | Self::DashedCross
        )
    }

    const fn filled(self) -> bool {
        matches!(
            self,
            Self::SolidFilled | Self::DashedFilled | Self::SolidCross | Self::DashedCross
        )
    }
}

/// Chronological message between two actors.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Message {
    pub from: String,
    pub to: String,
    pub label: String,
    pub arrow: MessageArrowKind,
    pub activate: bool,
    pub deactivate: bool,
}

/// Sequence block keyword.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BlockKind {
    Loop,
    Alt,
    Opt,
    Par,
    Critical,
    Break,
    Rect,
}

impl BlockKind {
    fn from_keyword(keyword: &str) -> Self {
        match keyword {
            "loop" => Self::Loop,
            "alt" => Self::Alt,
            "opt" => Self::Opt,
            "par" => Self::Par,
            "critical" => Self::Critical,
            "break" => Self::Break,
            "rect" => Self::Rect,
            _ => unreachable!("sequence parser only accepts known block keywords"),
        }
    }

    const fn keyword(self) -> &'static str {
        match self {
            Self::Loop => "loop",
            Self::Alt => "alt",
            Self::Opt => "opt",
            Self::Par => "par",
            Self::Critical => "critical",
            Self::Break => "break",
            Self::Rect => "rect",
        }
    }
}

/// Divider inside an `alt`, `par`, or `critical` block.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BlockDivider {
    pub index: usize,
    pub label: String,
}

/// Structural frame spanning a range of messages.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Block {
    pub kind: BlockKind,
    pub label: String,
    pub start_index: usize,
    pub end_index: usize,
    pub dividers: Vec<BlockDivider>,
}

/// Placement of a note relative to its actor lifeline.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NotePosition {
    Left,
    Right,
    Over,
}

/// Note attached to one or more actors.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Note {
    pub actor_ids: Vec<String>,
    pub text: String,
    pub position: NotePosition,
    pub after_index: i32,
}

/// Parsed logical sequence diagram.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct SequenceDiagram {
    pub actors: Vec<Actor>,
    pub messages: Vec<Message>,
    pub blocks: Vec<Block>,
    pub notes: Vec<Note>,
}

#[derive(Debug)]
struct NoteLayout {
    x: i32,
    y: i32,
    width: i32,
    height: i32,
    lines: Vec<String>,
}

/// Render a Mermaid sequence diagram using the configured terminal glyph set.
pub fn render(text: &str, config: &AsciiConfig, mode: ColorMode, theme: &Theme) -> String {
    let diagram = parser::parse_sequence_diagram(text);
    if diagram.actors.is_empty() {
        return String::new();
    }

    let use_ascii = config.use_ascii;
    let (h, v, tl, tr, bl, br, jt, jb, jl, jr) = if use_ascii {
        ('-', '|', '+', '+', '+', '+', '+', '+', '+', '+')
    } else {
        ('─', '│', '┌', '┐', '└', '┘', '┬', '┴', '├', '┤')
    };
    let actor_index = |id: &str| diagram.actors.iter().position(|actor| actor.id == id);
    let box_pad = 1_i32;
    let actor_box_widths: Vec<i32> = diagram
        .actors
        .iter()
        .map(|actor| max_line_width(&actor.label) as i32 + 2 * box_pad + 2)
        .collect();
    let half_box: Vec<i32> = actor_box_widths
        .iter()
        .map(|width| (width + 1) / 2)
        .collect();
    let actor_box_h = diagram
        .actors
        .iter()
        .map(|actor| line_count(&actor.label) as i32 + 2)
        .max()
        .unwrap_or(3)
        .max(3);

    let mut adjacent_max_width = vec![0_i32; diagram.actors.len().saturating_sub(1)];
    for message in &diagram.messages {
        let (Some(from), Some(to)) = (actor_index(&message.from), actor_index(&message.to)) else {
            continue;
        };
        if from == to {
            continue;
        }
        let low = from.min(to);
        let high = from.max(to);
        let needed = max_line_width(&message.label) as i32 + 4;
        let gaps = (high - low) as i32;
        let per_gap = (needed + gaps - 1) / gaps;
        for gap in &mut adjacent_max_width[low..high] {
            *gap = (*gap).max(per_gap);
        }
    }

    let mut lifeline_x = vec![half_box[0]];
    for index in 1..diagram.actors.len() {
        let gap = (half_box[index - 1] + half_box[index] + 2)
            .max(adjacent_max_width[index - 1] + 2)
            .max(10);
        lifeline_x.push(lifeline_x[index - 1] + gap);
    }

    let mut message_arrow_y = vec![0_i32; diagram.messages.len()];
    let mut message_label_y = vec![0_i32; diagram.messages.len()];
    let mut block_start_y = vec![None; diagram.blocks.len()];
    let mut block_end_y = vec![None; diagram.blocks.len()];
    let mut divider_y: Vec<Vec<Option<i32>>> = diagram
        .blocks
        .iter()
        .map(|block| vec![None; block.dividers.len()])
        .collect();
    let mut note_positions = Vec::<NoteLayout>::new();
    let mut current_y = actor_box_h;

    for message_index in 0..diagram.messages.len() {
        for (block_index, block) in diagram.blocks.iter().enumerate() {
            if block.start_index == message_index {
                current_y += 2;
                block_start_y[block_index] = Some(current_y - 1);
            }
        }
        for (block_index, block) in diagram.blocks.iter().enumerate() {
            for (divider_index, divider) in block.dividers.iter().enumerate() {
                if divider.index == message_index {
                    current_y += 1;
                    divider_y[block_index][divider_index] = Some(current_y);
                    current_y += 1;
                }
            }
        }

        current_y += 1;
        let message = &diagram.messages[message_index];
        let message_lines = line_count(&message.label) as i32;
        if message.from == message.to {
            message_label_y[message_index] = current_y + 1;
            message_arrow_y[message_index] = current_y;
            current_y += 2 + message_lines;
        } else {
            message_label_y[message_index] = current_y;
            message_arrow_y[message_index] = current_y + message_lines;
            current_y += message_lines + 1;
        }

        for note in diagram
            .notes
            .iter()
            .filter(|note| note.after_index == message_index as i32)
        {
            current_y += 1;
            let lines: Vec<String> = split_lines(&note.text).map(str::to_owned).collect();
            let note_width = lines
                .iter()
                .map(|line| display_width(line) as i32)
                .max()
                .unwrap_or(0)
                + 4;
            let note_height = lines.len() as i32 + 2;
            let actor = note
                .actor_ids
                .first()
                .and_then(|id| actor_index(id))
                .unwrap_or(0);
            let mut x = match note.position {
                NotePosition::Left => lifeline_x[actor] - note_width - 1,
                NotePosition::Right => lifeline_x[actor] + 2,
                NotePosition::Over => {
                    if let Some(other) = note.actor_ids.get(1).and_then(|id| actor_index(id)) {
                        (lifeline_x[actor] + lifeline_x[other]).div_euclid(2)
                            - note_width.div_euclid(2)
                    } else {
                        lifeline_x[actor] - note_width.div_euclid(2)
                    }
                }
            };
            x = x.max(0);
            note_positions.push(NoteLayout {
                x,
                y: current_y,
                width: note_width,
                height: note_height,
                lines,
            });
            current_y += note_height;
        }

        for (block_index, block) in diagram.blocks.iter().enumerate() {
            if block.end_index == message_index {
                current_y += 1;
                block_end_y[block_index] = Some(current_y);
                current_y += 1;
            }
        }
    }

    current_y += 1;
    let footer_y = current_y;
    let total_height = footer_y + actor_box_h;
    let mut total_width =
        lifeline_x.last().copied().unwrap_or(0) + half_box.last().copied().unwrap_or(0) + 2;
    for message in &diagram.messages {
        if message.from == message.to {
            if let Some(from) = actor_index(&message.from) {
                let self_right = lifeline_x[from] + 6 + 2 + display_width(&message.label) as i32;
                total_width = total_width.max(self_right + 1);
            }
        }
    }
    for note in &note_positions {
        total_width = total_width.max(note.x + note.width + 1);
    }

    let mut canvas = Canvas::new(total_width + 1, total_height);
    let mut roles = RoleCanvas::new(total_width + 1, total_height);

    for &x in &lifeline_x {
        for y in actor_box_h..=footer_y {
            set_cell(&mut canvas, &mut roles, x, y, v, CharRole::Line);
        }
    }
    for (index, actor) in diagram.actors.iter().enumerate() {
        draw_actor_box(
            &mut canvas,
            &mut roles,
            lifeline_x[index],
            0,
            &actor.label,
            box_pad,
            (tl, tr, bl, br, h, v),
        );
        draw_actor_box(
            &mut canvas,
            &mut roles,
            lifeline_x[index],
            footer_y,
            &actor.label,
            box_pad,
            (tl, tr, bl, br, h, v),
        );
        if !use_ascii {
            set_cell(
                &mut canvas,
                &mut roles,
                lifeline_x[index],
                actor_box_h - 1,
                jt,
                CharRole::Junction,
            );
            set_cell(
                &mut canvas,
                &mut roles,
                lifeline_x[index],
                footer_y,
                jb,
                CharRole::Junction,
            );
        }
    }

    for (message_index, message) in diagram.messages.iter().enumerate() {
        let (Some(from_index), Some(to_index)) =
            (actor_index(&message.from), actor_index(&message.to))
        else {
            continue;
        };
        let from_x = lifeline_x[from_index];
        let to_x = lifeline_x[to_index];
        let line_char = if message.arrow.dashed() {
            if use_ascii {
                '.'
            } else {
                '╌'
            }
        } else {
            h
        };
        if from_index == to_index {
            let y = message_arrow_y[message_index];
            let loop_width = 4;
            set_cell(&mut canvas, &mut roles, from_x, y, jl, CharRole::Junction);
            for x in from_x + 1..from_x + loop_width {
                set_cell(&mut canvas, &mut roles, x, y, line_char, CharRole::Line);
            }
            set_cell(
                &mut canvas,
                &mut roles,
                from_x + loop_width,
                y,
                if use_ascii { '+' } else { '┐' },
                CharRole::Corner,
            );
            set_cell(
                &mut canvas,
                &mut roles,
                from_x + loop_width,
                y + 1,
                v,
                CharRole::Line,
            );
            let cells = to_cells(&message.label);
            set_cells(
                &mut canvas,
                &mut roles,
                from_x + loop_width + 2,
                y + 1,
                &cells,
                CharRole::Text,
                0,
                total_width,
            );
            let arrow = if use_ascii {
                '<'
            } else if message.arrow.filled() {
                '◀'
            } else {
                '◁'
            };
            set_cell(
                &mut canvas,
                &mut roles,
                from_x,
                y + 2,
                arrow,
                CharRole::Arrow,
            );
            for x in from_x + 1..from_x + loop_width {
                set_cell(&mut canvas, &mut roles, x, y + 2, line_char, CharRole::Line);
            }
            set_cell(
                &mut canvas,
                &mut roles,
                from_x + loop_width,
                y + 2,
                if use_ascii { '+' } else { '┘' },
                CharRole::Corner,
            );
        } else {
            let label_y = message_label_y[message_index];
            let arrow_y = message_arrow_y[message_index];
            let midpoint = (from_x + to_x).div_euclid(2);
            for (line_index, line) in split_lines(&message.label).enumerate() {
                let cells = to_cells(line);
                let start = midpoint - (cells.len() as i32).div_euclid(2);
                set_cells(
                    &mut canvas,
                    &mut roles,
                    start,
                    label_y + line_index as i32,
                    &cells,
                    CharRole::Text,
                    0,
                    total_width,
                );
            }
            if from_x < to_x {
                for x in from_x + 1..to_x {
                    set_cell(
                        &mut canvas,
                        &mut roles,
                        x,
                        arrow_y,
                        line_char,
                        CharRole::Line,
                    );
                }
                let arrow = if use_ascii {
                    '>'
                } else if message.arrow.filled() {
                    '▶'
                } else {
                    '▷'
                };
                set_cell(
                    &mut canvas,
                    &mut roles,
                    to_x,
                    arrow_y,
                    arrow,
                    CharRole::Arrow,
                );
            } else {
                for x in to_x + 1..from_x {
                    set_cell(
                        &mut canvas,
                        &mut roles,
                        x,
                        arrow_y,
                        line_char,
                        CharRole::Line,
                    );
                }
                let arrow = if use_ascii {
                    '<'
                } else if message.arrow.filled() {
                    '◀'
                } else {
                    '◁'
                };
                set_cell(
                    &mut canvas,
                    &mut roles,
                    to_x,
                    arrow_y,
                    arrow,
                    CharRole::Arrow,
                );
            }
        }
    }

    for (block_index, block) in diagram.blocks.iter().enumerate() {
        let (Some(top), Some(bottom)) = (block_start_y[block_index], block_end_y[block_index])
        else {
            continue;
        };
        let mut minimum = total_width;
        let mut maximum = 0;
        for message_index in block.start_index..=block.end_index {
            let Some(message) = diagram.messages.get(message_index) else {
                break;
            };
            let from = actor_index(&message.from).unwrap_or(0);
            let to = actor_index(&message.to).unwrap_or(0);
            minimum = minimum.min(lifeline_x[from.min(to)]);
            maximum = maximum.max(lifeline_x[from.max(to)]);
        }
        let left = (minimum - 4).max(0);
        let right = (maximum + 4).min(total_width - 1);
        set_cell(&mut canvas, &mut roles, left, top, tl, CharRole::Border);
        for x in left + 1..right {
            set_cell(&mut canvas, &mut roles, x, top, h, CharRole::Border);
        }
        set_cell(&mut canvas, &mut roles, right, top, tr, CharRole::Border);
        let header = if block.label.is_empty() {
            block.kind.keyword().to_owned()
        } else {
            format!("{} [{}]", block.kind.keyword(), block.label)
        };
        for (line_index, line) in split_lines(&header).enumerate() {
            let y = top + line_index as i32;
            if y >= bottom {
                break;
            }
            let cells = to_cells(line);
            set_cells(
                &mut canvas,
                &mut roles,
                left + 1,
                y,
                &cells,
                CharRole::Text,
                left + 1,
                right,
            );
        }
        set_cell(&mut canvas, &mut roles, left, bottom, bl, CharRole::Border);
        for x in left + 1..right {
            set_cell(&mut canvas, &mut roles, x, bottom, h, CharRole::Border);
        }
        set_cell(&mut canvas, &mut roles, right, bottom, br, CharRole::Border);
        for y in top + 1..bottom {
            set_cell(&mut canvas, &mut roles, left, y, v, CharRole::Border);
            set_cell(&mut canvas, &mut roles, right, y, v, CharRole::Border);
        }
        for (divider_index, divider) in block.dividers.iter().enumerate() {
            let Some(y) = divider_y[block_index][divider_index] else {
                continue;
            };
            set_cell(&mut canvas, &mut roles, left, y, jl, CharRole::Junction);
            for x in left + 1..right {
                set_cell(
                    &mut canvas,
                    &mut roles,
                    x,
                    y,
                    if use_ascii { '-' } else { '╌' },
                    CharRole::Line,
                );
            }
            set_cell(&mut canvas, &mut roles, right, y, jr, CharRole::Junction);
            if !divider.label.is_empty() {
                let cells = to_cells(&format!("[{}]", divider.label));
                set_cells(
                    &mut canvas,
                    &mut roles,
                    left + 1,
                    y,
                    &cells,
                    CharRole::Text,
                    left + 1,
                    right,
                );
            }
        }
    }

    for note in note_positions {
        canvas.ensure_size(note.x + note.width + 1, note.y + note.height + 1);
        roles.ensure_size(note.x + note.width + 1, note.y + note.height + 1);
        set_cell(
            &mut canvas,
            &mut roles,
            note.x,
            note.y,
            tl,
            CharRole::Border,
        );
        for offset in 1..note.width - 1 {
            set_cell(
                &mut canvas,
                &mut roles,
                note.x + offset,
                note.y,
                h,
                CharRole::Border,
            );
        }
        set_cell(
            &mut canvas,
            &mut roles,
            note.x + note.width - 1,
            note.y,
            tr,
            CharRole::Border,
        );
        for (line_index, line) in note.lines.iter().enumerate() {
            let y = note.y + 1 + line_index as i32;
            set_cell(&mut canvas, &mut roles, note.x, y, v, CharRole::Border);
            set_cell(
                &mut canvas,
                &mut roles,
                note.x + note.width - 1,
                y,
                v,
                CharRole::Border,
            );
            let cells = to_cells(line);
            let limit = canvas.width();
            set_cells(
                &mut canvas,
                &mut roles,
                note.x + 2,
                y,
                &cells,
                CharRole::Text,
                0,
                limit,
            );
        }
        let bottom = note.y + note.height - 1;
        set_cell(
            &mut canvas,
            &mut roles,
            note.x,
            bottom,
            bl,
            CharRole::Border,
        );
        for offset in 1..note.width - 1 {
            set_cell(
                &mut canvas,
                &mut roles,
                note.x + offset,
                bottom,
                h,
                CharRole::Border,
            );
        }
        set_cell(
            &mut canvas,
            &mut roles,
            note.x + note.width - 1,
            bottom,
            br,
            CharRole::Border,
        );
    }

    canvas.render_with_roles(Some(&roles), mode, theme)
}

fn set_cell(canvas: &mut Canvas, roles: &mut RoleCanvas, x: i32, y: i32, ch: char, role: CharRole) {
    if canvas.in_bounds(x, y) {
        canvas.set(x, y, Cell::from(ch));
        roles.set(x, y, Some(role));
    }
}

#[allow(clippy::too_many_arguments)]
fn set_cells(
    canvas: &mut Canvas,
    roles: &mut RoleCanvas,
    x_start: i32,
    y: i32,
    cells: &[Cell],
    role: CharRole,
    minimum_x: i32,
    maximum_x_exclusive: i32,
) {
    let limit = maximum_x_exclusive.min(canvas.width());
    for (index, cell) in cells.iter().enumerate() {
        if cell.is_wide_pad() {
            continue;
        }
        let x = x_start + index as i32;
        let wide = cells.get(index + 1).is_some_and(Cell::is_wide_pad);
        if x < minimum_x || x + i32::from(wide) >= limit {
            continue;
        }
        if canvas.in_bounds(x, y) {
            canvas.set(x, y, cell.clone());
            roles.set(x, y, Some(role));
            if wide {
                canvas.set(x + 1, y, Cell::WIDE_PAD);
                roles.set(x + 1, y, Some(role));
            }
        }
    }
}

fn draw_actor_box(
    canvas: &mut Canvas,
    roles: &mut RoleCanvas,
    center_x: i32,
    top_y: i32,
    label: &str,
    box_pad: i32,
    glyphs: (char, char, char, char, char, char),
) {
    let (tl, tr, bl, br, h, v) = glyphs;
    let lines: Vec<&str> = split_lines(label).collect();
    let maximum_width = max_line_width(label) as i32;
    let width = maximum_width + 2 * box_pad + 2;
    let height = lines.len() as i32 + 2;
    let left = center_x - width.div_euclid(2);
    set_cell(canvas, roles, left, top_y, tl, CharRole::Border);
    for offset in 1..width - 1 {
        set_cell(canvas, roles, left + offset, top_y, h, CharRole::Border);
    }
    set_cell(canvas, roles, left + width - 1, top_y, tr, CharRole::Border);
    for (index, line) in lines.iter().enumerate() {
        let row = top_y + 1 + index as i32;
        set_cell(canvas, roles, left, row, v, CharRole::Border);
        set_cell(canvas, roles, left + width - 1, row, v, CharRole::Border);
        let cells = to_cells(line);
        let start = left + 1 + box_pad + (maximum_width - cells.len() as i32).div_euclid(2);
        let limit = canvas.width();
        set_cells(canvas, roles, start, row, &cells, CharRole::Text, 0, limit);
    }
    let bottom = top_y + height - 1;
    set_cell(canvas, roles, left, bottom, bl, CharRole::Border);
    for offset in 1..width - 1 {
        set_cell(canvas, roles, left + offset, bottom, h, CharRole::Border);
    }
    set_cell(
        canvas,
        roles,
        left + width - 1,
        bottom,
        br,
        CharRole::Border,
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_declarations_messages_and_notes() {
        let diagram = parser::parse_sequence_diagram(
            "sequenceDiagram\nparticipant A as Alice<br>Admin\nactor B as Bob\nA->>+B: \
			 sync\nB-->>-A: async\nA-)B: open\nB--xA: cross\nNote left of A: left\nNote right of B: \
			 right\nNote over A,B: both",
        );
        assert_eq!(diagram.actors.len(), 2);
        assert_eq!(diagram.actors[0].label, "Alice\nAdmin");
        assert_eq!(diagram.actors[1].kind, ActorKind::Actor);
        assert_eq!(
            diagram.messages.iter().map(|m| m.arrow).collect::<Vec<_>>(),
            vec![
                MessageArrowKind::SolidFilled,
                MessageArrowKind::DashedFilled,
                MessageArrowKind::SolidOpen,
                MessageArrowKind::DashedCross,
            ]
        );
        assert!(diagram.messages[0].activate);
        assert!(diagram.messages[1].deactivate);
        assert_eq!(
            diagram.notes.iter().map(|n| n.position).collect::<Vec<_>>(),
            vec![NotePosition::Left, NotePosition::Right, NotePosition::Over,]
        );
    }

    #[test]
    fn parses_every_arrow_operator() {
        let source = "sequenceDiagram\nA->>B: a\nA-->>B: b\nA->B: c\nA-->B: d\nA-)B: e\nA--)B: \
		              f\nA-xB: g\nA--xB: h";
        let diagram = parser::parse_sequence_diagram(source);
        assert_eq!(
            diagram.messages.iter().map(|m| m.arrow).collect::<Vec<_>>(),
            vec![
                MessageArrowKind::SolidFilled,
                MessageArrowKind::DashedFilled,
                MessageArrowKind::SolidOpen,
                MessageArrowKind::DashedOpen,
                MessageArrowKind::SolidOpen,
                MessageArrowKind::DashedOpen,
                MessageArrowKind::SolidCross,
                MessageArrowKind::DashedCross,
            ]
        );
    }

    #[test]
    fn parses_every_block_and_divider_kind() {
        use std::fmt::Write as _;
        let mut source = String::from("sequenceDiagram\nparticipant A\nparticipant B\n");
        for keyword in ["loop", "alt", "opt", "par", "critical", "break", "rect"] {
            write!(source, "{keyword} label\nA->>B: inside\n").unwrap();
            if keyword == "alt" {
                source.push_str("else other\n");
            }
            if keyword == "par" {
                source.push_str("and other\n");
            }
            source.push_str("end\n");
        }
        let diagram = parser::parse_sequence_diagram(&source);
        assert_eq!(
            diagram.blocks.iter().map(|b| b.kind).collect::<Vec<_>>(),
            vec![
                BlockKind::Loop,
                BlockKind::Alt,
                BlockKind::Opt,
                BlockKind::Par,
                BlockKind::Critical,
                BlockKind::Break,
                BlockKind::Rect,
            ]
        );
        assert_eq!(
            diagram
                .blocks
                .iter()
                .map(|b| b.dividers.len())
                .sum::<usize>(),
            2
        );
    }

    #[test]
    fn skips_explicit_activation_commands() {
        let diagram = parser::parse_sequence_diagram(
            "sequenceDiagram\nparticipant A\nactivate A\nA->>+A: work\ndeactivate A",
        );
        assert_eq!(diagram.actors.len(), 1);
        assert_eq!(diagram.messages.len(), 1);
        assert!(diagram.messages[0].activate);
    }
}
