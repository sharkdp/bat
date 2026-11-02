use super::{
    Actor, ActorKind, Block, BlockDivider, BlockKind, Message, MessageArrowKind, Note,
    NotePosition, SequenceDiagram,
};
use crate::mermaid::{lex::Cursor, text::normalize_label};

#[derive(Debug)]
struct OpenBlock {
    kind: BlockKind,
    label: String,
    start_index: usize,
    dividers: Vec<BlockDivider>,
}

#[derive(Clone, Copy, Debug)]
struct ParsedMessage<'a> {
    from: &'a str,
    operator: &'static str,
    activation: Option<char>,
    to: &'a str,
    label: &'a str,
}

/// `participant|actor WS+ ID (WS+ "as" WS+ LABEL)?`, where `ID` is one
/// or more non-whitespace characters and `LABEL` is non-empty.
fn parse_actor(line: &str) -> Option<(ActorKind, &str, Option<&str>)> {
    let mut cursor = Cursor::new(line);
    let kind = if cursor.eat("participant") {
        ActorKind::Participant
    } else if cursor.eat("actor") {
        ActorKind::Actor
    } else {
        return None;
    };
    if !cursor.expect_ws() {
        return None;
    }
    let id = cursor.word()?;
    if cursor.at_end() {
        return Some((kind, id, None));
    }
    if !cursor.expect_ws() || !cursor.eat("as") {
        return None;
    }
    let label = take_rest_after_required_ws(&mut cursor)?;
    Some((kind, id, Some(label)))
}

/// Case-insensitive `Note WS+ ("left of"|"right of"|"over") WS+ ACTORS
/// ":" WS* TEXT`; `ACTORS` contains at least one non-colon character and
/// `TEXT` is non-empty.
fn parse_note(line: &str) -> Option<(NotePosition, &str, &str)> {
    let mut cursor = Cursor::new(line);
    if !cursor.eat_ignore_ascii_case("Note") || !cursor.expect_ws() {
        return None;
    }
    let position = if cursor.eat_ignore_ascii_case("left of") {
        NotePosition::Left
    } else if cursor.eat_ignore_ascii_case("right of") {
        NotePosition::Right
    } else if cursor.eat_ignore_ascii_case("over") {
        NotePosition::Over
    } else {
        return None;
    };

    let whitespace_start = cursor.pos();
    let leading_whitespace = cursor.take_while(char::is_whitespace);
    if leading_whitespace.is_empty() {
        return None;
    }
    let mut actors_start = cursor.pos();
    let actors_end = {
        cursor.take_until_char(':')?;
        cursor.pos()
    };
    if actors_start == actors_end {
        let (last_offset, _) = leading_whitespace.char_indices().next_back()?;
        actors_start = whitespace_start + last_offset;
    }
    if actors_start == actors_end || !cursor.eat_char(':') {
        return None;
    }
    let text = take_rest_after_optional_ws(&mut cursor)?;
    Some((position, &line[actors_start..actors_end], text))
}

/// `loop|alt|opt|par|critical|break|rect`, followed by optional whitespace
/// and an arbitrary (possibly empty) label.
fn parse_block(line: &str) -> Option<(&str, &str)> {
    let mut cursor = Cursor::new(line);
    let keyword = cursor.eat_any(&["loop", "alt", "opt", "par", "critical", "break", "rect"])?;
    cursor.skip_ws();
    Some((keyword, cursor.take_rest()))
}

/// `else|and`, followed by optional whitespace and an arbitrary (possibly
/// empty) label.
fn parse_divider(line: &str) -> Option<(&str, &str)> {
    let mut cursor = Cursor::new(line);
    let keyword = cursor.eat_any(&["else", "and"])?;
    cursor.skip_ws();
    Some((keyword, cursor.take_rest()))
}

/// The full message grammar. The sender and receiver are lazy `non-WS+`
/// fields: each is the shortest prefix for which the remainder parses.
fn parse_message(line: &str) -> Option<ParsedMessage<'_>> {
    // Expansion order preserves the original alternatives and the greedy
    // optional dash/greater-than characters within each alternative.
    const ARROWS: &[&str] = &[
        "-->>", "-->", "->>", "->", // --?>?>
        "--)", "--x", "-)", "-x", // --?[)x]
        "-->>", "->>", // --?>>
        "-->", "->", // --?>
    ];
    parse_message_with_arrows(line, ARROWS)
}

/// The explicit simple-message alternatives, in their original order.
fn parse_simple_message(line: &str) -> Option<ParsedMessage<'_>> {
    const ARROWS: &[&str] = &["->>", "-->>", "-)", "--)", "-x", "--x", "->", "-->"];
    parse_message_with_arrows(line, ARROWS)
}

fn parse_message_with_arrows<'a>(
    line: &'a str,
    arrows: &[&'static str],
) -> Option<ParsedMessage<'a>> {
    let mut sender = Cursor::new(line);
    while sender.peek().is_some_and(|ch| !ch.is_whitespace()) {
        sender.bump();
        let sender_end = sender.pos();
        let mut arrow_start = sender;
        arrow_start.skip_ws();

        for &operator in arrows {
            let mut suffix = arrow_start;
            if !suffix.eat(operator) {
                continue;
            }
            if let Some((activation, to, label)) = parse_message_suffix(&mut suffix) {
                return Some(ParsedMessage {
                    from: &line[..sender_end],
                    operator,
                    activation,
                    to,
                    label,
                });
            }
        }
    }
    None
}

fn parse_message_suffix<'a>(cursor: &mut Cursor<'a>) -> Option<(Option<char>, &'a str, &'a str)> {
    cursor.skip_ws();
    let activation_start = cursor.pos();
    if let Some(activation @ ('+' | '-')) = cursor.peek() {
        cursor.bump();
        if let Some((to, label)) = parse_message_receiver(cursor) {
            return Some((Some(activation), to, label));
        }
        cursor.reset(activation_start);
    }
    let (to, label) = parse_message_receiver(cursor)?;
    Some((None, to, label))
}

fn parse_message_receiver<'a>(cursor: &mut Cursor<'a>) -> Option<(&'a str, &'a str)> {
    let receiver_start = cursor.pos();
    while cursor.peek().is_some_and(|ch| !ch.is_whitespace()) {
        cursor.bump();
        let receiver = cursor.since(receiver_start);
        let mut delimiter = *cursor;
        delimiter.skip_ws();
        if delimiter.eat_char(':') {
            if let Some(label) = take_rest_after_optional_ws(&mut delimiter) {
                return Some((receiver, label));
            }
        }
    }
    None
}

fn take_rest_after_required_ws<'a>(cursor: &mut Cursor<'a>) -> Option<&'a str> {
    let whitespace_start = cursor.pos();
    let whitespace = cursor.take_while(char::is_whitespace);
    if whitespace.is_empty() {
        return None;
    }
    if cursor.at_end() {
        let (last_offset, _) = whitespace.char_indices().next_back()?;
        cursor.reset(whitespace_start + last_offset);
    }
    Some(cursor.take_rest())
}

fn take_rest_after_optional_ws<'a>(cursor: &mut Cursor<'a>) -> Option<&'a str> {
    let whitespace_start = cursor.pos();
    let whitespace = cursor.take_while(char::is_whitespace);
    if cursor.at_end() && !whitespace.is_empty() {
        let (last_offset, _) = whitespace.char_indices().next_back()?;
        cursor.reset(whitespace_start + last_offset);
    }
    let rest = cursor.take_rest();
    (!rest.is_empty()).then_some(rest)
}

/// Parse Mermaid `sequenceDiagram` source into its ordered logical model.
pub fn parse_sequence_diagram(text: &str) -> SequenceDiagram {
    let lines: Vec<&str> = text
        .split('\n')
        .map(str::trim)
        .filter(|line| !line.is_empty() && !line.starts_with("%%"))
        .collect();
    let mut diagram = SequenceDiagram::default();
    let mut actor_ids = Vec::<String>::new();
    let mut block_stack = Vec::<OpenBlock>::new();

    for line in lines.iter().skip(1) {
        if let Some((kind, parsed_id, parsed_label)) = parse_actor(line) {
            let id = parsed_id.to_owned();
            let raw_label = parsed_label.map_or(id.as_str(), str::trim);
            let label = normalize_label(raw_label);
            if !actor_ids.iter().any(|known| known == &id) {
                actor_ids.push(id.clone());
                diagram.actors.push(Actor { id, label, kind });
            }
            continue;
        }

        if let Some((position, parsed_actor_ids, text)) = parse_note(line) {
            let actor_ids_for_note: Vec<String> = parsed_actor_ids
                .split(',')
                .map(|id| id.trim().to_owned())
                .collect();
            for id in &actor_ids_for_note {
                ensure_actor(&mut diagram, &mut actor_ids, id);
            }
            diagram.notes.push(Note {
                actor_ids: actor_ids_for_note,
                text: normalize_label(text.trim()),
                position,
                after_index: diagram.messages.len() as i32 - 1,
            });
            continue;
        }

        if let (Some((_keyword, label)), Some(block)) =
            (parse_divider(line), block_stack.last_mut())
        {
            block.dividers.push(BlockDivider {
                index: diagram.messages.len(),
                label: normalize_label(label.trim()),
            });
            continue;
        }

        if let Some((keyword, label)) = parse_block(line) {
            block_stack.push(OpenBlock {
                kind: BlockKind::from_keyword(keyword),
                label: normalize_label(label.trim()),
                start_index: diagram.messages.len(),
                dividers: Vec::new(),
            });
            continue;
        }

        if *line == "end" {
            if let Some(completed) = block_stack.pop() {
                diagram.blocks.push(Block {
                    kind: completed.kind,
                    label: completed.label,
                    start_index: completed.start_index,
                    end_index: diagram
                        .messages
                        .len()
                        .saturating_sub(1)
                        .max(completed.start_index),
                    dividers: completed.dividers,
                });
                continue;
            }
        }

        if let Some((parsed, arrow)) = parse_message(line)
            .or_else(|| parse_simple_message(line))
            .and_then(|parsed| {
                MessageArrowKind::from_operator(parsed.operator).map(|arrow| (parsed, arrow))
            })
        {
            let message = Message {
                from: parsed.from.to_owned(),
                to: parsed.to.to_owned(),
                label: normalize_label(parsed.label.trim()),
                arrow,
                activate: parsed.activation == Some('+'),
                deactivate: parsed.activation == Some('-'),
            };
            ensure_actor(&mut diagram, &mut actor_ids, &message.from);
            ensure_actor(&mut diagram, &mut actor_ids, &message.to);
            diagram.messages.push(message);
        }
    }

    diagram
}

fn ensure_actor(diagram: &mut SequenceDiagram, actor_ids: &mut Vec<String>, id: &str) {
    if actor_ids.iter().any(|known| known == id) {
        return;
    }
    actor_ids.push(id.to_owned());
    diagram.actors.push(Actor {
        id: id.to_owned(),
        label: id.to_owned(),
        kind: ActorKind::Participant,
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn message_capture_boundaries_follow_lazy_fields_and_arrow_backtracking() {
        for (line, expected) in [
            ("A->>B: x", ("A", "->>", None, "B", "x")),
            ("A-->>B: x", ("A", "-->>", None, "B", "x")),
            ("Alice-x Bob: y", ("Alice", "-x", None, "Bob", "y")),
            ("A-->>: x", ("A", "-->", None, ">", "x")),
            ("A->+: x", ("A", "->", None, "+", "x")),
            ("A->B:: x", ("A", "->", None, "B", ": x")),
        ] {
            let parsed = parse_message(line).unwrap();
            assert_eq!(
                (
                    parsed.from,
                    parsed.operator,
                    parsed.activation,
                    parsed.to,
                    parsed.label
                ),
                expected,
                "{line}",
            );
        }
    }
}
