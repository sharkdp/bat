use std::collections::HashMap;

use super::{
    ClassDiagram, ClassMember, ClassNamespace, ClassNode, ClassRelationship, MarkerAt,
    RelationshipType, Visibility,
};
use crate::mermaid::{lex::Cursor, text::normalize_label};

/// Parse Mermaid `classDiagram` source into its logical classes and
/// relationships.
pub fn parse_class_diagram(text: &str) -> ClassDiagram {
    let lines: Vec<&str> = text
        .split('\n')
        .map(str::trim)
        .filter(|line| !line.is_empty() && !line.starts_with("%%"))
        .collect();
    let mut diagram = ClassDiagram::default();
    let mut class_indices = HashMap::<String, usize>::new();
    let mut current_namespace: Option<ClassNamespace> = None;
    let mut current_class: Option<usize> = None;
    let mut brace_depth = 0;

    for line in lines.into_iter().skip(1) {
        if let Some(class_index) = current_class.filter(|_| brace_depth > 0) {
            if line == "}" {
                brace_depth -= 1;
                if brace_depth == 0 {
                    current_class = None;
                }
                continue;
            }

            if let Some(annotation) = parse_annotation(line) {
                diagram.classes[class_index].annotation = Some(annotation.to_owned());
                continue;
            }

            if let Some((member, is_method)) = parse_member(line) {
                let class = &mut diagram.classes[class_index];
                if is_method {
                    class.methods.push(member);
                } else {
                    class.attributes.push(member);
                }
            }
            continue;
        }

        if let Some(name) = parse_namespace(line) {
            current_namespace = Some(ClassNamespace {
                name: name.to_owned(),
                class_ids: Vec::new(),
            });
            continue;
        }

        if line == "}" && current_namespace.is_some() {
            diagram
                .namespaces
                .push(current_namespace.take().expect("namespace checked above"));
            continue;
        }

        if let Some((id, generic)) = parse_class_block(line) {
            let class_index = ensure_class(&mut diagram.classes, &mut class_indices, id);
            if let Some(generic) = generic {
                diagram.classes[class_index].label = format!("{id}<{generic}>");
            }
            current_class = Some(class_index);
            brace_depth = 1;
            if let Some(namespace) = &mut current_namespace {
                namespace.class_ids.push(id.to_owned());
            }
            continue;
        }

        if let Some((id, generic)) = parse_class_only(line) {
            let class_index = ensure_class(&mut diagram.classes, &mut class_indices, id);
            if let Some(generic) = generic {
                diagram.classes[class_index].label = format!("{id}<{generic}>");
            }
            if let Some(namespace) = &mut current_namespace {
                namespace.class_ids.push(id.to_owned());
            }
            continue;
        }

        if let Some((id, annotation)) = parse_inline_annotation(line) {
            let class_index = ensure_class(&mut diagram.classes, &mut class_indices, id);
            diagram.classes[class_index].annotation = Some(annotation.to_owned());
            continue;
        }

        if let Some((id, rest)) =
            parse_inline_member(line).filter(|(_, rest)| !relationship_in_member(rest))
        {
            let class_index = ensure_class(&mut diagram.classes, &mut class_indices, id);
            if let Some((member, is_method)) = parse_member(rest) {
                if is_method {
                    diagram.classes[class_index].methods.push(member);
                } else {
                    diagram.classes[class_index].attributes.push(member);
                }
            }
            continue;
        }

        if let Some(relationship) = parse_relationship(line) {
            ensure_class(&mut diagram.classes, &mut class_indices, &relationship.from);
            ensure_class(&mut diagram.classes, &mut class_indices, &relationship.to);
            diagram.relationships.push(relationship);
        }
    }

    diagram
}

// "namespace" ws name ws* "{", where name is one non-whitespace run.
fn parse_namespace(line: &str) -> Option<&str> {
    let mut cursor = Cursor::new(line);
    if !cursor.eat("namespace") || !cursor.expect_ws() {
        return None;
    }
    let body = cursor.take_rest().strip_suffix('{')?;
    let name = body.trim_end_matches(char::is_whitespace);
    (!name.is_empty() && name.chars().all(|c| !c.is_whitespace())).then_some(name)
}

// "class" ws id [ws* "~" generic "~"] ws* "{", with an ASCII-word generic.
fn parse_class_block(line: &str) -> Option<(&str, Option<&str>)> {
    let mut cursor = Cursor::new(line);
    if !cursor.eat("class") || !cursor.expect_ws() {
        return None;
    }
    let body = cursor.take_rest().strip_suffix('{')?;
    parse_class_declaration_body(body)
}

// "class" ws id [ws* "~" generic "~"] ws*.
fn parse_class_only(line: &str) -> Option<(&str, Option<&str>)> {
    let mut cursor = Cursor::new(line);
    if !cursor.eat("class") || !cursor.expect_ws() {
        return None;
    }
    parse_class_declaration_body(cursor.take_rest())
}

fn parse_class_declaration_body(body: &str) -> Option<(&str, Option<&str>)> {
    let body = body.trim_end_matches(char::is_whitespace);
    if let Some((without_close, open)) = body
        .strip_suffix('~')
        .and_then(|s| s.rfind('~').map(|open| (s, open)))
    {
        let generic = &without_close[open + 1..];
        let id = without_close[..open].trim_end_matches(char::is_whitespace);
        if !id.is_empty()
            && id.chars().all(|c| !c.is_whitespace())
            && !generic.is_empty()
            && generic
                .chars()
                .all(|c| c.is_ascii_alphanumeric() || c == '_')
        {
            return Some((id, Some(generic)));
        }
    }
    (!body.is_empty() && body.chars().all(|c| !c.is_whitespace())).then_some((body, None))
}

// "class" ws id ws* "{" ws* "<<" annotation ">>" ws* "}".
fn parse_inline_annotation(line: &str) -> Option<(&str, &str)> {
    let mut cursor = Cursor::new(line);
    if !cursor.eat("class") || !cursor.expect_ws() {
        return None;
    }
    let id_start = cursor.pos();
    while let Some(ch) = cursor.peek() {
        if ch == '{' || ch.is_whitespace() {
            let id_end = cursor.pos();
            if id_end == id_start {
                return None;
            }
            let candidate = cursor.pos();
            cursor.skip_ws();
            if cursor.eat_char('{') {
                cursor.skip_ws();
                if cursor.eat("<<") {
                    if let Some(annotation) = cursor.ident() {
                        if cursor.eat(">>") {
                            cursor.skip_ws();
                            if cursor.eat_char('}') && cursor.at_end() {
                                return Some((&line[id_start..id_end], annotation));
                            }
                        }
                    }
                }
            }
            cursor.reset(candidate);
            if ch.is_whitespace() {
                return None;
            }
        }
        cursor.bump();
    }
    None
}

// "<<" annotation ">>", with an ASCII-word annotation.
fn parse_annotation(line: &str) -> Option<&str> {
    let mut cursor = Cursor::new(line);
    if !cursor.eat("<<") {
        return None;
    }
    let annotation = cursor.ident()?;
    (cursor.eat(">>") && cursor.at_end()).then_some(annotation)
}

// id ws* ":" ws* member, choosing the earliest colon that leaves a non-empty
// member.
fn parse_inline_member(line: &str) -> Option<(&str, &str)> {
    let mut cursor = Cursor::new(line);
    let id_start = cursor.pos();
    while let Some(ch) = cursor.peek() {
        if ch == ':' {
            let id_end = cursor.pos();
            if id_end > id_start {
                let mut candidate = cursor;
                candidate.bump();
                if let Some(member) = capture_after_greedy_ws(candidate.rest()) {
                    return Some((&line[id_start..id_end], member));
                }
            }
        } else if ch.is_whitespace() {
            let id_end = cursor.pos();
            if id_end == id_start {
                return None;
            }
            cursor.skip_ws();
            if !cursor.eat_char(':') {
                return None;
            }
            return capture_after_greedy_ws(cursor.rest())
                .map(|member| (&line[id_start..id_end], member));
        }
        cursor.bump();
    }
    None
}

// name "(" parameters ")" [ws* return-type], using the first opening and
// closing parentheses.
fn parse_method(line: &str) -> Option<(&str, &str, Option<&str>)> {
    let mut cursor = Cursor::new(line);
    let name = cursor.take_until_char('(')?;
    if name.is_empty() {
        return None;
    }
    cursor.bump();
    let parameters = cursor.take_until_char(')')?;
    cursor.bump();
    let return_type = if cursor.at_end() {
        None
    } else {
        Some(capture_after_greedy_ws(cursor.rest())?)
    };
    Some((name, parameters, return_type))
}

fn relationship_in_member(line: &str) -> bool {
    ["<|--", "--", "*--", "o--", "-->", "..>", "..|>"]
        .iter()
        .any(|marker| line.contains(marker))
}

fn capture_after_greedy_ws(value: &str) -> Option<&str> {
    if let Some(start) = value.find(|c: char| !c.is_whitespace()) {
        Some(&value[start..])
    } else {
        value
            .char_indices()
            .next_back()
            .map(|(start, _)| &value[start..])
    }
}

fn parse_optional_cardinality<'a>(cursor: &mut Cursor<'a>) -> Option<&'a str> {
    let start = cursor.pos();
    let cardinality = cursor.quoted()?;
    if cursor.expect_ws() {
        Some(cardinality)
    } else {
        cursor.reset(start);
        None
    }
}

fn parse_relationship_target<'a>(cursor: &mut Cursor<'a>) -> Option<(&'a str, Option<&'a str>)> {
    let target_start = cursor.pos();
    while let Some(ch) = cursor.peek() {
        if ch == ':' {
            let target_end = cursor.pos();
            if target_end > target_start {
                let mut candidate = *cursor;
                candidate.bump();
                if let Some(label) = capture_after_greedy_ws(candidate.rest()) {
                    return Some((cursor.since(target_start), Some(label)));
                }
            }
        } else if ch.is_whitespace() {
            if cursor.pos() == target_start {
                return None;
            }
            let target = cursor.since(target_start);
            cursor.skip_ws();
            if !cursor.eat_char(':') {
                return None;
            }
            let label = capture_after_greedy_ws(cursor.rest())?;
            return Some((target, Some(label)));
        }
        cursor.bump();
    }
    (cursor.pos() > target_start).then(|| (cursor.since(target_start), None))
}

fn ensure_class(
    classes: &mut Vec<ClassNode>,
    indices: &mut HashMap<String, usize>,
    id: &str,
) -> usize {
    if let Some(&index) = indices.get(id) {
        return index;
    }
    let index = classes.len();
    classes.push(ClassNode {
        id: id.to_owned(),
        label: id.to_owned(),
        annotation: None,
        attributes: Vec::new(),
        methods: Vec::new(),
    });
    indices.insert(id.to_owned(), index);
    index
}

fn parse_member(line: &str) -> Option<(ClassMember, bool)> {
    let trimmed = line.trim();
    let trimmed = trimmed.strip_suffix(';').unwrap_or(trimmed);
    if trimmed.is_empty() {
        return None;
    }

    let (visibility, rest) = match trimmed.chars().next() {
        Some('+') => (Visibility::Public, trimmed[1..].trim()),
        Some('-') => (Visibility::Private, trimmed[1..].trim()),
        Some('#') => (Visibility::Protected, trimmed[1..].trim()),
        Some('~') => (Visibility::Package, trimmed[1..].trim()),
        _ => (Visibility::None, trimmed),
    };

    if let Some((name, parameters, return_type)) = parse_method(rest) {
        let raw_name = name.trim();
        let is_static = raw_name.ends_with('$') || rest.contains('$');
        let is_abstract = raw_name.ends_with('*') || rest.contains('*');
        let name = remove_classifier(raw_name);
        let params = (!parameters.trim().is_empty()).then(|| parameters.trim().to_owned());
        let member_type = return_type
            .map(str::trim)
            .filter(|value| !value.is_empty())
            .map(str::to_owned);
        let member = ClassMember {
            visibility,
            name,
            type_name: member_type,
            is_static,
            is_abstract,
            is_method: true,
            params,
        };
        return Some((member, true));
    }

    let parts: Vec<&str> = rest.split_whitespace().collect();
    let (type_name, raw_name) = if parts.len() >= 2 {
        (Some(parts[0].to_owned()), parts[1..].join(" "))
    } else {
        (None, parts.first().copied().unwrap_or(rest).to_owned())
    };
    let is_static = raw_name.ends_with('$');
    let is_abstract = raw_name.ends_with('*');
    let member = ClassMember {
        visibility,
        name: remove_classifier(&raw_name),
        type_name,
        is_static,
        is_abstract,
        is_method: false,
        params: None,
    };
    Some((member, false))
}

fn remove_classifier(name: &str) -> String {
    if name.ends_with(['$', '*']) {
        name[..name.len() - 1].to_owned()
    } else {
        name.to_owned()
    }
}

fn parse_relationship(line: &str) -> Option<ClassRelationship> {
    // from ws ["cardinality" ws] operator ws ["cardinality" ws] to [ws ":" ws
    // label]
    let mut cursor = Cursor::new(line);
    let from = cursor.word()?;
    if !cursor.expect_ws() {
        return None;
    }
    let from_cardinality = parse_optional_cardinality(&mut cursor);
    let arrow = cursor.eat_any(&[
        "<|--", "<|..", "*--", "o--", "-->", "--*", "--o", "--|>", "..>", "..|>", "<--", "<..",
        "<.", "--",
    ])?;
    if !cursor.expect_ws() {
        return None;
    }
    let to_cardinality = parse_optional_cardinality(&mut cursor);
    let (to, label) = parse_relationship_target(&mut cursor)?;
    let (relationship_type, marker_at) = parse_arrow(arrow)?;
    let normalized = |value: Option<&str>, trim: bool| {
        value
            .map(|value| if trim { value.trim() } else { value })
            .filter(|value| !value.is_empty())
            .map(normalize_label)
    };
    Some(ClassRelationship {
        from: from.to_owned(),
        to: to.to_owned(),
        relationship_type,
        marker_at,
        label: normalized(label, true),
        from_cardinality: normalized(from_cardinality, false),
        to_cardinality: normalized(to_cardinality, false),
    })
}

fn parse_arrow(arrow: &str) -> Option<(RelationshipType, MarkerAt)> {
    Some(match arrow {
        "<|--" => (RelationshipType::Inheritance, MarkerAt::From),
        "--|>" => (RelationshipType::Inheritance, MarkerAt::To),
        "<|.." => (RelationshipType::Realization, MarkerAt::From),
        "..|>" => (RelationshipType::Realization, MarkerAt::To),
        "*--" => (RelationshipType::Composition, MarkerAt::From),
        "--*" => (RelationshipType::Composition, MarkerAt::To),
        "o--" => (RelationshipType::Aggregation, MarkerAt::From),
        "--o" => (RelationshipType::Aggregation, MarkerAt::To),
        "-->" | "--" => (RelationshipType::Association, MarkerAt::To),
        "<--" => (RelationshipType::Association, MarkerAt::From),
        "..>" => (RelationshipType::Dependency, MarkerAt::To),
        "<.." => (RelationshipType::Dependency, MarkerAt::From),
        _ => return None,
    })
}
