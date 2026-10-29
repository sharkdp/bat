use super::{Cardinality, ErAttribute, ErDiagram, ErEntity, ErKey, ErRelationship};
use crate::mermaid::{lex::Cursor, text::normalize_label};

/// Parse Mermaid `erDiagram` source into entities, attributes, and
/// relationships.
pub fn parse_er_diagram(text: &str) -> ErDiagram {
    let lines: Vec<&str> = text
        .split('\n')
        .map(str::trim)
        .filter(|line| !line.is_empty() && !line.starts_with("%%"))
        .collect();
    let mut diagram = ErDiagram::default();
    let mut current_entity: Option<usize> = None;

    for line in lines.into_iter().skip(1) {
        if let Some(entity_index) = current_entity {
            if line == "}" {
                current_entity = None;
                continue;
            }
            if let Some(attribute) = parse_attribute(line) {
                diagram.entities[entity_index].attributes.push(attribute);
            }
            continue;
        }

        if let Some(id) = parse_entity_block(line) {
            current_entity = Some(ensure_entity(&mut diagram.entities, id));
            continue;
        }

        if let Some(relationship) = parse_relationship_line(line) {
            ensure_entity(&mut diagram.entities, &relationship.entity1);
            ensure_entity(&mut diagram.entities, &relationship.entity2);
            diagram.relationships.push(relationship);
        }
    }

    diagram
}

fn ensure_entity(entities: &mut Vec<ErEntity>, id: &str) -> usize {
    if let Some(index) = entities.iter().position(|entity| entity.id == id) {
        return index;
    }
    entities.push(ErEntity {
        id: id.to_owned(),
        label: id.to_owned(),
        attributes: Vec::new(),
    });
    entities.len() - 1
}

/// `entity-block := non-whitespace+ whitespace* "{"`.
fn parse_entity_block(line: &str) -> Option<&str> {
    let body = line.strip_suffix('{')?;
    let mut cursor = Cursor::new(body);
    let id = cursor.word()?;
    cursor.skip_ws();
    cursor.at_end().then_some(id)
}

/// `attribute := non-whitespace+ whitespace+ non-whitespace+
///              (whitespace+ any-char+)?`.
fn parse_attribute(line: &str) -> Option<ErAttribute> {
    let mut cursor = Cursor::new(line);
    let r#type = cursor.word()?.to_owned();
    if !cursor.expect_ws() {
        return None;
    }
    let name = cursor.word()?.to_owned();
    let rest = if cursor.at_end() {
        ""
    } else {
        let whitespace_start = cursor.pos();
        if !cursor.expect_ws() {
            return None;
        }
        if cursor.at_end() {
            // The optional group still needs one character after its required
            // whitespace. With an all-whitespace suffix, the greedy whitespace
            // run backtracks by one character for that capture.
            if line[whitespace_start..].chars().count() < 2 {
                return None;
            }
            ""
        } else {
            cursor.take_rest()
        }
    };

    let parsed_comment = parse_comment(rest);
    let comment = parsed_comment.map(|(_, _, body)| normalize_label(body));
    let keys = if let Some((start, end, _)) = parsed_comment {
        let mut without_comment = String::with_capacity(rest.len() - (end - start));
        without_comment.push_str(&rest[..start]);
        without_comment.push_str(&rest[end..]);
        without_comment
            .split_whitespace()
            .filter_map(ErKey::parse)
            .collect()
    } else {
        rest.split_whitespace().filter_map(ErKey::parse).collect()
    };

    Some(ErAttribute {
        r#type,
        name,
        keys,
        comment,
    })
}

/// Find the first `comment := '"' non-quote* '"'`, returning its byte span
/// and body. The search is intentionally unanchored.
fn parse_comment(text: &str) -> Option<(usize, usize, &str)> {
    let mut cursor = Cursor::new(text);
    cursor.take_until_char('"')?;
    let start = cursor.pos();
    let body = cursor.quoted()?;
    Some((start, cursor.pos(), body))
}

/// `relationship := entity whitespace+ style whitespace+ entity
///                  whitespace* ":" whitespace* label`, where entities and
/// style are non-whitespace tokens, `label` is nonempty, and a relationship
/// style uses `--` or `..`.
fn parse_relationship_line(line: &str) -> Option<ErRelationship> {
    let mut cursor = Cursor::new(line);
    let entity1 = cursor.word()?.to_owned();
    if !cursor.expect_ws() {
        return None;
    }
    let cardinality_text = cursor.word()?;
    if !cursor.expect_ws() {
        return None;
    }

    let entity2_start = cursor.pos();
    let entity2_word = cursor.word()?;

    // The entity token is greedy. Prefer a colon after the full token; only
    // backtrack to a colon within it when that form cannot match.
    let external_colon = {
        cursor.skip_ws();
        cursor.eat_char(':').then_some(cursor.pos())
    };
    let (entity2, raw_label) = if let Some(label_start) = external_colon {
        let label = &line[label_start..];
        if label.is_empty() {
            return None;
        }
        (entity2_word, label)
    } else {
        let colon = entity2_word
            .rmatch_indices(':')
            .find(|(index, _)| *index > 0 && entity2_start + index + 1 < line.len())
            .map(|(index, _)| index)?;
        (&entity2_word[..colon], &line[entity2_start + colon + 1..])
    };

    let (left_style, separator, right_style) = parse_relationship_style(cardinality_text)?;
    if separator == "." {
        return None;
    }
    let cardinality1 = parse_cardinality(left_style)?;
    let cardinality2 = parse_cardinality(right_style)?;
    let identifying = separator == "--";

    let raw_label = strip_relationship_quotes(raw_label.trim());
    let label = normalize_label(raw_label);
    Some(ErRelationship {
        entity1,
        entity2: entity2.to_owned(),
        cardinality1,
        cardinality2,
        label,
        identifying,
    })
}

/// `relationship-style := cardinality ("--" | ".." | ".") cardinality`,
/// where each cardinality is a nonempty run of `|`, `o`, `}`, or `{`.
fn parse_relationship_style(text: &str) -> Option<(&str, &str, &str)> {
    let mut cursor = Cursor::new(text);
    let left = cursor.take_while(|c| matches!(c, '|' | 'o' | '}' | '{'));
    if left.is_empty() {
        return None;
    }
    let separator = cursor.eat_any(&["--", "..", "."])?;
    let right = cursor.take_while(|c| matches!(c, '|' | 'o' | '}' | '{'));
    if right.is_empty() || !cursor.at_end() {
        return None;
    }
    Some((left, separator, right))
}

fn strip_relationship_quotes(label: &str) -> &str {
    let without_leading = label
        .strip_prefix('"')
        .or_else(|| label.strip_prefix('\''))
        .unwrap_or(label);
    without_leading
        .strip_suffix('"')
        .or_else(|| without_leading.strip_suffix('\''))
        .unwrap_or(without_leading)
}

fn parse_cardinality(text: &str) -> Option<Cardinality> {
    let mut chars: Vec<char> = text.chars().collect();
    chars.sort_unstable();
    match chars.as_slice() {
        ['|', '|'] => Some(Cardinality::One),
        ['o', '|'] => Some(Cardinality::ZeroOne),
        ['|', '}'] | ['{', '|'] => Some(Cardinality::Many),
        ['o', '{'] => Some(Cardinality::ZeroMany),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_entity_attributes_keys_and_comments() {
        let diagram = parse_er_diagram(
            "erDiagram\n  CUSTOMER {\n    string name PK FK \"primary<br>name\"\n    int age\n    \
			 string email uk\n  }",
        );
        assert_eq!(diagram.entities.len(), 1);
        assert_eq!(
            diagram.entities[0],
            ErEntity {
                id: "CUSTOMER".into(),
                label: "CUSTOMER".into(),
                attributes: vec![
                    ErAttribute {
                        r#type: "string".into(),
                        name: "name".into(),
                        keys: vec![ErKey::Primary, ErKey::Foreign],
                        comment: Some("primary\nname".into()),
                    },
                    ErAttribute {
                        r#type: "int".into(),
                        name: "age".into(),
                        keys: vec![],
                        comment: None,
                    },
                    ErAttribute {
                        r#type: "string".into(),
                        name: "email".into(),
                        keys: vec![ErKey::Unique],
                        comment: None,
                    },
                ],
            },
        );
    }

    #[test]
    fn parses_relationship_styles_labels_and_deduplicates_entities() {
        let diagram = parse_er_diagram(
            "erDiagram\n  CUSTOMER ||--o{ ORDER : \"places<br/>often\"\n  ORDER |o..}| ITEM : \
			 'contains'",
        );
        assert_eq!(
            diagram
                .entities
                .iter()
                .map(|entity| entity.id.as_str())
                .collect::<Vec<_>>(),
            ["CUSTOMER", "ORDER", "ITEM"]
        );
        assert_eq!(diagram.relationships.len(), 2);
        assert_eq!(diagram.relationships[0].label, "places\noften");
        assert!(diagram.relationships[0].identifying);
        assert_eq!(diagram.relationships[1].label, "contains");
        assert!(!diagram.relationships[1].identifying);
    }

    #[test]
    fn parses_every_cardinality_token() {
        for (token, expected) in [
            ("||", Cardinality::One),
            ("o|", Cardinality::ZeroOne),
            ("|o", Cardinality::ZeroOne),
            ("}|", Cardinality::Many),
            ("|{", Cardinality::Many),
            ("o{", Cardinality::ZeroMany),
            ("{o", Cardinality::ZeroMany),
        ] {
            let source = format!("erDiagram\n  A {token}--|| B : relates");
            let diagram = parse_er_diagram(&source);
            assert_eq!(diagram.relationships.len(), 1, "token {token}");
            assert_eq!(
                diagram.relationships[0].cardinality1, expected,
                "token {token}"
            );
        }
        assert_eq!(parse_cardinality("oo"), None);
    }

    #[test]
    fn preserves_comment_search_and_removal_semantics() {
        let attribute = parse_attribute(r#"string id P"first"K FK "second""#).unwrap();
        assert_eq!(attribute.keys, vec![ErKey::Primary, ErKey::Foreign]);
        assert_eq!(attribute.comment.as_deref(), Some("first"));
        assert!(parse_attribute("string id ").is_none());
        assert!(parse_attribute("string id  ").is_some());
    }

    #[test]
    fn greedily_parses_relationship_entities_and_style_separators() {
        let relationship = parse_relationship_line("A ||--|| B:x : label").unwrap();
        assert_eq!(relationship.entity2, "B:x");
        assert_eq!(relationship.label, "label");

        let relationship = parse_relationship_line("A ||--|| B:x:y").unwrap();
        assert_eq!(relationship.entity2, "B:x");
        assert_eq!(relationship.label, "y");

        assert_eq!(parse_relationship_style("||.o{"), Some(("||", ".", "o{")));
        assert!(parse_relationship_line("A ||.o{ B : label").is_none());
        assert_eq!(parse_entity_block("A{B{"), Some("A{B"));
    }
}
