//! Parser for Mermaid flowcharts and state diagrams.

use super::{Direction, EdgeStyle, NodeShape};
use crate::mermaid::{
    lex::{is_word_char, Cursor},
    text::normalize_label,
    ParseError,
};

/// A parsed Mermaid flowchart or state diagram.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MermaidGraph {
    /// Diagram-wide layout direction.
    pub direction: Direction,
    /// Nodes in first-insertion order, with unique IDs.
    pub nodes: Vec<MermaidNode>,
    /// Edges in source order.
    pub edges: Vec<MermaidEdge>,
    /// Top-level subgraphs in source order.
    pub subgraphs: Vec<MermaidSubgraph>,
    /// Named class definitions and their style properties.
    pub class_defs: Vec<(String, Vec<(String, String)>)>,
    /// Node-to-class assignments in first-assignment order.
    pub class_assignments: Vec<(String, String)>,
    /// Inline node styles in first-style order.
    pub node_styles: Vec<(String, Vec<(String, String)>)>,
    /// Inline edge styles in first-style order.
    pub link_styles: Vec<(LinkStyleTarget, Vec<(String, String)>)>,
}

/// A target selected by a `linkStyle` directive.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LinkStyleTarget {
    /// All edges without a more specific style.
    Default,
    /// The edge at the given zero-based index.
    Index(usize),
}

/// A logical node before layout.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MermaidNode {
    /// Unique source identifier.
    pub id: String,
    /// Normalized display label.
    pub label: String,
    /// Shape selected by the node delimiters.
    pub shape: NodeShape,
}

/// A logical edge before routing.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MermaidEdge {
    /// Source node identifier.
    pub source: String,
    /// Target node identifier.
    pub target: String,
    /// Optional normalized edge label.
    pub label: Option<String>,
    /// Line style selected by the edge operator.
    pub style: EdgeStyle,
    /// Whether the source end has an arrowhead.
    pub has_arrow_start: bool,
    /// Whether the target end has an arrowhead.
    pub has_arrow_end: bool,
}

/// A possibly nested Mermaid subgraph or composite state.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MermaidSubgraph {
    /// Subgraph identifier.
    pub id: String,
    /// Normalized display label.
    pub label: String,
    /// Directly registered node IDs in source order.
    pub node_ids: Vec<String>,
    /// Nested subgraphs in source order.
    pub children: Vec<Self>,
    /// Optional internal layout direction.
    pub direction: Option<Direction>,
}

impl MermaidGraph {
    /// Look up a parsed node by its source identifier.
    pub fn node(&self, id: &str) -> Option<&MermaidNode> {
        self.nodes.iter().find(|node| node.id == id)
    }

    /// Return the insertion-order index of a parsed node.
    pub fn node_index(&self, id: &str) -> Option<usize> {
        self.nodes.iter().position(|node| node.id == id)
    }
}

#[derive(Clone, Copy)]
enum Header {
    Flow(Direction),
    State,
}

#[derive(Clone, Copy)]
enum Directive<'a> {
    ClassDef {
        name: &'a str,
        properties: &'a str,
    },
    ClassAssignment {
        ids: &'a str,
        class_name: &'a str,
    },
    NodeStyle {
        ids: &'a str,
        properties: &'a str,
    },
    LinkStyle {
        target: &'a str,
        properties: &'a str,
    },
    Direction(Direction),
}

#[derive(Clone, Copy)]
struct ParsedArrow<'a> {
    has_arrow_start: bool,
    style: EdgeStyle,
    has_arrow_end: bool,
    label: Option<&'a str>,
    consumed: usize,
}

#[inline]
const fn is_line_content(c: char) -> bool {
    !matches!(c, '\n' | '\r' | '\u{2028}' | '\u{2029}')
}

#[inline]
const fn is_id_char(c: char) -> bool {
    is_word_char(c) || c == '-'
}

#[inline]
fn is_state_id_char(c: char) -> bool {
    c.is_ascii_digit() || c == '_' || c.is_alphabetic()
}

#[inline]
fn is_state_ref_char(c: char) -> bool {
    is_state_id_char(c) || c == '-'
}

/// `("graph" | "flowchart") ws+ direction ws* | "stateDiagram" ("-v2")? ws*`.
fn parse_header(line: &str) -> Option<Header> {
    let mut cursor = Cursor::new(line);
    let start = cursor.pos();
    if cursor.eat_ignore_ascii_case("stateDiagram") {
        cursor.eat_ignore_ascii_case("-v2");
        cursor.skip_ws();
        if cursor.at_end() {
            return Some(Header::State);
        }
        cursor.reset(start);
    }
    if !(cursor.eat_ignore_ascii_case("graph") || cursor.eat_ignore_ascii_case("flowchart"))
        || !cursor.expect_ws()
    {
        return None;
    }
    let direction = parse_direction_token(&mut cursor)?;
    cursor.skip_ws();
    cursor.at_end().then_some(Header::Flow(direction))
}

fn parse_direction_token(cursor: &mut Cursor<'_>) -> Option<Direction> {
    for (token, direction) in [
        ("TD", Direction::TD),
        ("TB", Direction::TB),
        ("LR", Direction::LR),
        ("BT", Direction::BT),
        ("RL", Direction::RL),
    ] {
        let start = cursor.pos();
        if cursor.eat_ignore_ascii_case(token) {
            return Some(direction);
        }
        cursor.reset(start);
    }
    None
}

/// Parse flowchart styling and direction directives in their source precedence.
fn parse_directive(line: &str) -> Option<Directive<'_>> {
    parse_class_def(line)
        .or_else(|| parse_class_assignment(line))
        .or_else(|| parse_node_style(line))
        .or_else(|| parse_link_style_directive(line))
        .or_else(|| parse_direction_directive(line))
}

fn parse_class_def(line: &str) -> Option<Directive<'_>> {
    let mut cursor = Cursor::new(line);
    if !cursor.eat("classDef") || !cursor.expect_ws() {
        return None;
    }
    let name = cursor.ident()?;
    if !cursor.expect_ws() {
        return None;
    }
    let properties = cursor.take_while(is_line_content);
    (!properties.is_empty() && cursor.at_end()).then_some(Directive::ClassDef { name, properties })
}

fn parse_class_assignment(line: &str) -> Option<Directive<'_>> {
    let mut cursor = Cursor::new(line);
    if !cursor.eat("class") || !cursor.expect_ws() {
        return None;
    }
    let ids = cursor.take_while(|c| is_id_char(c) || c == ',');
    if ids.is_empty() || !cursor.expect_ws() {
        return None;
    }
    let class_name = cursor.ident()?;
    cursor
        .at_end()
        .then_some(Directive::ClassAssignment { ids, class_name })
}

fn parse_node_style(line: &str) -> Option<Directive<'_>> {
    let mut cursor = Cursor::new(line);
    if !cursor.eat("style") || !cursor.expect_ws() {
        return None;
    }
    let ids = cursor.take_while(|c| is_id_char(c) || c == ',');
    if ids.is_empty() || !cursor.expect_ws() {
        return None;
    }
    let properties = cursor.take_while(is_line_content);
    (!properties.is_empty() && cursor.at_end()).then_some(Directive::NodeStyle { ids, properties })
}

fn parse_link_style_directive(line: &str) -> Option<Directive<'_>> {
    let mut cursor = Cursor::new(line);
    if !cursor.eat("linkStyle") || !cursor.expect_ws() {
        return None;
    }
    let body_start = cursor.pos();

    // The `default` alternative precedes the numeric target alternative.
    if cursor.eat("default") && cursor.expect_ws() {
        let properties = cursor.take_while(is_line_content);
        if !properties.is_empty() && cursor.at_end() {
            return Some(Directive::LinkStyle {
                target: "default",
                properties,
            });
        }
    }
    cursor.reset(body_start);

    // target := [0-9,ws]+ (greedy); separator := ws+; properties := line-char+.
    let allowed_end = {
        cursor.take_while(|c| c.is_ascii_digit() || c == ',' || c.is_whitespace());
        cursor.pos()
    };
    let mut split = allowed_end;
    while split > body_start {
        let previous = line[..split].char_indices().next_back()?.0;
        split = previous;
        if line[split..]
            .chars()
            .next()
            .is_some_and(char::is_whitespace)
        {
            let mut separator = Cursor::new(&line[split..]);
            separator.skip_ws();
            let properties = separator.rest();
            if !properties.is_empty() && properties.chars().all(is_line_content) {
                return Some(Directive::LinkStyle {
                    target: &line[body_start..split],
                    properties,
                });
            }
        }
    }
    None
}

fn parse_direction_directive(line: &str) -> Option<Directive<'_>> {
    let mut cursor = Cursor::new(line);
    if !cursor.eat_ignore_ascii_case("direction") || !cursor.expect_ws() {
        return None;
    }
    let direction = parse_direction_token(&mut cursor)?;
    cursor.skip_ws();
    cursor.at_end().then_some(Directive::Direction(direction))
}

/// `subgraph ws+ line-char+`, with `id ws* "[" line-char+ "]"` as its label
/// form.
fn parse_subgraph_start(line: &str) -> Option<(String, String)> {
    let mut cursor = Cursor::new(line);
    if !cursor.eat("subgraph") || !cursor.expect_ws() {
        return None;
    }
    let rest = cursor.take_while(is_line_content);
    if rest.is_empty() || !cursor.at_end() {
        return None;
    }
    let rest = rest.trim();
    if let Some((id, label)) = parse_bracketed_subgraph(rest) {
        return Some((id.to_owned(), normalize_label(label)));
    }

    let mut id = String::with_capacity(rest.len());
    let mut in_whitespace = false;
    for c in rest.chars() {
        if c.is_whitespace() {
            if !in_whitespace {
                id.push('_');
                in_whitespace = true;
            }
        } else {
            in_whitespace = false;
            if is_word_char(c) {
                id.push(c);
            }
        }
    }
    Some((id, normalize_label(rest)))
}

fn parse_bracketed_subgraph(rest: &str) -> Option<(&str, &str)> {
    let mut cursor = Cursor::new(rest);
    let id = cursor.take_while(is_id_char);
    if id.is_empty() {
        return None;
    }
    cursor.skip_ws();
    if !cursor.eat_char('[') || !rest.ends_with(']') {
        return None;
    }
    let label_start = cursor.pos();
    let label_end = rest.len() - 1;
    let label = &rest[label_start..label_end];
    (!label.is_empty() && label.chars().all(is_line_content)).then_some((id, label))
}

fn parse_composite_state(line: &str) -> Option<(&str, Option<&str>)> {
    let mut cursor = Cursor::new(line);
    if !cursor.eat("state") || !cursor.expect_ws() {
        return None;
    }
    let mut label = None;
    if cursor.peek() == Some('"') {
        label = Some(cursor.quoted()?);
        if label == Some("") || !cursor.expect_ws() || !cursor.eat("as") || !cursor.expect_ws() {
            return None;
        }
    }
    let id = cursor.take_while(is_state_id_char);
    if id.is_empty() {
        return None;
    }
    cursor.skip_ws();
    (cursor.eat_char('{') && cursor.at_end()).then_some((id, label))
}

fn parse_state_alias(line: &str) -> Option<(&str, &str)> {
    let mut cursor = Cursor::new(line);
    if !cursor.eat("state") || !cursor.expect_ws() {
        return None;
    }
    let label = cursor.quoted()?;
    if label.is_empty() || !cursor.expect_ws() || !cursor.eat("as") || !cursor.expect_ws() {
        return None;
    }
    let id = cursor.take_while(is_state_id_char);
    if id.is_empty() {
        return None;
    }
    cursor.skip_ws();
    cursor.at_end().then_some((id, label))
}

fn parse_state_ref<'a>(cursor: &mut Cursor<'a>) -> Option<&'a str> {
    if cursor.eat("[*]") {
        return Some("[*]");
    }
    let id = cursor.take_while(is_state_ref_char);
    (!id.is_empty()).then_some(id)
}

fn parse_state_transition(line: &str) -> Option<(&str, &str, Option<&str>)> {
    // source ws* "-->" ws* target (ws* ":" ws* line-char+)?.
    // Source IDs admit `-`, so find the arrow at the first `>` and backtrack its
    // two dashes.
    let arrow_end = line.find('>')? + 1;
    if arrow_end < 3
        || !line.is_char_boundary(arrow_end - 3)
        || &line[arrow_end - 3..arrow_end] != "-->"
    {
        return None;
    }
    let mut source_cursor = Cursor::new(&line[..arrow_end - 3]);
    let source = parse_state_ref(&mut source_cursor)?;
    source_cursor.skip_ws();
    if !source_cursor.at_end() {
        return None;
    }

    let mut cursor = Cursor::new(&line[arrow_end..]);
    cursor.skip_ws();
    let target = parse_state_ref(&mut cursor)?;
    cursor.skip_ws();
    if cursor.at_end() {
        return Some((source, target, None));
    }
    if !cursor.eat_char(':') {
        return None;
    }
    cursor.skip_ws();
    let label = cursor.take_while(is_line_content);
    (!label.is_empty() && cursor.at_end()).then_some((source, target, Some(label)))
}

fn parse_state_description(line: &str) -> Option<(&str, &str)> {
    let mut cursor = Cursor::new(line);
    let id = cursor.take_while(is_state_ref_char);
    if id.is_empty() {
        return None;
    }
    cursor.skip_ws();
    if !cursor.eat_char(':') {
        return None;
    }
    cursor.skip_ws();
    let description = cursor.take_while(is_line_content);
    (!description.is_empty() && cursor.at_end()).then_some((id, description))
}

fn parse_arrow(text: &str) -> Option<ParsedArrow<'_>> {
    parse_symbol_arrow(text).or_else(|| parse_text_arrow(text))
}

fn parse_symbol_arrow(text: &str) -> Option<ParsedArrow<'_>> {
    // arrow := "<"? ("-->" | "-.->" | "==>" | "---" | "-.-" | "===")
    //          ("|" [^|]* "|")?.
    let mut cursor = Cursor::new(text);
    let has_arrow_start = cursor.eat_char('<');
    let operator = cursor.eat_any(&["-->", "-.->", "==>", "---", "-.-", "==="])?;
    let label_start = cursor.pos();
    let label = if cursor.eat_char('|') {
        let body = cursor.take_until_char('|');
        if let Some(body) = body {
            cursor.eat_char('|');
            Some(body)
        } else {
            cursor.reset(label_start);
            None
        }
    } else {
        None
    };
    Some(ParsedArrow {
        has_arrow_start,
        style: arrow_style(operator),
        has_arrow_end: operator.ends_with('>'),
        label,
        consumed: cursor.pos(),
    })
}

fn parse_text_arrow(text: &str) -> Option<ParsedArrow<'_>> {
    // text-arrow := "<"? ("--" | "-.") ws+ any-char+? ws+
    //               ("-->" | "---" | ".->" | "-.-" | "==>" | "===").
    let mut cursor = Cursor::new(text);
    let has_arrow_start = cursor.eat_char('<');
    let open = cursor.eat_any(&["--", "-."])?;
    let initial_ws_start = cursor.pos();
    if !cursor.expect_ws() {
        return None;
    }
    let mut label_start = cursor.pos();

    // The first whitespace run is greedy. Backtrack it one character at a time,
    // then apply the lazy label capture at each position.
    while label_start > initial_ws_start {
        let mut probe = Cursor::new(text);
        probe.reset(label_start);
        while let Some(c) = probe.bump() {
            if c == '\n' {
                break;
            }
            let label_end = probe.pos();
            if !probe.peek().is_some_and(char::is_whitespace) {
                continue;
            }
            let separator_start = probe.pos();
            probe.skip_ws();
            if let Some(close) = probe.eat_any(&["-->", "---", ".->", "-.-", "==>", "==="]) {
                return Some(ParsedArrow {
                    has_arrow_start,
                    style: text_arrow_style(open, close),
                    has_arrow_end: close.ends_with('>'),
                    label: Some(&text[label_start..label_end]),
                    consumed: probe.pos(),
                });
            }
            probe.reset(separator_start);
        }
        label_start = text[..label_start].char_indices().next_back()?.0;
    }
    None
}

type NodeShapeSpec<'a> = (&'a str, NodeShape);
type ParsedNodeRef<'a> = (&'a str, Option<NodeShapeSpec<'a>>, usize);

fn parse_node_ref(text: &str) -> Option<ParsedNodeRef<'_>> {
    let mut cursor = Cursor::new(text);
    let mut id = cursor.take_while(is_id_char);
    if id.is_empty() {
        return None;
    }
    if id.ends_with('-') {
        let trimmed = id.trim_end_matches('-');
        if !trimmed.is_empty() && parse_arrow(&text[trimmed.len()..]).is_some() {
            id = trimmed;
        }
    }
    let after_id = id.len();
    // Longest and most specific openers precede their prefixes.
    for (open, close, shape) in [
        ("(((", ")))", NodeShape::DoubleCircle),
        ("[[", "]]", NodeShape::Subroutine),
        ("[(", ")]", NodeShape::Cylinder),
        ("([", "])", NodeShape::Stadium),
        ("{{", "}}", NodeShape::Hexagon),
        ("[/", "\\]", NodeShape::Trapezoid),
        ("[\\", "/]", NodeShape::TrapezoidAlt),
        (">", "]", NodeShape::Asymmetric),
        ("((", "))", NodeShape::Circle),
        ("(", ")", NodeShape::Rounded),
        ("[", "]", NodeShape::Rectangle),
        ("{", "}", NodeShape::Diamond),
    ] {
        cursor.reset(after_id);
        if !cursor.eat(open) {
            continue;
        }
        let Some(label) = cursor.take_until(close) else {
            continue;
        };
        if label.is_empty() || !label.chars().all(is_line_content) {
            continue;
        }
        cursor.eat(close);
        return Some((id, Some((label, shape)), cursor.pos()));
    }
    Some((id, None, after_id))
}

fn parse_class_shorthand(text: &str) -> Option<(&str, usize)> {
    let mut cursor = Cursor::new(text);
    if !cursor.eat(":::") || !cursor.peek().is_some_and(is_word_char) {
        return None;
    }
    let mut class_name = cursor.take_while(is_id_char);
    if class_name.ends_with('-') {
        let trimmed = class_name.trim_end_matches('-');
        if !trimmed.is_empty() && parse_arrow(&text[3 + trimmed.len()..]).is_some() {
            class_name = trimmed;
        }
    }
    Some((class_name, 3 + class_name.len()))
}

/// Parse Mermaid flowchart or state-diagram source into its logical graph.
pub fn parse_flowchart(text: &str) -> Result<MermaidGraph, ParseError> {
    let lines: Vec<&str> = text
        .split('\n')
        .map(str::trim)
        .filter(|line| !line.is_empty() && !line.starts_with("%%"))
        .collect();
    let Some(header) = lines.first().copied() else {
        return Err(ParseError("Empty mermaid diagram".into()));
    };

    match parse_header(header) {
        Some(Header::State) => Ok(parse_state_diagram(&lines)),
        Some(Header::Flow(direction)) => Ok(parse_flowchart_lines(&lines, direction)),
        None => Err(ParseError(format!(
            "Invalid mermaid header: \"{header}\". Expected \"graph TD\", \"flowchart LR\", \
			 \"stateDiagram-v2\", etc."
        ))),
    }
}

fn parse_flowchart_lines(lines: &[&str], direction: Direction) -> MermaidGraph {
    let mut graph = empty_graph(direction);
    let mut stack: Vec<MermaidSubgraph> = Vec::new();

    for line in &lines[1..] {
        if let Some(directive) = parse_directive(line) {
            match directive {
                Directive::ClassDef { name, properties } => {
                    map_set(
                        &mut graph.class_defs,
                        name.to_owned(),
                        parse_style_props(properties),
                    );
                    continue;
                }
                Directive::ClassAssignment { ids, class_name } => {
                    for id in ids.split(',').map(str::trim) {
                        map_set(
                            &mut graph.class_assignments,
                            id.to_owned(),
                            class_name.to_owned(),
                        );
                    }
                    continue;
                }
                Directive::NodeStyle { ids, properties } => {
                    let properties = parse_style_props(properties);
                    for id in ids.split(',').map(str::trim) {
                        merge_style_map(&mut graph.node_styles, id.to_owned(), &properties);
                    }
                    continue;
                }
                Directive::LinkStyle { target, properties } => {
                    apply_link_style(target, properties, &mut graph);
                    continue;
                }
                Directive::Direction(direction) if !stack.is_empty() => {
                    stack.last_mut().expect("checked nonempty").direction = Some(direction);
                    continue;
                }
                Directive::Direction(_) => {}
            }
        }
        if let Some((id, label)) = parse_subgraph_start(line) {
            stack.push(MermaidSubgraph {
                id,
                label,
                node_ids: Vec::new(),
                children: Vec::new(),
                direction: None,
            });
            continue;
        }
        if *line == "end" {
            if let Some(completed) = stack.pop() {
                if let Some(parent) = stack.last_mut() {
                    parent.children.push(completed);
                } else {
                    graph.subgraphs.push(completed);
                }
            }
            continue;
        }
        parse_edge_line(line, &mut graph, &mut stack);
    }

    graph
}

fn parse_state_diagram(lines: &[&str]) -> MermaidGraph {
    let mut graph = empty_graph(Direction::TD);
    let mut stack: Vec<MermaidSubgraph> = Vec::new();
    let mut composite_ids = Vec::<String>::new();
    let mut start_count = 0usize;
    let mut end_count = 0usize;

    for line in &lines[1..] {
        if let Some(Directive::Direction(direction)) = parse_direction_directive(line) {
            if let Some(current) = stack.last_mut() {
                current.direction = Some(direction);
            } else {
                graph.direction = direction;
            }
            continue;
        }
        if let Some(Directive::LinkStyle { target, properties }) = parse_link_style_directive(line)
        {
            apply_link_style(target, properties, &mut graph);
            continue;
        }
        if let Some((id, label)) = parse_composite_state(line) {
            let id = id.to_owned();
            let label = label.map_or_else(|| id.clone(), str::to_owned);
            stack.push(MermaidSubgraph {
                id: id.clone(),
                label,
                node_ids: Vec::new(),
                children: Vec::new(),
                direction: None,
            });
            if !composite_ids.iter().any(|existing| existing == &id) {
                composite_ids.push(id.clone());
            }
            if let Some(index) = graph.node_index(&id) {
                graph.nodes.remove(index);
            }
            continue;
        }
        if *line == "}" {
            if let Some(completed) = stack.pop() {
                if let Some(parent) = stack.last_mut() {
                    parent.children.push(completed);
                } else {
                    graph.subgraphs.push(completed);
                }
            }
            continue;
        }
        if let Some((id, label)) = parse_state_alias(line) {
            register_state_node(
                &mut graph,
                &mut stack,
                MermaidNode {
                    id: id.to_owned(),
                    label: normalize_label(label),
                    shape: NodeShape::Rounded,
                },
            );
            continue;
        }
        if let Some((source, target, raw_label)) = parse_state_transition(line) {
            let mut source = source.to_owned();
            let mut target = target.to_owned();
            let label = raw_label.and_then(|raw| {
                let raw = raw.trim();
                (!raw.is_empty()).then(|| normalize_label(raw))
            });

            if source == "[*]" {
                start_count += 1;
                source = numbered_pseudostate("_start", start_count);
                register_state_node(
                    &mut graph,
                    &mut stack,
                    MermaidNode {
                        id: source.clone(),
                        label: String::new(),
                        shape: NodeShape::StateStart,
                    },
                );
            } else if !composite_ids.iter().any(|id| id == &source) {
                ensure_state_node(&mut graph, &mut stack, &source);
            }

            if target == "[*]" {
                end_count += 1;
                target = numbered_pseudostate("_end", end_count);
                register_state_node(
                    &mut graph,
                    &mut stack,
                    MermaidNode {
                        id: target.clone(),
                        label: String::new(),
                        shape: NodeShape::StateEnd,
                    },
                );
            } else if !composite_ids.iter().any(|id| id == &target) {
                ensure_state_node(&mut graph, &mut stack, &target);
            }

            graph.edges.push(MermaidEdge {
                source,
                target,
                label,
                style: EdgeStyle::Solid,
                has_arrow_start: false,
                has_arrow_end: true,
            });
            continue;
        }
        if let Some((id, description)) = parse_state_description(line) {
            register_state_node(
                &mut graph,
                &mut stack,
                MermaidNode {
                    id: id.to_owned(),
                    label: normalize_label(description.trim()),
                    shape: NodeShape::Rounded,
                },
            );
        }
    }

    graph
}

const fn empty_graph(direction: Direction) -> MermaidGraph {
    MermaidGraph {
        direction,
        nodes: Vec::new(),
        edges: Vec::new(),
        subgraphs: Vec::new(),
        class_defs: Vec::new(),
        class_assignments: Vec::new(),
        node_styles: Vec::new(),
        link_styles: Vec::new(),
    }
}

fn apply_link_style(target: &str, raw_properties: &str, graph: &mut MermaidGraph) {
    let target = target.trim();
    let properties = parse_style_props(raw_properties);
    if target == "default" {
        merge_style_map(
            &mut graph.link_styles,
            LinkStyleTarget::Default,
            &properties,
        );
    } else {
        for index in target
            .split(',')
            .filter_map(|value| value.trim().parse::<usize>().ok())
        {
            merge_style_map(
                &mut graph.link_styles,
                LinkStyleTarget::Index(index),
                &properties,
            );
        }
    }
}

fn parse_style_props(properties: &str) -> Vec<(String, String)> {
    let cleaned = properties.strip_suffix(';').unwrap_or(properties);
    let mut parsed = Vec::new();
    for pair in cleaned.split(',') {
        let Some(colon) = pair.find(':') else {
            continue;
        };
        if colon == 0 {
            continue;
        }
        let key = pair[..colon].trim();
        let value = pair[colon + 1..].trim();
        if !key.is_empty() && !value.is_empty() {
            map_set(&mut parsed, key.to_owned(), value.to_owned());
        }
    }
    parsed
}

fn map_set<K: PartialEq, V>(entries: &mut Vec<(K, V)>, key: K, value: V) {
    if let Some((_, current)) = entries.iter_mut().find(|(existing, _)| existing == &key) {
        *current = value;
    } else {
        entries.push((key, value));
    }
}

fn merge_style_map<K: PartialEq + Clone>(
    entries: &mut Vec<(K, Vec<(String, String)>)>,
    key: K,
    properties: &[(String, String)],
) {
    if let Some((_, current)) = entries.iter_mut().find(|(existing, _)| existing == &key) {
        for (name, value) in properties {
            map_set(current, name.clone(), value.clone());
        }
    } else {
        entries.push((key, properties.to_vec()));
    }
}

fn register_state_node(graph: &mut MermaidGraph, stack: &mut [MermaidSubgraph], node: MermaidNode) {
    let id = node.id.clone();
    if graph.node(&id).is_none() {
        graph.nodes.push(node);
    }
    track_in_subgraph(stack, &id);
}

fn ensure_state_node(graph: &mut MermaidGraph, stack: &mut [MermaidSubgraph], id: &str) {
    if graph.node(id).is_none() {
        register_state_node(
            graph,
            stack,
            MermaidNode {
                id: id.to_owned(),
                label: id.to_owned(),
                shape: NodeShape::Rounded,
            },
        );
    } else {
        track_in_subgraph(stack, id);
    }
}

fn numbered_pseudostate(prefix: &str, count: usize) -> String {
    if count == 1 {
        prefix.to_owned()
    } else {
        format!("{prefix}{count}")
    }
}

fn parse_edge_line(line: &str, graph: &mut MermaidGraph, stack: &mut [MermaidSubgraph]) {
    let Some((mut previous, mut remaining)) = consume_node_group(line.trim(), graph, stack) else {
        return;
    };

    while !remaining.is_empty() {
        let Some(arrow) = parse_arrow(remaining) else {
            break;
        };
        let label = arrow.label.and_then(|raw| {
            let value = raw.trim();
            (!value.is_empty()).then(|| normalize_label(value))
        });
        remaining = remaining[arrow.consumed..].trim();
        let Some((next, rest)) = consume_node_group(remaining, graph, stack) else {
            break;
        };
        remaining = rest;

        for source in &previous {
            for target in &next {
                graph.edges.push(MermaidEdge {
                    source: source.clone(),
                    target: target.clone(),
                    label: label.clone(),
                    style: arrow.style,
                    has_arrow_start: arrow.has_arrow_start,
                    has_arrow_end: arrow.has_arrow_end,
                });
            }
        }
        previous = next;
    }
}

fn consume_node_group<'a>(
    text: &'a str,
    graph: &mut MermaidGraph,
    stack: &mut [MermaidSubgraph],
) -> Option<(Vec<String>, &'a str)> {
    let (first, mut remaining) = consume_node(text, graph, stack)?;
    let mut ids = vec![first];
    remaining = remaining.trim();
    while let Some(after_ampersand) = remaining.strip_prefix('&') {
        remaining = after_ampersand.trim();
        let Some((next, rest)) = consume_node(remaining, graph, stack) else {
            break;
        };
        ids.push(next);
        remaining = rest.trim();
    }
    Some((ids, remaining))
}

fn consume_node<'a>(
    text: &'a str,
    graph: &mut MermaidGraph,
    stack: &mut [MermaidSubgraph],
) -> Option<(String, &'a str)> {
    let (raw_id, shaped, consumed) = parse_node_ref(text)?;
    let id = raw_id.to_owned();
    if let Some((label, shape)) = shaped {
        register_node(
            graph,
            stack,
            MermaidNode {
                id: id.clone(),
                label: normalize_label(label),
                shape,
            },
        );
    } else if graph.node(&id).is_none() {
        register_node(
            graph,
            stack,
            MermaidNode {
                id: id.clone(),
                label: id.clone(),
                shape: NodeShape::Rectangle,
            },
        );
    }
    let mut remaining = &text[consumed..];

    if let Some((class_name, class_consumed)) = parse_class_shorthand(remaining) {
        map_set(
            &mut graph.class_assignments,
            id.clone(),
            class_name.to_owned(),
        );
        remaining = &remaining[class_consumed..];
    }
    Some((id, remaining))
}

fn register_node(graph: &mut MermaidGraph, stack: &mut [MermaidSubgraph], node: MermaidNode) {
    let id = node.id.clone();
    if graph.node(&id).is_none() {
        graph.nodes.push(node);
    }
    track_in_subgraph(stack, &id);
}

fn track_in_subgraph(stack: &mut [MermaidSubgraph], id: &str) {
    if let Some(current) = stack.last_mut() {
        if !current.node_ids.iter().any(|existing| existing == id) {
            current.node_ids.push(id.to_owned());
        }
    }
}

fn arrow_style(operator: &str) -> EdgeStyle {
    match operator {
        "-.->" | "-.-" => EdgeStyle::Dotted,
        "==>" | "===" => EdgeStyle::Thick,
        _ => EdgeStyle::Solid,
    }
}

fn text_arrow_style(open: &str, close: &str) -> EdgeStyle {
    if open == "-." || close == ".->" || close == "-.-" {
        EdgeStyle::Dotted
    } else if open == "==" || close == "==>" || close == "===" {
        EdgeStyle::Thick
    } else {
        EdgeStyle::Solid
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_headers_and_exact_errors() {
        assert_eq!(
            parse_flowchart("graph TD").unwrap().direction,
            Direction::TD
        );
        assert_eq!(
            parse_flowchart("FLOWCHART lr").unwrap().direction,
            Direction::LR
        );
        assert_eq!(
            parse_flowchart("stateDiagram-v2").unwrap().direction,
            Direction::TD
        );
        assert_eq!(
            parse_flowchart("statediagram").unwrap().direction,
            Direction::TD
        );
        assert_eq!(
            parse_flowchart("\n %% only a comment \n").unwrap_err().0,
            "Empty mermaid diagram"
        );
        assert_eq!(
			parse_flowchart("sequenceDiagram").unwrap_err().0,
			"Invalid mermaid header: \"sequenceDiagram\". Expected \"graph TD\", \"flowchart LR\", \
			 \"stateDiagram-v2\", etc."
		);
    }

    #[test]
    fn preserves_greedy_and_case_insensitive_boundaries() {
        let uppercase_state = parse_flowchart("STATEDIAGRAM-V2\nA---->B").unwrap();
        assert_eq!(
            uppercase_state
                .edges
                .first()
                .map(|edge| (edge.source.as_str(), edge.target.as_str())),
            Some(("A--", "B"))
        );

        let whitespace_label = parse_flowchart("graph LR\nA --   --> B").unwrap();
        assert_eq!(whitespace_label.edges.len(), 1);
        assert_eq!(whitespace_label.edges[0].label, None);
    }

    #[test]
    fn parses_every_flowchart_node_shape() {
        let graph = parse_flowchart(
            "graph TD\nA[rectangle]\nB(rounded)\nC{diamond}\nD([stadium])\nE((circle))\n\
			 F[[subroutine]]\nG(((doublecircle)))\nH{{hexagon}}\nI[(cylinder)]\nJ>asymmetric]\nK[/\
			 trapezoid\\]\nL[\\trapezoid-alt/]",
        )
        .unwrap();
        let expected = [
            NodeShape::Rectangle,
            NodeShape::Rounded,
            NodeShape::Diamond,
            NodeShape::Stadium,
            NodeShape::Circle,
            NodeShape::Subroutine,
            NodeShape::DoubleCircle,
            NodeShape::Hexagon,
            NodeShape::Cylinder,
            NodeShape::Asymmetric,
            NodeShape::Trapezoid,
            NodeShape::TrapezoidAlt,
        ];
        assert_eq!(
            graph
                .nodes
                .iter()
                .map(|node| node.shape)
                .collect::<Vec<_>>(),
            expected
        );
    }

    #[test]
    fn parses_chains_and_ampersand_cartesian_products() {
        let graph = parse_flowchart("graph LR\nA --> B --> C\nA & B --> C & D").unwrap();
        let pairs: Vec<_> = graph
            .edges
            .iter()
            .map(|edge| (edge.source.as_str(), edge.target.as_str()))
            .collect();
        assert_eq!(
            pairs,
            [
                ("A", "B"),
                ("B", "C"),
                ("A", "C"),
                ("A", "D"),
                ("B", "C"),
                ("B", "D")
            ]
        );
    }

    #[test]
    fn parses_edge_labels_styles_and_arrowheads() {
        let graph = parse_flowchart(
			"graph LR\nA -->|x| B\nB -- yes --> C\nC -.->|dot| D\nD -. maybe .-> E\nE ==> F\nF <--> \
			 G\nG <-.-> H\nH <==> I\nI --- J",
		)
		.unwrap();
        assert_eq!(
            graph
                .edges
                .iter()
                .map(|edge| edge.label.as_deref())
                .collect::<Vec<_>>(),
            [
                Some("x"),
                Some("yes"),
                Some("dot"),
                Some("maybe"),
                None,
                None,
                None,
                None,
                None
            ]
        );
        assert_eq!(
            graph
                .edges
                .iter()
                .map(|edge| edge.style)
                .collect::<Vec<_>>(),
            [
                EdgeStyle::Solid,
                EdgeStyle::Solid,
                EdgeStyle::Dotted,
                EdgeStyle::Dotted,
                EdgeStyle::Thick,
                EdgeStyle::Solid,
                EdgeStyle::Dotted,
                EdgeStyle::Thick,
                EdgeStyle::Solid,
            ]
        );
        assert_eq!(
            graph
                .edges
                .iter()
                .map(|edge| (edge.has_arrow_start, edge.has_arrow_end))
                .collect::<Vec<_>>(),
            [
                (false, true),
                (false, true),
                (false, true),
                (false, true),
                (false, true),
                (true, true),
                (true, true),
                (true, true),
                (false, false),
            ]
        );
    }

    #[test]
    fn parses_nested_subgraphs_and_tracks_only_innermost_membership() {
        let graph = parse_flowchart(
            "graph TD\nsubgraph outer [Outer]\ndirection LR\nA\nsubgraph inner [Inner]\ndirection \
			 BT\nA\nB[Bee]\nend\nC\nend",
        )
        .unwrap();
        let outer = &graph.subgraphs[0];
        assert_eq!(outer.direction, Some(Direction::LR));
        assert_eq!(outer.node_ids, ["A", "C"]);
        assert_eq!(outer.children[0].direction, Some(Direction::BT));
        assert_eq!(outer.children[0].node_ids, ["B"]);
    }

    #[test]
    fn parses_state_pseudostates_and_composite_blocks() {
        let graph = parse_flowchart(
            "stateDiagram-v2\n[*] --> A\nA : Later label\nnote right of A : ignored\nstate \
			 Processing {\ndirection LR\nA --> B : work\n}\nA --> [*]",
        )
        .unwrap();
        assert_eq!(graph.node("_start").unwrap().shape, NodeShape::StateStart);
        assert_eq!(graph.node("_end").unwrap().shape, NodeShape::StateEnd);
        assert_eq!(graph.node("A").unwrap().label, "A");
        assert!(graph.node("note").is_none());
        assert!(graph.node("Processing").is_none());
        assert_eq!(graph.subgraphs[0].id, "Processing");
        assert_eq!(graph.subgraphs[0].node_ids, ["A", "B"]);
        assert_eq!(graph.subgraphs[0].direction, Some(Direction::LR));
        assert_eq!(graph.edges[1].label.as_deref(), Some("work"));
    }

    #[test]
    fn captures_class_definitions_assignments_and_merged_styles() {
        let graph = parse_flowchart(
			"graph TD\nA[Alpha]:::hot\nB\nclassDef hot fill:#f00,stroke:#333;\nclass A,B cold\nstyle \
			 A fill:#fff\nstyle A stroke:#000\nlinkStyle 0, 1 stroke:#abc",
		)
		.unwrap();
        assert_eq!(
            graph.class_defs,
            [(
                "hot".into(),
                vec![
                    ("fill".into(), "#f00".into()),
                    ("stroke".into(), "#333".into())
                ]
            )]
        );
        assert_eq!(
            graph.class_assignments,
            [("A".into(), "cold".into()), ("B".into(), "cold".into())]
        );
        assert_eq!(
            graph.node_styles[0].1,
            [
                ("fill".into(), "#fff".into()),
                ("stroke".into(), "#000".into())
            ]
        );
        assert_eq!(graph.link_styles.len(), 2);
    }

    #[test]
    fn first_node_declaration_wins_but_explicit_references_join_subgraphs() {
        let graph = parse_flowchart("graph TD\nA --> A[Later]\nsubgraph S\nA\nA[Still later]\nend")
            .unwrap();
        assert_eq!(graph.node("A").unwrap().label, "A");
        assert_eq!(graph.node("A").unwrap().shape, NodeShape::Rectangle);
        assert_eq!(graph.subgraphs[0].node_ids, ["A"]);
    }

    #[test]
    fn state_transition_handles_multibyte_chars_before_arrow_without_panic() {
        let invalid = parse_flowchart("stateDiagram-v2\nxé->y").unwrap();
        assert!(invalid.edges.is_empty());

        let valid = parse_flowchart("stateDiagram-v2\nxé-->y").unwrap();
        assert_eq!(valid.edges.len(), 1);
        assert_eq!(valid.edges[0].source, "xé");
        assert_eq!(valid.edges[0].target, "y");
    }

    #[test]
    fn flowchart_backtracks_trailing_dashes_on_spaceless_arrows() {
        let simple = parse_flowchart("graph TD\nA-->B").unwrap();
        assert_eq!(
            simple
                .nodes
                .iter()
                .map(|n| (n.id.as_str(), n.label.as_str()))
                .collect::<Vec<_>>(),
            [("A", "A"), ("B", "B")]
        );
        assert_eq!(simple.edges.len(), 1);
        assert_eq!(simple.edges[0].source, "A");
        assert_eq!(simple.edges[0].target, "B");

        let labeled = parse_flowchart("graph LR\nA-->|lbl|B\nB-.->C").unwrap();
        assert_eq!(
            labeled
                .nodes
                .iter()
                .map(|n| n.id.as_str())
                .collect::<Vec<_>>(),
            ["A", "B", "C"]
        );
        assert_eq!(labeled.edges.len(), 2);
        assert_eq!(labeled.edges[0].source, "A");
        assert_eq!(labeled.edges[0].target, "B");
        assert_eq!(labeled.edges[0].label.as_deref(), Some("lbl"));
        assert_eq!(labeled.edges[1].source, "B");
        assert_eq!(labeled.edges[1].target, "C");
        assert_eq!(labeled.edges[1].style, EdgeStyle::Dotted);
    }
}
