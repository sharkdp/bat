use syntect::parsing::{ParseState, ScopeStack, SyntaxDefinition, SyntaxSetBuilder};

#[test]
fn only_trailing_spaces_and_tabs_receive_the_error_scope() {
    let definition = SyntaxDefinition::load_from_str(
        include_str!("../assets/syntaxes/02_Extra/trailing-whitespace.sublime-syntax"),
        true,
        None,
    )
    .unwrap();
    let mut builder = SyntaxSetBuilder::new();
    builder.add(definition);
    let syntaxes = builder.build();
    let syntax = syntaxes.find_syntax_by_name("Trailing Whitespace").unwrap();
    let error_scope = "invalid.illegal.trailing-whitespace".parse().unwrap();
    for (line, expected) in [
        ("  leading and internal spaces\n", ""),
        ("\tleading\ttabs\n", ""),
        ("trailing spaces  \n", "  "),
        ("trailing tabs\t\t\n", "\t\t"),
        ("mixed \t \r\n", " \t "),
        ("\t  \n", "\t  "),
        ("clean CRLF\r\n", ""),
        ("Unicode: 中文 café\u{a0}\n", ""),
        ("no newline \t", " \t"),
        ("\n", ""),
    ] {
        let mut parser = ParseState::new(syntax);
        let mut stack = ScopeStack::new();
        let operations = parser.parse_line(line, &syntaxes).unwrap();
        let mut highlighted = String::new();
        for (index, (offset, operation)) in operations.iter().enumerate() {
            stack.apply(operation).unwrap();
            let end = operations
                .get(index + 1)
                .map_or(line.len(), |(next, _)| *next);
            if stack.as_slice().contains(&error_scope) {
                highlighted.push_str(&line[*offset..end]);
            }
        }
        assert_eq!(highlighted, expected, "{line:?}");
    }
}
