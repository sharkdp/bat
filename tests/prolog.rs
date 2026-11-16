use std::collections::BTreeMap;

use syntect::parsing::{ParseState, ScopeStack, SyntaxDefinition, SyntaxSetBuilder};

#[test]
fn prolog_rules_facts_and_multiline_comments_retain_token_scopes() {
    let syntax = SyntaxDefinition::load_from_str(
        include_str!("../assets/syntaxes/02_Extra/Prolog/Prolog.sublime-syntax"),
        true,
        None,
    )
    .unwrap();
    let mut builder = SyntaxSetBuilder::new();
    builder.add(syntax);
    let syntaxes = builder.build();
    let syntax = syntaxes.find_syntax_by_name("Prolog").unwrap();
    for extension in ["pro", "prolog", "swiplrc"] {
        assert_eq!(
            syntaxes.find_syntax_by_extension(extension).unwrap().name,
            "Prolog"
        );
    }
    assert!(syntaxes.find_syntax_by_extension("pl").is_none());
    let mut parser = ParseState::new(syntax);
    let mut stack = ScopeStack::new();
    let mut scoped = BTreeMap::<String, String>::new();
    for line in include_str!("syntax-tests/source/Prolog/example.prolog").split_inclusive('\n') {
        let operations = parser.parse_line(line, &syntaxes).unwrap();
        let mut previous = 0;
        for (offset, operation) in &operations {
            for scope in stack.as_slice() {
                scoped
                    .entry(scope.to_string())
                    .or_default()
                    .push_str(&line[previous..*offset]);
            }
            stack.apply(operation).unwrap();
            previous = *offset;
        }
        for scope in stack.as_slice() {
            scoped
                .entry(scope.to_string())
                .or_default()
                .push_str(&line[previous..]);
        }
    }
    for (scope, contents) in [
        ("comment.line.percent-sign.prolog", "Facts, variables"),
        ("comment.block.prolog", "multiple lines."),
        ("constant.other.atom.quoted.prolog", "'hello world'"),
        ("constant.numeric.integer.prolog", "3.14"),
        ("entity.name.function.fact.prolog", "parent"),
        ("entity.name.function.clause.prolog", "ancestor"),
        ("variable.parameter.uppercase.prolog", "X"),
        ("keyword.operator.prolog", " is "),
        ("string.quoted.double.prolog", "\"Name: \""),
        ("keyword.control.cut.prolog", "!"),
        ("entity.name.function.dcg.prolog", "word"),
    ] {
        assert!(
            scoped
                .get(scope)
                .is_some_and(|text| text.contains(contents)),
            "missing {scope}: {contents:?}; {scoped:?}"
        );
    }
}
