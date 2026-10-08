//! Optional scope indexing for brace-delimited definitions. Highlighting grammars
//! identify definition names and real block punctuation, so braces in comments,
//! strings, and ordinary expressions cannot start or end a definition.
use crate::error::Result;
use crate::line_range::{LineRange, LineRanges};
use syntect::parsing::{ParseState, Scope, ScopeStack, SyntaxReference, SyntaxSet};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Kind {
    Function,
    Type,
    Namespace,
    Block,
}

#[derive(Clone, Debug)]
struct Block {
    start: usize,
    open: usize,
    end: usize,
    kind: Kind,
}

#[derive(Default)]
struct Frame {
    block: Option<Block>,
    statement: Option<usize>,
    definition: Option<(Kind, usize)>,
}

pub(crate) struct Structure {
    blocks: Vec<Block>,
    lines: usize,
    comments: Vec<(usize, usize)>,
    imports: Vec<(usize, usize)>,
}

impl Structure {
    pub(crate) fn parse(text: &str, syntax: &SyntaxReference, set: &SyntaxSet) -> Result<Self> {
        let mut parser = ParseState::new(syntax);
        let mut scopes = ScopeStack::new();
        let mut frames = vec![Frame::default()];
        let mut blocks = Vec::new();
        let mut lines = 0;
        let objc = syntax.scope.to_string().starts_with("source.objc");
        let mut comments = Vec::new();
        let mut imports = Vec::new();
        let mut comment_start = None;
        let mut last_comment_line = 0;
        let mut import_start = None;
        // Resolve scope prefixes once, outside the input loop.
        let scope = |s| Scope::new(s).expect("valid static scope");
        let comment = scope("comment");
        let string = scope("string");
        let begin = scope("punctuation.section.block.begin");
        let end = scope("punctuation.section.block.end");
        let function = scope("entity.name.function");
        let namespace = scope("entity.name.namespace");
        let types = [
            "entity.name.class",
            "entity.name.struct",
            "entity.name.type",
            "entity.name.interface",
            "entity.name.enum",
        ]
        .map(scope);
        let preprocessor = scope("meta.preprocessor");
        let block_comment = scope("comment.block");
        let import_scopes = [
            "meta.import",
            "meta.preprocessor.include",
            "keyword.control.import",
        ]
        .map(scope);
        for (index, line) in text.split_inclusive('\n').enumerate() {
            let line_number = index + 1;
            lines = line_number;
            let operations = parser.parse_line(line, set).map_err(syntect::Error::from)?;
            let mut previous = 0;
            let mut import_line = false;
            // Inspect nonempty spans only; grammars often pop and restore a
            // scope at the same byte offset while changing parsing contexts.
            for (offset, operation) in operations
                .iter()
                .map(|(p, op)| (*p, Some(op)))
                .chain(std::iter::once((line.len(), None)))
            {
                let part = &line[previous..offset];
                let has = |prefix: Scope| scopes.as_slice().iter().any(|s| prefix.is_prefix_of(*s));
                if !part.is_empty() {
                    if has(block_comment) {
                        comment_start.get_or_insert(line_number);
                        last_comment_line = line_number;
                    } else if let Some(first) = comment_start.take() {
                        comments.push((first, last_comment_line));
                    }
                    import_line |= import_scopes.iter().any(|s| has(*s));
                }
                if !part.trim().is_empty() && !has(comment) && !has(string) {
                    let frame = frames.last_mut().expect("root frame");
                    if has(preprocessor) {
                        frame.statement = None;
                        frame.definition = None;
                    } else if has(begin) {
                        let (kind, start) = frame
                            .definition
                            .take()
                            .unwrap_or((Kind::Block, line_number));
                        frame.statement = None;
                        frames.push(Frame {
                            block: Some(Block {
                                start,
                                open: line_number,
                                end: line_number,
                                kind,
                            }),
                            ..Frame::default()
                        });
                    } else if has(end) {
                        if frames.len() > 1 {
                            let mut block =
                                frames.pop().expect("block frame").block.expect("block");
                            block.end = line_number;
                            blocks.push(block);
                        }
                        let frame = frames.last_mut().expect("root frame");
                        frame.statement = None;
                        frame.definition = None;
                    } else {
                        let start = *frame.statement.get_or_insert(line_number);
                        if has(function) {
                            // Objective-C implementation scopes have no opening
                            // brace. Its selector declaration starts on this line.
                            let start = if objc { line_number } else { start };
                            frame.definition = Some((
                                Kind::Function,
                                frame
                                    .definition
                                    .filter(|(k, _)| *k == Kind::Function)
                                    .map_or(start, |(_, n)| n),
                            ));
                        } else if has(namespace) {
                            frame.definition = Some((Kind::Namespace, start));
                        } else if types.iter().any(|s| has(*s))
                            && !matches!(frame.definition, Some((Kind::Function, _)))
                        {
                            frame.definition = Some((Kind::Type, start));
                        }
                        if part.contains(';')
                            || (part.contains(':')
                                && scopes.as_slice().iter().any(|s| {
                                    s.to_string().starts_with("punctuation.separator.access")
                                }))
                        {
                            frame.statement = None;
                            frame.definition = None;
                        }
                    }
                }
                if let Some(operation) = operation {
                    scopes.apply(operation).map_err(syntect::Error::from)?;
                }
                previous = offset;
            }
            if !scopes
                .as_slice()
                .iter()
                .any(|s| block_comment.is_prefix_of(*s))
            {
                if let Some(first) = comment_start.take() {
                    comments.push((first, last_comment_line));
                }
            }
            if import_line {
                import_start.get_or_insert(line_number);
            } else if let Some(first) = import_start.take() {
                imports.push((first, line_number - 1));
            }
        }
        if let Some(first) = import_start {
            imports.push((first, lines));
        }
        Ok(Self {
            blocks,
            lines,
            comments,
            imports,
        })
    }

    pub(crate) fn folded(&self) -> LineRanges {
        let mut hidden = Vec::new();
        hidden.extend(
            self.blocks
                .iter()
                .filter(|b| b.open + 1 < b.end)
                .map(|b| (b.open + 1, b.end - 1)),
        );
        hidden.extend(
            self.comments
                .iter()
                .filter(|(start, end)| start + 1 < *end)
                .map(|(start, end)| (start + 1, end - 1)),
        );
        hidden.extend(
            self.imports
                .iter()
                .filter(|(start, end)| start < end)
                .map(|(start, end)| (start + 1, *end)),
        );
        hidden.sort_unstable();
        let mut visible = Vec::new();
        let mut next = 1;
        for (start, end) in hidden {
            if start > next {
                visible.push(LineRange::new(next, start - 1));
            }
            next = next.max(end + 1);
        }
        if next <= self.lines {
            visible.push(LineRange::new(next, self.lines));
        }
        LineRanges::from(visible)
    }

    pub(crate) fn context(&self, selected: &[usize]) -> LineRanges {
        LineRanges::from(
            selected
                .iter()
                .filter(|&&line| line > 0 && line <= self.lines)
                .map(|&line| {
                    // The innermost complete function/type wins. Namespace-contained
                    // globals fall back to the selected line instead of the whole file.
                    self.blocks
                        .iter()
                        .filter(|b| {
                            matches!(b.kind, Kind::Function | Kind::Type)
                                && b.start <= line
                                && line <= b.end
                        })
                        .min_by_key(|b| (b.end - b.start, usize::MAX - b.start))
                        .map(|b| LineRange::new(b.start, b.end))
                        .unwrap_or_else(|| LineRange::new(line, line))
                })
                .collect::<Vec<_>>(),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assets::HighlightingAssets;
    use crate::line_range::{MaxBufferedLineNumber, RangeCheckResult};
    fn selected(language: &str, text: &str, line: usize) -> Vec<usize> {
        let assets = HighlightingAssets::from_binary();
        let set = assets.get_syntax_set().unwrap();
        let structure =
            Structure::parse(text, set.find_syntax_by_name(language).unwrap(), set).unwrap();
        let ranges = structure.context(&[line]);
        (1..=structure.lines)
            .filter(|&n| {
                ranges.check(n, MaxBufferedLineNumber::Final(structure.lines))
                    == RangeCheckResult::InRange
            })
            .collect()
    }
    #[test]
    fn c_multiline_definition_and_nested_braces() {
        let text = "#include <stdio.h>\nint global;\nstatic\nint example(\n int x)\n{\n if(x) {\n  puts(\"}\"); // {\n }\n return x;\n}\nint later;\n";
        assert_eq!(selected("C", text, 8), (3..=11).collect::<Vec<_>>());
        assert_eq!(selected("C", text, 2), vec![2]);
        assert_eq!(selected("C", text, 12), vec![12]);
    }
    #[test]
    fn cpp_nested_namespace_class_and_method() {
        let text = "namespace Demo {\nint global;\nclass Example {\n public:\n int method(int x) {\n  return x;\n }\n};\n}\n";
        assert_eq!(selected("C++", text, 6), vec![4, 5, 6, 7]);
        assert_eq!(selected("C++", text, 3), (3..=8).collect::<Vec<_>>());
        assert_eq!(selected("C++", text, 2), vec![2]);
    }
    #[test]
    fn java_method_and_class() {
        let text = "import java.io.File;\nclass Demo {\n int method(int x) {\n  return x;\n }\n}\n";
        assert_eq!(selected("Java", text, 4), vec![3, 4, 5]);
        assert_eq!(selected("Java", text, 2), vec![2, 3, 4, 5, 6]);
    }
    #[test]
    fn objc_selector_is_not_the_whole_implementation() {
        let text = "@implementation Demo\n- (int)method:(int)x {\n return x;\n}\n- (void)other {\n}\n@end\n";
        assert_eq!(selected("Objective-C", text, 3), vec![2, 3, 4]);
    }
    #[test]
    fn rust_ignores_nested_control_blocks() {
        let text = "use std::io;\nfn example() {\n if true {\n  println!(\"}\");\n }\n}\n";
        assert_eq!(selected("Rust", text, 4), vec![2, 3, 4, 5, 6]);
    }
    #[test]
    fn prototypes_and_unclosed_definitions_do_not_capture_later_input() {
        let text = "int example(int x);\nint global;\nint unfinished() {\n return 1;\n";
        assert_eq!(selected("C", text, 2), vec![2]);
        assert_eq!(selected("C", text, 4), vec![4]);
    }
    #[test]
    fn plain_text_and_out_of_bounds_fall_back_safely() {
        assert_eq!(selected("Plain Text", "hello {\nworld\n}\n", 2), vec![2]);
        assert!(selected("Plain Text", "hello", 20).is_empty());
    }
}
