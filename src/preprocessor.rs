use std::fmt::Write;

use crate::{
    nonprintable_notation::NonprintableNotation,
    vscreen::{EscapeSequenceOffsets, EscapeSequenceOffsetsIterator},
};

/// Expand tabs like an ANSI-enabled expand(1).
pub fn expand_tabs(line: &str, width: usize, cursor: &mut usize) -> String {
    let mut buffer = String::with_capacity(line.len() * 2);

    for seq in EscapeSequenceOffsetsIterator::new(line) {
        match seq {
            EscapeSequenceOffsets::Text { .. } => {
                let mut text = &line[seq.index_of_start()..seq.index_past_end()];
                while let Some(index) = text.find('\t') {
                    // Add previous text.
                    if index > 0 {
                        *cursor += index;
                        buffer.push_str(&text[0..index]);
                    }

                    // Add tab.
                    let spaces = width - (*cursor % width);
                    *cursor += spaces;
                    buffer.push_str(&" ".repeat(spaces));

                    // Next.
                    text = &text[index + 1..text.len()];
                }

                *cursor += text.len();
                buffer.push_str(text);
            }
            _ => {
                // Copy the ANSI escape sequence.
                buffer.push_str(&line[seq.index_of_start()..seq.index_past_end()])
            }
        }
    }

    buffer
}

fn try_parse_utf8_char(input: &[u8]) -> Option<(char, usize)> {
    let str_from_utf8 = |seq| std::str::from_utf8(seq).ok();

    let decoded = input
        .get(0..1)
        .and_then(str_from_utf8)
        .map(|c| (c, 1))
        .or_else(|| input.get(0..2).and_then(str_from_utf8).map(|c| (c, 2)))
        .or_else(|| input.get(0..3).and_then(str_from_utf8).map(|c| (c, 3)))
        .or_else(|| input.get(0..4).and_then(str_from_utf8).map(|c| (c, 4)));

    decoded.map(|(seq, n)| (seq.chars().next().unwrap(), n))
}

pub fn replace_nonprintable(
    input: &[u8],
    tab_width: usize,
    nonprintable_notation: NonprintableNotation,
) -> String {
    let mut output = String::new();

    let tab_width = if tab_width == 0 { 4 } else { tab_width };

    let mut idx = 0;
    let mut line_idx = 0;
    let len = input.len();
    while idx < len {
        if let Some((chr, skip_ahead)) = try_parse_utf8_char(&input[idx..]) {
            idx += skip_ahead;
            line_idx += 1;

            match chr {
                // space
                ' ' => output.push('·'),
                // tab
                '\t' => {
                    let tab_stop = tab_width - ((line_idx - 1) % tab_width);
                    line_idx = 0;
                    if tab_stop == 1 {
                        output.push('↹');
                    } else {
                        output.push('├');
                        output.push_str(&"─".repeat(tab_stop - 2));
                        output.push('┤');
                    }
                }
                // line feed
                '\x0A' => {
                    output.push_str(match nonprintable_notation {
                        NonprintableNotation::Caret => "^J\x0A",
                        NonprintableNotation::Unicode => "␊\x0A",
                    });
                    line_idx = 0;
                }
                // ASCII control characters
                '\x00'..='\x1F' => {
                    let c = u32::from(chr);

                    match nonprintable_notation {
                        NonprintableNotation::Caret => {
                            let caret_character = char::from_u32(0x40 + c).unwrap();
                            write!(output, "^{caret_character}").ok();
                        }

                        NonprintableNotation::Unicode => {
                            let replacement_symbol = char::from_u32(0x2400 + c).unwrap();
                            output.push(replacement_symbol)
                        }
                    }
                }
                // delete
                '\x7F' => match nonprintable_notation {
                    NonprintableNotation::Caret => output.push_str("^?"),
                    NonprintableNotation::Unicode => output.push('\u{2421}'),
                },
                // printable ASCII
                c if c.is_ascii_alphanumeric()
                    || c.is_ascii_punctuation()
                    || c.is_ascii_graphic() =>
                {
                    output.push(c)
                }
                // everything else
                c => output.push_str(&c.escape_unicode().collect::<String>()),
            }
        } else {
            write!(output, "\\x{:02X}", input[idx]).ok();
            idx += 1;
        }
    }

    output
}

/// Strips ANSI escape sequences from the input.
pub fn strip_ansi(line: &str) -> String {
    let mut buffer = String::with_capacity(line.len());

    for seq in EscapeSequenceOffsetsIterator::new(line) {
        if let EscapeSequenceOffsets::Text { .. } = seq {
            buffer.push_str(&line[seq.index_of_start()..seq.index_past_end()]);
        }
    }

    buffer
}

#[derive(Debug, PartialEq, Clone, Copy, Default)]
pub enum StripAnsiMode {
    #[default]
    Never,
    Always,
    Auto,
}

#[test]
fn test_try_parse_utf8_char() {
    assert_eq!(try_parse_utf8_char(&[0x20]), Some((' ', 1)));
    assert_eq!(try_parse_utf8_char(&[0x20, 0x20]), Some((' ', 1)));
    assert_eq!(try_parse_utf8_char(&[0x20, 0xef]), Some((' ', 1)));

    assert_eq!(try_parse_utf8_char(&[0x00]), Some(('\x00', 1)));
    assert_eq!(try_parse_utf8_char(&[0x1b]), Some(('\x1b', 1)));

    assert_eq!(try_parse_utf8_char(&[0xc3, 0xa4]), Some(('ä', 2)));
    assert_eq!(try_parse_utf8_char(&[0xc3, 0xa4, 0xef]), Some(('ä', 2)));
    assert_eq!(try_parse_utf8_char(&[0xc3, 0xa4, 0x20]), Some(('ä', 2)));

    assert_eq!(try_parse_utf8_char(&[0xe2, 0x82, 0xac]), Some(('€', 3)));
    assert_eq!(
        try_parse_utf8_char(&[0xe2, 0x82, 0xac, 0xef]),
        Some(('€', 3))
    );
    assert_eq!(
        try_parse_utf8_char(&[0xe2, 0x82, 0xac, 0x20]),
        Some(('€', 3))
    );

    assert_eq!(try_parse_utf8_char(&[0xe2, 0x88, 0xb0]), Some(('∰', 3)));

    assert_eq!(
        try_parse_utf8_char(&[0xf0, 0x9f, 0x8c, 0x82]),
        Some(('🌂', 4))
    );
    assert_eq!(
        try_parse_utf8_char(&[0xf0, 0x9f, 0x8c, 0x82, 0xef]),
        Some(('🌂', 4))
    );
    assert_eq!(
        try_parse_utf8_char(&[0xf0, 0x9f, 0x8c, 0x82, 0x20]),
        Some(('🌂', 4))
    );

    assert_eq!(try_parse_utf8_char(&[]), None);
    assert_eq!(try_parse_utf8_char(&[0xef]), None);
    assert_eq!(try_parse_utf8_char(&[0xef, 0x20]), None);
    assert_eq!(try_parse_utf8_char(&[0xf0, 0xf0]), None);
}

#[test]
fn test_strip_ansi() {
    // The sequence detection is covered by the tests in the vscreen module.
    assert_eq!(strip_ansi("no ansi"), "no ansi");
    assert_eq!(strip_ansi("\x1B[33mone"), "one");
    assert_eq!(
        strip_ansi("\x1B]1\x07multiple\x1B[J sequences"),
        "multiple sequences"
    );
}
