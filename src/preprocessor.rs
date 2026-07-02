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

/// Strips ANSI escape sequences and substitutes terminal-active control bytes
/// and visual-spoofing Unicode codepoints (bidi, zero-width) with U+FFFD.
pub fn sanitize(line: &str) -> String {
    let stripped = strip_ansi(line);
    let mut buffer = String::with_capacity(stripped.len());
    let bytes = stripped.as_bytes();
    let mut start = 0;
    let mut i = 0;
    // Skip directly to the next trigger byte instead of testing each one.
    while let Some(off) = bytes[i..].iter().position(|&b| is_sanitize_trigger(b)) {
        i += off;
        let len = sanitize_at(bytes, i, &stripped, &mut buffer, &mut start);
        i += len;
    }
    buffer.push_str(&stripped[start..]);
    buffer
}

#[inline]
fn is_sanitize_trigger(b: u8) -> bool {
    // C0 controls minus \t \n \f; DEL; UTF-8 leads with dangerous codepoints.
    matches!(b, 0x00..=0x08 | 0x0B | 0x0D..=0x1F | 0x7F | 0xC2 | 0xE2 | 0xEF)
}

/// Substitutes the byte/sequence at `bytes[i]` (or passes it through on
/// false-alarm trigger), flushing the prefix from `start`. Returns bytes consumed.
fn sanitize_at(
    bytes: &[u8],
    i: usize,
    full: &str,
    buffer: &mut String,
    start: &mut usize,
) -> usize {
    buffer.push_str(&full[*start..i]);
    let consumed = match bytes[i] {
        b'\r' if bytes.get(i + 1) == Some(&b'\n') => {
            buffer.push_str("\r\n");
            2
        }
        // 0xC2 leads U+0080..U+00FF; filter the C1 range.
        0xC2 if matches!(bytes.get(i + 1), Some(0x80..=0x9F)) => {
            buffer.push('\u{FFFD}');
            2
        }
        0xE2 if is_dangerous_e2(bytes, i) => {
            buffer.push('\u{FFFD}');
            3
        }
        // 0xEF 0xBB 0xBF = U+FEFF (BOM / zero-width no-break space).
        0xEF if bytes.get(i + 1) == Some(&0xBB) && bytes.get(i + 2) == Some(&0xBF) => {
            buffer.push('\u{FFFD}');
            3
        }
        // False-alarm trigger: pass the full UTF-8 sequence through.
        lead @ (0xC2 | 0xE2 | 0xEF) => {
            let n = utf8_len_from_lead(lead);
            buffer.push_str(&full[i..i + n]);
            n
        }
        _ => {
            buffer.push('\u{FFFD}');
            1
        }
    };
    *start = i + consumed;
    consumed
}

#[inline]
fn is_dangerous_e2(bytes: &[u8], i: usize) -> bool {
    // U+200B..D (zero-width), U+202A..E (bidi controls), U+2066..9 (isolates).
    matches!(
        (bytes.get(i + 1), bytes.get(i + 2)),
        (Some(0x80), Some(0x8B..=0x8D | 0xAA..=0xAE)) | (Some(0x81), Some(0xA6..=0xA9))
    )
}

#[inline]
fn utf8_len_from_lead(lead: u8) -> usize {
    if lead < 0x80 {
        1
    } else if lead < 0xE0 {
        2
    } else if lead < 0xF0 {
        3
    } else {
        4
    }
}

/// Escape C0, DEL, and C1 control characters so a string from an untrusted
/// filename or path can be safely written to the terminal.
pub fn sanitize_for_terminal(input: &str) -> String {
    if !input
        .chars()
        .any(|c| matches!(c, '\x00'..='\x08' | '\x0A'..='\x1F' | '\x7F'..='\u{9F}'))
    {
        return input.to_owned();
    }

    let mut out = String::with_capacity(input.len() + 8);
    for c in input.chars() {
        match c {
            '\t' => out.push('\t'),
            '\x00'..='\x1F' => {
                out.push('^');
                out.push(char::from_u32(0x40 + c as u32).unwrap_or('?'));
            }
            '\x7F' => out.push_str("^?"),
            '\u{80}'..='\u{9F}' => {
                use std::fmt::Write as _;
                let _ = write!(out, "\\u{{{:x}}}", c as u32);
            }
            other => out.push(other),
        }
    }
    out
}

/// Strips overstrike sequences (backspace formatting) from input.
///
/// Overstrike formatting is used by man pages and some help output:
/// - Bold: `X\x08X` (character, backspace, same character)
/// - Underline: `_\x08X` (underscore, backspace, character)
///
/// This function removes these sequences, keeping only the visible character.
/// `first_backspace` is the position of the first backspace in the line.
pub fn strip_overstrike(line: &str, first_backspace: usize) -> String {
    let mut output = String::with_capacity(line.len());
    output.push_str(&line[..first_backspace]);
    pop_visible_char(&mut output);

    let mut remaining = &line[first_backspace + 1..];

    loop {
        if let Some(pos) = remaining.find('\x08') {
            output.push_str(&remaining[..pos]);
            pop_visible_char(&mut output);
            remaining = &remaining[pos + 1..];
        } else {
            output.push_str(remaining);
            break;
        }
    }

    output
}

fn pop_visible_char(output: &mut String) {
    let mut char_to_remove = None;

    for seq in EscapeSequenceOffsetsIterator::new(output) {
        if let EscapeSequenceOffsets::Text { .. } = seq {
            let start = seq.index_of_start();
            let text = &output[start..seq.index_past_end()];

            if let Some((relative_start, chr)) = text.char_indices().next_back() {
                let char_start = start + relative_start;
                char_to_remove = Some((char_start, char_start + chr.len_utf8()));
            }
        }
    }

    if let Some((start, end)) = char_to_remove {
        output.replace_range(start..end, "");
    }
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

#[test]
fn test_strip_ansi_8bit_c1_introducers() {
    assert_eq!(strip_ansi("a\u{9B}31mRED\u{9B}0mb"), "aREDb");
    assert_eq!(strip_ansi("a\x1bP1;0|payload\x1b\\b"), "ab");
    assert_eq!(strip_ansi("a\u{90}body\u{9C}b"), "ab");
}

#[test]
fn test_strip_ansi_single_char_esc() {
    // strip_ansi must consume both bytes of single-byte ESC sequences (RIS, DECSC, keypad, VT52).
    assert_eq!(strip_ansi("a\x1bcb"), "ab");
    assert_eq!(strip_ansi("a\x1b7b\x1b8c"), "abc");
    assert_eq!(strip_ansi("a\x1b=b\x1b>c"), "abc");
    assert_eq!(strip_ansi("a\x1bZb"), "ab");
}

#[test]
fn test_strip_ansi_preserves_control_bytes() {
    // strip_ansi removes only ANSI escape sequences; control bytes pass through.
    assert_eq!(strip_ansi("safe\rEVIL"), "safe\rEVIL");
    assert_eq!(strip_ansi("a\x08b\x07c\x0E\x0Fd"), "a\x08b\x07c\x0E\x0Fd");
}

#[test]
fn test_sanitize_substitutes_dangerous_bytes() {
    let r = '\u{FFFD}';
    assert_eq!(sanitize("safe\rEVIL"), format!("safe{r}EVIL"));
    assert_eq!(sanitize("a\x08b"), format!("a{r}b"));
    assert_eq!(sanitize("a\x07b"), format!("a{r}b"));
    assert_eq!(sanitize("a\x0Bb"), format!("a{r}b"));
    assert_eq!(sanitize("a\x0Eb\x0Fc"), format!("a{r}b{r}c"));
    assert_eq!(sanitize("a\u{8D}b"), format!("a{r}b"));
    assert_eq!(sanitize("a\u{85}b"), format!("a{r}b"));
    assert_eq!(sanitize("trailing\r"), format!("trailing{r}"));
    assert_eq!(sanitize("a\x7Fb"), format!("a{r}b"));
}

#[test]
fn test_sanitize_substitutes_bidi_and_zero_width() {
    let r = '\u{FFFD}';
    // Trojan-Source bidi formatting: U+202A..U+202E
    assert_eq!(sanitize("a\u{202A}b"), format!("a{r}b"));
    assert_eq!(sanitize("a\u{202E}b"), format!("a{r}b"));
    // Bidi isolates: U+2066..U+2069
    assert_eq!(sanitize("a\u{2066}b"), format!("a{r}b"));
    assert_eq!(sanitize("a\u{2069}b"), format!("a{r}b"));
    // Zero-width: U+200B..U+200D
    assert_eq!(sanitize("a\u{200B}b"), format!("a{r}b"));
    assert_eq!(sanitize("a\u{200D}b"), format!("a{r}b"));
    // BOM in middle of file: U+FEFF
    assert_eq!(sanitize("a\u{FEFF}b"), format!("a{r}b"));
}

#[test]
fn test_sanitize_preserves_legitimate_bytes() {
    assert_eq!(sanitize("crlf\r\nline\r\n"), "crlf\r\nline\r\n");
    assert_eq!(sanitize("a\tb\nc"), "a\tb\nc");
    assert_eq!(sanitize("plain ascii"), "plain ascii");
    assert_eq!(sanitize("üñíçödé"), "üñíçödé");
    // FF (U+000C) passes through; section separator in C source / Emacs Lisp.
    assert_eq!(sanitize("section1\x0Csection2"), "section1\x0Csection2");
    // Common Unicode that shares a UTF-8 lead byte with dangerous codepoints
    // must pass through unchanged.
    assert_eq!(sanitize("snowman ☃ moon ☾"), "snowman ☃ moon ☾");
    assert_eq!(sanitize("emoji 🎉 ☃"), "emoji 🎉 ☃");
    assert_eq!(sanitize("0xC2 lead: ÿ ñ ç"), "0xC2 lead: ÿ ñ ç");
    assert_eq!(sanitize("CJK 漢字 emoji 🦀"), "CJK 漢字 emoji 🦀");
}

#[test]
fn test_sanitize_strips_ansi() {
    let r = '\u{FFFD}';
    // sanitize is a strict superset of strip_ansi.
    assert_eq!(sanitize("a\x1B[31mb\x1B[0mc"), "abc");
    assert_eq!(sanitize("a\u{9B}31mb\u{9B}0mc"), "abc");
    // ANSI then dangerous byte: ANSI gone, byte substituted.
    assert_eq!(sanitize("\x1B[31mhello\rEVIL"), format!("hello{r}EVIL"));
}

#[test]
fn test_strip_overstrike() {
    // Bold: X\x08X (same char repeated)
    assert_eq!(strip_overstrike("H\x08Hello", 1), "Hello");

    // Underline: _\x08X (underscore before char)
    assert_eq!(strip_overstrike("_\x08Hello", 1), "Hello");

    // Multiple overstrike sequences
    assert_eq!(strip_overstrike("B\x08Bo\x08ol\x08ld\x08d", 1), "Bold");

    // Backspace at start of line (nothing to pop)
    assert_eq!(strip_overstrike("\x08Hello", 0), "Hello");

    // Multiple consecutive backspaces
    assert_eq!(strip_overstrike("ABC\x08\x08\x08XYZ", 3), "XYZ");

    // Unicode with overstrike
    assert_eq!(strip_overstrike("ä\x08äöü", 2), "äöü");
}

#[test]
fn test_strip_overstrike_preserves_ansi_before_backspace() {
    assert_eq!(strip_overstrike("v\x1b[22m\x08v", 6), "\x1b[22mv");
}

#[test]
fn test_sanitize_for_terminal_passthrough() {
    assert_eq!(sanitize_for_terminal(""), "");
    assert_eq!(sanitize_for_terminal("hello.txt"), "hello.txt");
    assert_eq!(sanitize_for_terminal("résumé.pdf"), "résumé.pdf");
    assert_eq!(sanitize_for_terminal("日本語.md"), "日本語.md");
    assert_eq!(
        sanitize_for_terminal("path/with spaces/file.log"),
        "path/with spaces/file.log"
    );
    assert_eq!(sanitize_for_terminal("a\tb"), "a\tb");
}

#[test]
fn test_sanitize_for_terminal_c0_controls() {
    assert_eq!(
        sanitize_for_terminal("\x1b[31mINJECTED\x1b[0m.txt"),
        "^[[31mINJECTED^[[0m.txt"
    );
    assert_eq!(sanitize_for_terminal("bad\x07rest"), "bad^Grest");
    assert_eq!(sanitize_for_terminal("\x00\x08\n\r\x7F"), "^@^H^J^M^?");
    assert_eq!(sanitize_for_terminal("\u{9b}31m"), "\\u{9b}31m");
    assert_eq!(
        sanitize_for_terminal("\u{9d}0;pwned\x07"),
        "\\u{9d}0;pwned^G"
    );
}

#[test]
fn test_sanitize_for_terminal_idempotent_on_sanitized() {
    let dirty = "\x1b]0;pwned\x07file.txt";
    let clean = sanitize_for_terminal(dirty);
    assert_eq!(sanitize_for_terminal(&clean), clean);
    assert!(!clean.contains('\x1b'));
    assert!(!clean.contains('\x07'));
}
