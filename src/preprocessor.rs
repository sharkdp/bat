use std::fmt::Write;

use crate::{
    nonprintable_notation::NonprintableNotation,
    vscreen::{
        AnsiStyle, EscapeSequence, EscapeSequenceIterator, EscapeSequenceOffsets,
        EscapeSequenceOffsetsIterator,
    },
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
#[allow(dead_code)]
pub fn strip_ansi(line: &str) -> String {
    let mut buffer = String::with_capacity(line.len());

    for seq in EscapeSequenceOffsetsIterator::new(line) {
        if let EscapeSequenceOffsets::Text { .. } = seq {
            buffer.push_str(&line[seq.index_of_start()..seq.index_past_end()]);
        }
    }

    buffer
}

/// Strip ANSI escape sequences from a line, returning both the stripped text
/// and a style overlay that maps byte positions in the stripped text to the
/// `AnsiStyle` that was active at each position in the original text.
///
/// This allows syntax highlighting (syntect) to operate on clean text, while
/// preserving the original ANSI formatting (bold, underline, color, etc.) for
/// re-application during rendering.
pub fn strip_ansi_with_overlay(line: &str) -> (String, Vec<(usize, AnsiStyle)>) {
    let mut stripped = String::with_capacity(line.len());
    let mut style_changes: Vec<(usize, AnsiStyle)> = Vec::new();
    let mut current_style = AnsiStyle::new();

    for chunk in EscapeSequenceIterator::new(line) {
        match chunk {
            EscapeSequence::Text(text) => {
                let offset = stripped.len();

                // Only record a style change if it differs from the last recorded one.
                let is_style_change = style_changes
                    .last()
                    .map_or(true, |(_, prev)| *prev != current_style);

                if is_style_change {
                    style_changes.push((offset, current_style.clone()));
                }

                stripped.push_str(text);
            }
            _ => {
                current_style.update(chunk);
            }
        }
    }

    (stripped, style_changes)
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

/// Controls when ANSI escape sequences are stripped from input before rendering.
///
/// - `Never` (default): ANSI escape sequences pass through to the terminal unchanged.
/// - `Always`: All ANSI escape sequences are stripped and discarded.
/// - `Auto`: ANSI escape sequences are stripped before syntax highlighting, but
///   semantic formatting (bold, underline, OSC8 hyperlinks) is re-applied on the
///   output. This preserves both syntax highlighting and the input's formatting.
///   Recommended for use as MANPAGER (e.g. `MANPAGER="bat -pl man --strip-ansi=auto"`).
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

// ── strip_ansi_with_overlay tests ──────────────────────────────────────────

/// Helper: extract the AnsiStyle at a given byte offset
/// within the stripped text, using the same binary-search logic as the printer.
#[cfg(test)]
fn overlay_style_at(offset: usize, overlay: &[(usize, AnsiStyle)]) -> AnsiStyle {
    match overlay.binary_search_by_key(&offset, |(pos, _)| *pos) {
        Ok(idx) => overlay[idx].1.clone(),
        Err(idx) => {
            if idx == 0 {
                AnsiStyle::new()
            } else {
                overlay[idx - 1].1.clone()
            }
        }
    }
}

#[test]
fn test_strip_ansi_with_overlay_no_ansi() {
    let (stripped, overlay) = strip_ansi_with_overlay("plain text");
    assert_eq!(stripped, "plain text");
    // Default style at offset 0 produces empty display (no ANSI codes)
    assert_eq!(
        format!("{}", overlay_style_at(0, &overlay)),
        "",
        "default style should produce no ANSI output"
    );
}

#[test]
fn test_strip_ansi_with_overlay_bold_on_off() {
    let (stripped, overlay) = strip_ansi_with_overlay("\x1B[1mNAME\x1B[22m more");
    assert_eq!(stripped, "NAME more");

    let style = overlay_style_at(0, &overlay);
    assert!(
        format!("{style}").contains("\x1B[1m"),
        "bold should be active at offset 0"
    );

    // After "NAME " at offset 5, bold should be off due to \x1B[22m
    let style_after = overlay_style_at(5, &overlay);
    assert!(
        !format!("{style_after}").contains("\x1B[1m"),
        "bold should be off after \\x1B[22m"
    );
}

#[test]
fn test_strip_ansi_with_overlay_underline_on_off() {
    let (stripped, overlay) = strip_ansi_with_overlay("\x1B[4marg\x1B[24m rest");
    assert_eq!(stripped, "arg rest");

    let style = overlay_style_at(0, &overlay);
    assert!(
        format!("{style}").contains("\x1B[4m"),
        "underline should be active"
    );

    let style_after = overlay_style_at(3, &overlay);
    assert!(
        !format!("{style_after}").contains("\x1B[4m"),
        "underline should be off after \\x1B[24m"
    );
}

#[test]
fn test_strip_ansi_with_overlay_bold_and_underline_combined() {
    let input = "\x1B[1mNAME\x1B[22m\x1B[4muname\x1B[24m";
    let (stripped, overlay) = strip_ansi_with_overlay(input);
    assert_eq!(stripped, "NAMEuname");

    let style_name = overlay_style_at(0, &overlay);
    let name_fmt = format!("{style_name}");
    assert!(name_fmt.contains("\x1B[1m"), "bold active for NAME");
    assert!(!name_fmt.contains("\x1B[4m"), "underline off for NAME");

    let style_arg = overlay_style_at(4, &overlay);
    let arg_fmt = format!("{style_arg}");
    assert!(!arg_fmt.contains("\x1B[1m"), "bold off for uname");
    assert!(arg_fmt.contains("\x1B[4m"), "underline active for uname");
}

#[test]
fn test_strip_ansi_with_overlay_dim() {
    let (stripped, overlay) = strip_ansi_with_overlay("\x1B[2mdim text\x1B[22m");
    assert_eq!(stripped, "dim text");

    let style = overlay_style_at(0, &overlay);
    assert!(
        format!("{style}").contains("\x1B[2m"),
        "dim should be active"
    );
}

#[test]
fn test_strip_ansi_with_overlay_italic() {
    let (stripped, overlay) = strip_ansi_with_overlay("\x1B[3mitalic\x1B[23m rest");
    assert_eq!(stripped, "italic rest");

    let style = overlay_style_at(0, &overlay);
    assert!(
        format!("{style}").contains("\x1B[3m"),
        "italic should be active"
    );

    let style_after = overlay_style_at(6, &overlay);
    assert!(
        !format!("{style_after}").contains("\x1B[3m"),
        "italic should be off after \\x1B[23m"
    );
}

#[test]
fn test_strip_ansi_with_overlay_foreground_color() {
    let (stripped, overlay) = strip_ansi_with_overlay("\x1B[33mColor\x1B[39m");
    assert_eq!(stripped, "Color");

    let style = overlay_style_at(0, &overlay);
    assert!(
        format!("{style}").contains("\x1B[33m"),
        "foreground color should be active"
    );
}

#[test]
fn test_strip_ansi_with_overlay_256_color() {
    let (stripped, overlay) = strip_ansi_with_overlay("\x1B[38;5;182mHeading\x1B[39m");
    assert_eq!(stripped, "Heading");

    let style = overlay_style_at(0, &overlay);
    assert!(
        format!("{style}").contains("38;5;182"),
        "foreground should contain 256-color code 182"
    );
}

#[test]
fn test_strip_ansi_with_overlay_truecolor() {
    let (stripped, overlay) = strip_ansi_with_overlay("\x1B[38;2;255;100;0mOrange\x1B[39m");
    assert_eq!(stripped, "Orange");

    let style = overlay_style_at(0, &overlay);
    assert!(
        format!("{style}").contains("38;2;255;100;0"),
        "foreground should contain truecolor code"
    );
}

#[test]
fn test_strip_ansi_with_overlay_background_color() {
    let (stripped, overlay) = strip_ansi_with_overlay("\x1B[41mBG\x1B[49m");
    assert_eq!(stripped, "BG");

    let style = overlay_style_at(0, &overlay);
    assert!(
        format!("{style}").contains("\x1B[41m"),
        "background color should be active"
    );
}

#[test]
fn test_strip_ansi_with_overlay_sgr_reset() {
    let (stripped, overlay) = strip_ansi_with_overlay("\x1B[1;33mBoldYellow\x1B[0mplain");
    assert_eq!(stripped, "BoldYellowplain");

    let style_bold = overlay_style_at(0, &overlay);
    let bold_fmt = format!("{style_bold}");
    assert!(bold_fmt.contains("\x1B[1m"), "bold active before reset");
    assert!(
        bold_fmt.contains("\x1B[33m"),
        "foreground active before reset"
    );

    let style_plain = overlay_style_at(10, &overlay);
    let plain_fmt = format!("{style_plain}");
    assert!(!plain_fmt.contains("\x1B[1m"), "bold off after reset");
    assert!(
        !plain_fmt.contains("\x1B[33m"),
        "foreground off after reset"
    );
}

#[test]
fn test_strip_ansi_with_overlay_osc8_hyperlink() {
    let input = "\x1B]8;;man:sshd(8)\x1B\\sshd\x1B]8;;\x1B\\";
    let (stripped, overlay) = strip_ansi_with_overlay(input);
    assert_eq!(stripped, "sshd");

    let style = overlay_style_at(0, &overlay);
    let fmt = format!("{style}");
    assert!(
        fmt.contains("\x1B]8;;"),
        "hyperlink should be active for linked text"
    );
    assert!(
        fmt.contains("man:sshd(8)"),
        "hyperlink URI should be preserved"
    );
    // Reset sequence should close the hyperlink
    assert_eq!(
        style.to_reset_sequence(),
        "\x1B]8;;\x1B\\",
        "reset should close hyperlink"
    );
}

#[test]
fn test_strip_ansi_with_overlay_osc8_hyperlink_open_close() {
    let input = "before\x1B]8;;https://example.com\x1B\\linked\x1B]8;;\x1B\\after";
    let (stripped, overlay) = strip_ansi_with_overlay(input);
    assert_eq!(stripped, "beforelinkedafter");

    // "before" at offset 0: no hyperlink
    let style_before = overlay_style_at(0, &overlay);
    assert!(
        !format!("{style_before}").contains("\x1B]8;;"),
        "no hyperlink before opening"
    );

    // "linked" at offset 6: hyperlink open (has a URI)
    let style_linked = overlay_style_at(6, &overlay);
    let linked_fmt = format!("{style_linked}");
    assert!(
        linked_fmt.contains("\x1B]8;;"),
        "hyperlink present for linked text"
    );
    assert!(
        linked_fmt.contains("https://example.com"),
        "hyperlink URI in linked text"
    );

    // "after" at offset 12: hyperlink closed
    // NOTE: The current AnsiStyle implementation stores close sequences as
    // "\x1B]8;;\x1B\\" in the hyperlink field instead of clearing it.
    // This is a pre-existing limitation — the close sequence is present
    // but it's a no-op close rather than an open with a URI.
    let style_after = overlay_style_at(12, &overlay);
    let after_fmt = format!("{style_after}");
    assert!(
        !after_fmt.contains("https://example.com"),
        "hyperlink URI should be gone after close"
    );
    // The close hyperlink still produces output (a close escape sequence).
    // In an ideal fix, to_reset_sequence() would be empty for closed links.
}

#[test]
fn test_strip_ansi_with_overlay_man_page_heading() {
    let input = "\x1B[1mNAME\x1B[22m     uname - print system information";
    let (stripped, overlay) = strip_ansi_with_overlay(input);
    assert_eq!(stripped, "NAME     uname - print system information");

    let style_name = overlay_style_at(0, &overlay);
    assert!(
        format!("{style_name}").contains("\x1B[1m"),
        "bold active for heading"
    );

    let style_body = overlay_style_at(4, &overlay);
    assert!(
        !format!("{style_body}").contains("\x1B[1m"),
        "bold off for body text"
    );
}

#[test]
fn test_strip_ansi_with_overlay_mixed_ansi_and_text() {
    let input = "plain\x1B[1mbold\x1B[22m\x1B[4muline\x1B[24mend";
    let (stripped, overlay) = strip_ansi_with_overlay(input);
    assert_eq!(stripped, "plainboldulineend");

    let s0 = format!("{}", overlay_style_at(0, &overlay));
    assert!(!s0.contains("\x1B[1m") && !s0.contains("\x1B[4m"));

    let s5 = format!("{}", overlay_style_at(5, &overlay));
    assert!(s5.contains("\x1B[1m") && !s5.contains("\x1B[4m"));

    let s9 = format!("{}", overlay_style_at(9, &overlay));
    assert!(!s9.contains("\x1B[1m") && s9.contains("\x1B[4m"));

    let s14 = format!("{}", overlay_style_at(14, &overlay));
    assert!(!s14.contains("\x1B[1m") && !s14.contains("\x1B[4m"));
}

#[test]
fn test_strip_ansi_with_overlay_bold_reset_vs_specific() {
    // \x1B[22m resets BOTH bold and dim; \x1B[1m should not restore dim
    let input = "\x1B[1;2mbold+dim\x1B[22m\x1B[1monly-bold";
    let (stripped, overlay) = strip_ansi_with_overlay(input);
    assert_eq!(stripped, "bold+dimonly-bold");

    let s0 = format!("{}", overlay_style_at(0, &overlay));
    assert!(s0.contains("\x1B[1m"), "bold should be active");
    assert!(s0.contains("\x1B[2m"), "dim should be active");

    let s9 = format!("{}", overlay_style_at(9, &overlay));
    assert!(s9.contains("\x1B[1m"), "bold active after re-enable");
    assert!(!s9.contains("\x1B[2m"), "dim off after 22m reset");
}

#[test]
fn test_strip_ansi_with_overlay_adjacent_style_changes() {
    // Style changes with no text between them should produce a single overlay
    // entry for the final style state
    let input = "\x1B[1m\x1B[33mcolored\x1B[0m";
    let (stripped, overlay) = strip_ansi_with_overlay(input);
    assert_eq!(stripped, "colored");

    let style = overlay_style_at(0, &overlay);
    let fmt = format!("{style}");
    assert!(fmt.contains("\x1B[1m"), "bold active");
    assert!(fmt.contains("\x1B[33m"), "foreground active");
}

#[test]
fn test_strip_ansi_with_overlay_empty_input() {
    let (stripped, overlay) = strip_ansi_with_overlay("");
    assert_eq!(stripped, "");
    assert!(overlay.is_empty());
}

#[test]
fn test_strip_ansi_with_overlay_only_escapes() {
    let (stripped, overlay) = strip_ansi_with_overlay("\x1B[1m\x1B[33m\x1B[0m");
    assert_eq!(stripped, "");
    assert!(overlay.is_empty(), "no text means no overlay entries");
}

#[test]
fn test_strip_ansi_with_overlay_bold_with_osc8_combined() {
    // Bold heading containing a cross-reference hyperlink
    let input = "\x1B[1mSEE ALSO\x1B[22m  \x1B]8;;man:sshd(8)\x1B\\sshd(8)\x1B]8;;\x1B\\";
    let (stripped, overlay) = strip_ansi_with_overlay(input);
    assert_eq!(stripped, "SEE ALSO  sshd(8)");

    let style_heading = overlay_style_at(0, &overlay);
    let heading_fmt = format!("{style_heading}");
    assert!(heading_fmt.contains("\x1B[1m"), "bold active for heading");
    assert!(!heading_fmt.contains("\x1B]8;;"), "no hyperlink in heading");

    let style_link = overlay_style_at(10, &overlay);
    let link_fmt = format!("{style_link}");
    assert!(!link_fmt.contains("\x1B[1m"), "no bold in link text");
    assert!(
        link_fmt.contains("\x1B]8;;"),
        "hyperlink active for link text"
    );
    assert!(link_fmt.contains("man:sshd(8)"), "hyperlink URI present");
}
