use std::{
    fmt::{Display, Formatter},
    iter::Peekable,
    str::CharIndices,
};

// Wrapper to avoid unnecessary branching when input doesn't have ANSI escape sequences.
pub struct AnsiStyle {
    attributes: Option<Attributes>,
}

impl AnsiStyle {
    pub fn new() -> Self {
        AnsiStyle { attributes: None }
    }

    pub fn update(&mut self, sequence: EscapeSequence) -> bool {
        match &mut self.attributes {
            Some(a) => a.update(sequence),
            None => {
                self.attributes = Some(Attributes::new());
                self.attributes.as_mut().unwrap().update(sequence)
            }
        }
    }

    pub fn to_reset_sequence(&self) -> String {
        match self.attributes {
            Some(ref a) => a.to_reset_sequence(),
            None => String::new(),
        }
    }
}

impl Display for AnsiStyle {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.attributes {
            Some(ref a) => a.fmt(f),
            None => Ok(()),
        }
    }
}

struct Attributes {
    has_sgr_sequences: bool,

    foreground: String,
    background: String,
    underlined: String,

    /// The character set to use.
    /// REGEX: `\^[()][AB0-3]`
    charset: String,

    /// A buffer for unknown sequences.
    unknown_buffer: String,

    /// ON:  ^[1m
    /// OFF: ^[22m
    bold: String,

    /// ON:  ^[2m
    /// OFF: ^[22m
    dim: String,

    /// ON:  ^[4m
    /// OFF: ^[24m
    underline: String,

    /// ON:  ^[3m
    /// OFF: ^[23m
    italic: String,

    /// ON:  ^[9m
    /// OFF: ^[29m
    strike: String,

    /// The hyperlink sequence.
    /// FORMAT: \x1B]8;{ID};{URL}\e\\
    ///
    /// `\e\\` may be replaced with BEL `\x07`.
    /// Setting both {ID} and {URL} to an empty string represents no hyperlink.
    hyperlink: String,
}

impl Attributes {
    pub fn new() -> Self {
        Attributes {
            has_sgr_sequences: false,

            foreground: "".to_owned(),
            background: "".to_owned(),
            underlined: "".to_owned(),
            charset: "".to_owned(),
            unknown_buffer: "".to_owned(),
            bold: "".to_owned(),
            dim: "".to_owned(),
            underline: "".to_owned(),
            italic: "".to_owned(),
            strike: "".to_owned(),
            hyperlink: "".to_owned(),
        }
    }

    /// Update the attributes with an escape sequence.
    /// Returns `false` if the sequence is unsupported.
    pub fn update(&mut self, sequence: EscapeSequence) -> bool {
        use EscapeSequence::*;
        match sequence {
            Text(_) => return false,
            Unknown(_) => { /* defer to update_with_unsupported */ }
            OSC {
                raw_sequence,
                command,
                ..
            } => {
                if command.starts_with("8;") {
                    return self.update_with_hyperlink(raw_sequence);
                }
                /* defer to update_with_unsupported */
            }
            CSI {
                final_byte,
                parameters,
                ..
            } => {
                match final_byte {
                    "m" => return self.update_with_sgr(parameters),
                    _ => {
                        // NOTE(eth-p): We might want to ignore these, since they involve cursor or buffer manipulation.
                        /* defer to update_with_unsupported */
                    }
                }
            }
            NF { nf_sequence, .. } => {
                let mut iter = nf_sequence.chars();
                match iter.next() {
                    Some('(') => return self.update_with_charset('(', iter),
                    Some(')') => return self.update_with_charset(')', iter),
                    _ => { /* defer to update_with_unsupported */ }
                }
            }
        }

        self.update_with_unsupported(sequence.raw())
    }

    fn sgr_reset(&mut self) {
        self.has_sgr_sequences = false;

        self.foreground.clear();
        self.background.clear();
        self.underlined.clear();
        self.bold.clear();
        self.dim.clear();
        self.underline.clear();
        self.italic.clear();
        self.strike.clear();
    }

    fn update_with_sgr(&mut self, parameters: &str) -> bool {
        let mut iter = parameters
            .split(';')
            .map(|p| if p.is_empty() { "0" } else { p })
            .map(|p| p.parse::<u16>())
            .map(|p| p.unwrap_or(0)); // Treat errors as 0.

        self.has_sgr_sequences = true;
        while let Some(p) = iter.next() {
            match p {
                0 => self.sgr_reset(),
                1 => self.bold = "\x1B[1m".to_owned(),
                2 => self.dim = "\x1B[2m".to_owned(),
                3 => self.italic = "\x1B[3m".to_owned(),
                4 => self.underline = "\x1B[4m".to_owned(),
                23 => self.italic.clear(),
                24 => self.underline.clear(),
                22 => {
                    self.bold.clear();
                    self.dim.clear();
                }
                30..=39 => self.foreground = Self::parse_color(p, &mut iter),
                40..=49 => self.background = Self::parse_color(p, &mut iter),
                58..=59 => self.underlined = Self::parse_color(p, &mut iter),
                90..=97 => self.foreground = Self::parse_color(p, &mut iter),
                100..=107 => self.background = Self::parse_color(p, &mut iter),
                _ => {
                    // Unsupported SGR sequence.
                    // Be compatible and pretend one just wasn't was provided.
                }
            }
        }

        true
    }

    fn update_with_unsupported(&mut self, sequence: &str) -> bool {
        self.unknown_buffer.push_str(sequence);
        false
    }

    fn update_with_hyperlink(&mut self, sequence: &str) -> bool {
        if sequence == "8;;" {
            // Empty hyperlink ID and HREF -> end of hyperlink.
            self.hyperlink.clear();
        } else {
            self.hyperlink.clear();
            self.hyperlink.push_str(sequence);
        }

        true
    }

    fn update_with_charset(&mut self, kind: char, set: impl Iterator<Item = char>) -> bool {
        self.charset = format!("\x1B{kind}{}", set.take(1).collect::<String>());
        true
    }

    fn parse_color(color: u16, parameters: &mut dyn Iterator<Item = u16>) -> String {
        match color % 10 {
            8 => match parameters.next() {
                Some(5) /* 256-color */ => format!("\x1B[{color};5;{}m", join(";", 1, parameters)),
                Some(2) /* 24-bit color */ => format!("\x1B[{color};2;{}m", join(";", 3, parameters)),
                Some(c) => format!("\x1B[{color};{c}m"),
                _ => "".to_owned(),
            },
            9 => "".to_owned(),
            _ => format!("\x1B[{color}m"),
        }
    }

    /// Gets an ANSI escape sequence to reset all the known attributes.
    pub fn to_reset_sequence(&self) -> String {
        let mut buf = String::with_capacity(17);

        // TODO: Enable me in a later pull request.
        // if self.has_sgr_sequences {
        //     buf.push_str("\x1B[m");
        // }

        if !self.hyperlink.is_empty() {
            buf.push_str("\x1B]8;;\x1B\\"); // Disable hyperlink.
        }

        // TODO: Enable me in a later pull request.
        // if !self.charset.is_empty() {
        //     // https://espterm.github.io/docs/VT100%20escape%20codes.html
        //     buf.push_str("\x1B(B\x1B)B"); // setusg0 and setusg1
        // }

        buf
    }
}

impl Display for Attributes {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}{}{}{}{}{}{}{}",
            self.foreground,
            self.background,
            self.underlined,
            self.charset,
            self.bold,
            self.dim,
            self.underline,
            self.italic,
            self.strike,
            self.hyperlink,
        )
    }
}

fn join(
    delimiter: &str,
    limit: usize,
    iterator: &mut dyn Iterator<Item = impl ToString>,
) -> String {
    iterator
        .take(limit)
        .map(|i| i.to_string())
        .collect::<Vec<String>>()
        .join(delimiter)
}

/// A range of indices for a raw ANSI escape sequence.
#[derive(Debug, PartialEq)]
pub enum EscapeSequenceOffsets {
    Text {
        start: usize,
        end: usize,
    },
    Unknown {
        start: usize,
        end: usize,
    },
    #[allow(clippy::upper_case_acronyms)]
    NF {
        // https://en.wikipedia.org/wiki/ANSI_escape_code#nF_Escape_sequences
        start_sequence: usize,
        start: usize,
        end: usize,
    },
    #[allow(clippy::upper_case_acronyms)]
    OSC {
        // https://en.wikipedia.org/wiki/ANSI_escape_code#OSC_(Operating_System_Command)_sequences
        start_sequence: usize,
        start_command: usize,
        start_terminator: usize,
        end: usize,
    },
    #[allow(clippy::upper_case_acronyms)]
    CSI {
        // https://en.wikipedia.org/wiki/ANSI_escape_code#CSI_(Control_Sequence_Introducer)_sequences
        start_sequence: usize,
        start_parameters: usize,
        start_intermediates: usize,
        start_final_byte: usize,
        end: usize,
    },
}

impl EscapeSequenceOffsets {
    /// Returns the byte-index of the first character in the escape sequence.
    pub fn index_of_start(&self) -> usize {
        use EscapeSequenceOffsets::*;
        match self {
            Text { start, .. } => *start,
            Unknown { start, .. } => *start,
            NF { start_sequence, .. } => *start_sequence,
            OSC { start_sequence, .. } => *start_sequence,
            CSI { start_sequence, .. } => *start_sequence,
        }
    }

    /// Returns the byte-index past the last character in the escape sequence.
    pub fn index_past_end(&self) -> usize {
        use EscapeSequenceOffsets::*;
        match self {
            Text { end, .. } => *end,
            Unknown { end, .. } => *end,
            NF { end, .. } => *end,
            OSC { end, .. } => *end,
            CSI { end, .. } => *end,
        }
    }
}

/// An iterator over the offests of ANSI/VT escape sequences within a string.
///
/// ## Example
///
/// ```ignore
/// let iter = EscapeSequenceOffsetsIterator::new("\x1B[33mThis is yellow text.\x1B[m");
/// ```
pub struct EscapeSequenceOffsetsIterator<'a> {
    text: &'a str,
    chars: Peekable<CharIndices<'a>>,
}

impl<'a> EscapeSequenceOffsetsIterator<'a> {
    pub fn new(text: &'a str) -> EscapeSequenceOffsetsIterator<'a> {
        EscapeSequenceOffsetsIterator {
            text,
            chars: text.char_indices().peekable(),
        }
    }

    /// Takes values from the iterator while the predicate returns true.
    /// If the predicate returns false, that value is left.
    fn chars_take_while(&mut self, pred: impl Fn(char) -> bool) -> Option<(usize, usize)> {
        self.chars.peek()?;

        let start = self.chars.peek().unwrap().0;
        let mut end: usize = start;
        while let Some((i, c)) = self.chars.peek() {
            if !pred(*c) {
                break;
            }

            end = *i + c.len_utf8();
            self.chars.next();
        }

        Some((start, end))
    }

    fn next_text(&mut self) -> Option<EscapeSequenceOffsets> {
        self.chars_take_while(|c| c != '\x1B')
            .map(|(start, end)| EscapeSequenceOffsets::Text { start, end })
    }

    fn next_sequence(&mut self) -> Option<EscapeSequenceOffsets> {
        let (start_sequence, c) = self.chars.next().expect("to not be finished");
        match self.chars.peek() {
            None => Some(EscapeSequenceOffsets::Unknown {
                start: start_sequence,
                end: start_sequence + c.len_utf8(),
            }),

            Some((_, ']')) => self.next_osc(start_sequence),
            Some((_, '[')) => self.next_csi(start_sequence),
            Some((i, c)) => match c {
                '\x20'..='\x2F' => self.next_nf(start_sequence),
                c => Some(EscapeSequenceOffsets::Unknown {
                    start: start_sequence,
                    end: i + c.len_utf8(),
                }),
            },
        }
    }

    fn next_osc(&mut self, start_sequence: usize) -> Option<EscapeSequenceOffsets> {
        let (osc_open_index, osc_open_char) = self.chars.next().expect("to not be finished");
        debug_assert_eq!(osc_open_char, ']');

        let mut start_terminator: usize;
        let mut end_sequence: usize;

        loop {
            match self.chars_take_while(|c| !matches!(c, '\x07' | '\x1B')) {
                None => {
                    start_terminator = self.text.len();
                    end_sequence = start_terminator;
                    break;
                }

                Some((_, end)) => {
                    start_terminator = end;
                    end_sequence = end;
                }
            }

            match self.chars.next() {
                Some((ti, '\x07')) => {
                    end_sequence = ti + '\x07'.len_utf8();
                    break;
                }

                Some((ti, '\x1B')) => {
                    match self.chars.next() {
                        Some((i, '\\')) => {
                            end_sequence = i + '\\'.len_utf8();
                            break;
                        }

                        None => {
                            end_sequence = ti + '\x1B'.len_utf8();
                            break;
                        }

                        _ => {
                            // Repeat, since `\\`(anything) isn't a valid ST.
                        }
                    }
                }

                None => {
                    // Prematurely ends.
                    break;
                }

                Some((_, tc)) => {
                    panic!("this should not be reached: char {tc:?}")
                }
            }
        }

        Some(EscapeSequenceOffsets::OSC {
            start_sequence,
            start_command: osc_open_index + osc_open_char.len_utf8(),
            start_terminator,
            end: end_sequence,
        })
    }

    fn next_csi(&mut self, start_sequence: usize) -> Option<EscapeSequenceOffsets> {
        let (csi_open_index, csi_open_char) = self.chars.next().expect("to not be finished");
        debug_assert_eq!(csi_open_char, '[');

        let start_parameters: usize = csi_open_index + csi_open_char.len_utf8();

        // Keep iterating while within the range of `0x30-0x3F`.
        let mut start_intermediates: usize = start_parameters;
        if let Some((_, end)) = self.chars_take_while(|c| matches!(c, '\x30'..='\x3F')) {
            start_intermediates = end;
        }

        // Keep iterating while within the range of `0x20-0x2F`.
        let mut start_final_byte: usize = start_intermediates;
        if let Some((_, end)) = self.chars_take_while(|c| matches!(c, '\x20'..='\x2F')) {
            start_final_byte = end;
        }

        // Take the last char.
        let end_of_sequence = match self.chars.next() {
            None => start_final_byte,
            Some((i, c)) => i + c.len_utf8(),
        };

        Some(EscapeSequenceOffsets::CSI {
            start_sequence,
            start_parameters,
            start_intermediates,
            start_final_byte,
            end: end_of_sequence,
        })
    }

    fn next_nf(&mut self, start_sequence: usize) -> Option<EscapeSequenceOffsets> {
        let (nf_open_index, nf_open_char) = self.chars.next().expect("to not be finished");
        debug_assert!(matches!(nf_open_char, '\x20'..='\x2F'));

        let start: usize = nf_open_index;
        let mut end: usize = start;

        // Keep iterating while within the range of `0x20-0x2F`.
        match self.chars_take_while(|c| matches!(c, '\x20'..='\x2F')) {
            Some((_, i)) => end = i,
            None => {
                return Some(EscapeSequenceOffsets::NF {
                    start_sequence,
                    start,
                    end,
                })
            }
        }

        // Get the final byte.
        if let Some((i, c)) = self.chars.next() {
            end = i + c.len_utf8()
        }

        Some(EscapeSequenceOffsets::NF {
            start_sequence,
            start,
            end,
        })
    }
}

impl Iterator for EscapeSequenceOffsetsIterator<'_> {
    type Item = EscapeSequenceOffsets;
    fn next(&mut self) -> Option<Self::Item> {
        match self.chars.peek() {
            Some((_, '\x1B')) => self.next_sequence(),
            Some((_, _)) => self.next_text(),
            None => None,
        }
    }
}

/// An iterator over ANSI/VT escape sequences within a string.
///
/// ## Example
///
/// ```ignore
/// let iter = EscapeSequenceIterator::new("\x1B[33mThis is yellow text.\x1B[m");
/// ```
pub struct EscapeSequenceIterator<'a> {
    text: &'a str,
    offset_iter: EscapeSequenceOffsetsIterator<'a>,
}

impl<'a> EscapeSequenceIterator<'a> {
    pub fn new(text: &'a str) -> EscapeSequenceIterator<'a> {
        EscapeSequenceIterator {
            text,
            offset_iter: EscapeSequenceOffsetsIterator::new(text),
        }
    }
}

impl<'a> Iterator for EscapeSequenceIterator<'a> {
    type Item = EscapeSequence<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        use EscapeSequenceOffsets::*;
        self.offset_iter.next().map(|offsets| match offsets {
            Unknown { start, end } => EscapeSequence::Unknown(&self.text[start..end]),
            Text { start, end } => EscapeSequence::Text(&self.text[start..end]),
            NF {
                start_sequence,
                start,
                end,
            } => EscapeSequence::NF {
                raw_sequence: &self.text[start_sequence..end],
                nf_sequence: &self.text[start..end],
            },
            OSC {
                start_sequence,
                start_command,
                start_terminator,
                end,
            } => EscapeSequence::OSC {
                raw_sequence: &self.text[start_sequence..end],
                command: &self.text[start_command..start_terminator],
                terminator: &self.text[start_terminator..end],
            },
            CSI {
                start_sequence,
                start_parameters,
                start_intermediates,
                start_final_byte,
                end,
            } => EscapeSequence::CSI {
                raw_sequence: &self.text[start_sequence..end],
                parameters: &self.text[start_parameters..start_intermediates],
                intermediates: &self.text[start_intermediates..start_final_byte],
                final_byte: &self.text[start_final_byte..end],
            },
        })
    }
}

/// A parsed ANSI/VT100 escape sequence.
#[derive(Debug, PartialEq)]
pub enum EscapeSequence<'a> {
    Text(&'a str),
    Unknown(&'a str),
    #[allow(clippy::upper_case_acronyms)]
    NF {
        raw_sequence: &'a str,
        nf_sequence: &'a str,
    },
    #[allow(clippy::upper_case_acronyms)]
    OSC {
        raw_sequence: &'a str,
        command: &'a str,
        terminator: &'a str,
    },
    #[allow(clippy::upper_case_acronyms)]
    CSI {
        raw_sequence: &'a str,
        parameters: &'a str,
        intermediates: &'a str,
        final_byte: &'a str,
    },
}

impl<'a> EscapeSequence<'a> {
    pub fn raw(&self) -> &'a str {
        use EscapeSequence::*;
        match *self {
            Text(raw) => raw,
            Unknown(raw) => raw,
            NF { raw_sequence, .. } => raw_sequence,
            OSC { raw_sequence, .. } => raw_sequence,
            CSI { raw_sequence, .. } => raw_sequence,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::vscreen::{
        EscapeSequence, EscapeSequenceIterator, EscapeSequenceOffsets,
        EscapeSequenceOffsetsIterator,
    };

    #[test]
    fn test_escape_sequence_offsets_iterator_parses_text() {
        let mut iter = EscapeSequenceOffsetsIterator::new("text");
        assert_eq!(
            iter.next(),
            Some(EscapeSequenceOffsets::Text { start: 0, end: 4 })
        );
    }

    #[test]
    fn test_escape_sequence_offsets_iterator_parses_text_stops_at_esc() {
        let mut iter = EscapeSequenceOffsetsIterator::new("text\x1B[ming");
        assert_eq!(
            iter.next(),
            Some(EscapeSequenceOffsets::Text { start: 0, end: 4 })
        );
    }

    #[test]
    fn test_escape_sequence_offsets_iterator_parses_osc_with_bel() {
        let mut iter = EscapeSequenceOffsetsIterator::new("\x1B]abc\x07");
        assert_eq!(
            iter.next(),
            Some(EscapeSequenceOffsets::OSC {
                start_sequence: 0,
                start_command: 2,
                start_terminator: 5,
                end: 6,
            })
        );
    }

    #[test]
    fn test_escape_sequence_offsets_iterator_parses_osc_with_st() {
        let mut iter = EscapeSequenceOffsetsIterator::new("\x1B]abc\x1B\\");
        assert_eq!(
            iter.next(),
            Some(EscapeSequenceOffsets::OSC {
                start_sequence: 0,
                start_command: 2,
                start_terminator: 5,
                end: 7,
            })
        );
    }

    #[test]
    fn test_escape_sequence_offsets_iterator_parses_osc_thats_broken() {
        let mut iter = EscapeSequenceOffsetsIterator::new("\x1B]ab");
        assert_eq!(
            iter.next(),
            Some(EscapeSequenceOffsets::OSC {
                start_sequence: 0,
                start_command: 2,
                start_terminator: 4,
                end: 4,
            })
        );
    }

    #[test]
    fn test_escape_sequence_offsets_iterator_parses_csi() {
        let mut iter = EscapeSequenceOffsetsIterator::new("\x1B[m");
        assert_eq!(
            iter.next(),
            Some(EscapeSequenceOffsets::CSI {
                start_sequence: 0,
                start_parameters: 2,
                start_intermediates: 2,
                start_final_byte: 2,
                end: 3
            })
        );
    }

    #[test]
    fn test_escape_sequence_offsets_iterator_parses_csi_with_parameters() {
        let mut iter = EscapeSequenceOffsetsIterator::new("\x1B[1;34m");
        assert_eq!(
            iter.next(),
            Some(EscapeSequenceOffsets::CSI {
                start_sequence: 0,
                start_parameters: 2,
                start_intermediates: 6,
                start_final_byte: 6,
                end: 7
            })
        );
    }

    #[test]
    fn test_escape_sequence_offsets_iterator_parses_csi_with_intermediates() {
        let mut iter = EscapeSequenceOffsetsIterator::new("\x1B[$m");
        assert_eq!(
            iter.next(),
            Some(EscapeSequenceOffsets::CSI {
                start_sequence: 0,
                start_parameters: 2,
                start_intermediates: 2,
                start_final_byte: 3,
                end: 4
            })
        );
    }

    #[test]
    fn test_escape_sequence_offsets_iterator_parses_csi_with_parameters_and_intermediates() {
        let mut iter = EscapeSequenceOffsetsIterator::new("\x1B[1$m");
        assert_eq!(
            iter.next(),
            Some(EscapeSequenceOffsets::CSI {
                start_sequence: 0,
                start_parameters: 2,
                start_intermediates: 3,
                start_final_byte: 4,
                end: 5
            })
        );
    }

    #[test]
    fn test_escape_sequence_offsets_iterator_parses_csi_thats_broken() {
        let mut iter = EscapeSequenceOffsetsIterator::new("\x1B[");
        assert_eq!(
            iter.next(),
            Some(EscapeSequenceOffsets::CSI {
                start_sequence: 0,
                start_parameters: 2,
                start_intermediates: 2,
                start_final_byte: 2,
                end: 2
            })
        );

        let mut iter = EscapeSequenceOffsetsIterator::new("\x1B[1");
        assert_eq!(
            iter.next(),
            Some(EscapeSequenceOffsets::CSI {
                start_sequence: 0,
                start_parameters: 2,
                start_intermediates: 3,
                start_final_byte: 3,
                end: 3
            })
        );

        let mut iter = EscapeSequenceOffsetsIterator::new("\x1B[1$");
        assert_eq!(
            iter.next(),
            Some(EscapeSequenceOffsets::CSI {
                start_sequence: 0,
                start_parameters: 2,
                start_intermediates: 3,
                start_final_byte: 4,
                end: 4
            })
        );
    }

    #[test]
    fn test_escape_sequence_offsets_iterator_parses_nf() {
        let mut iter = EscapeSequenceOffsetsIterator::new("\x1B($0");
        assert_eq!(
            iter.next(),
            Some(EscapeSequenceOffsets::NF {
                start_sequence: 0,
                start: 1,
                end: 4
            })
        );
    }

    #[test]
    fn test_escape_sequence_offsets_iterator_parses_nf_thats_broken() {
        let mut iter = EscapeSequenceOffsetsIterator::new("\x1B(");
        assert_eq!(
            iter.next(),
            Some(EscapeSequenceOffsets::NF {
                start_sequence: 0,
                start: 1,
                end: 1
            })
        );
    }

    #[test]
    fn test_escape_sequence_offsets_iterator_iterates() {
        let mut iter = EscapeSequenceOffsetsIterator::new("text\x1B[33m\x1B]OSC\x07\x1B(0");
        assert_eq!(
            iter.next(),
            Some(EscapeSequenceOffsets::Text { start: 0, end: 4 })
        );
        assert_eq!(
            iter.next(),
            Some(EscapeSequenceOffsets::CSI {
                start_sequence: 4,
                start_parameters: 6,
                start_intermediates: 8,
                start_final_byte: 8,
                end: 9
            })
        );
        assert_eq!(
            iter.next(),
            Some(EscapeSequenceOffsets::OSC {
                start_sequence: 9,
                start_command: 11,
                start_terminator: 14,
                end: 15
            })
        );
        assert_eq!(
            iter.next(),
            Some(EscapeSequenceOffsets::NF {
                start_sequence: 15,
                start: 16,
                end: 18
            })
        );
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_escape_sequence_iterator_iterates() {
        let mut iter = EscapeSequenceIterator::new("text\x1B[33m\x1B]OSC\x07\x1B]OSC\x1B\\\x1B(0");
        assert_eq!(iter.next(), Some(EscapeSequence::Text("text")));
        assert_eq!(
            iter.next(),
            Some(EscapeSequence::CSI {
                raw_sequence: "\x1B[33m",
                parameters: "33",
                intermediates: "",
                final_byte: "m",
            })
        );
        assert_eq!(
            iter.next(),
            Some(EscapeSequence::OSC {
                raw_sequence: "\x1B]OSC\x07",
                command: "OSC",
                terminator: "\x07",
            })
        );
        assert_eq!(
            iter.next(),
            Some(EscapeSequence::OSC {
                raw_sequence: "\x1B]OSC\x1B\\",
                command: "OSC",
                terminator: "\x1B\\",
            })
        );
        assert_eq!(
            iter.next(),
            Some(EscapeSequence::NF {
                raw_sequence: "\x1B(0",
                nf_sequence: "(0",
            })
        );
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_sgr_attributes_do_not_leak_into_wrong_field() {
        let mut attrs = crate::vscreen::Attributes::new();

        // Bold, Dim, Italic, Underline, Foreground, Background
        attrs.update(EscapeSequence::CSI {
            raw_sequence: "\x1B[1;2;3;4;31;41m",
            parameters: "1;2;3;4;31;41",
            intermediates: "",
            final_byte: "m",
        });

        assert_eq!(attrs.bold, "\x1B[1m");
        assert_eq!(attrs.dim, "\x1B[2m");
        assert_eq!(attrs.italic, "\x1B[3m");
        assert_eq!(attrs.underline, "\x1B[4m");
        assert_eq!(attrs.foreground, "\x1B[31m");
        assert_eq!(attrs.background, "\x1B[41m");

        // Bold, Bright Foreground, Bright Background
        attrs.sgr_reset();
        attrs.update(EscapeSequence::CSI {
            raw_sequence: "\x1B[1;94;103m",
            parameters: "1;94;103",
            intermediates: "",
            final_byte: "m",
        });

        assert_eq!(attrs.bold, "\x1B[1m");
        assert_eq!(attrs.foreground, "\x1B[94m");
        assert_eq!(attrs.background, "\x1B[103m");
    }
}
