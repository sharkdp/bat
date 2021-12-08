use std::fmt::{Display, Formatter};

// Wrapper to avoid unnecessary branching when input doesn't have ANSI escape sequences.
pub struct AnsiStyle {
    attributes: Option<Attributes>,
}

impl AnsiStyle {
    pub fn new() -> Self {
        AnsiStyle { attributes: None }
    }

    pub fn update(&mut self, sequence: &str) -> bool {
        match &mut self.attributes {
            Some(a) => a.update(sequence),
            None => {
                self.attributes = Some(Attributes::new());
                self.attributes.as_mut().unwrap().update(sequence)
            }
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
}

impl Attributes {
    pub fn new() -> Self {
        Attributes {
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
        }
    }

    /// Update the attributes with an escape sequence.
    /// Returns `false` if the sequence is unsupported.
    pub fn update(&mut self, sequence: &str) -> bool {
        let mut chars = sequence.char_indices().skip(1);

        if let Some((_, t)) = chars.next() {
            match t {
                '(' => self.update_with_charset('(', chars.map(|(_, c)| c)),
                ')' => self.update_with_charset(')', chars.map(|(_, c)| c)),
                '[' => {
                    if let Some((i, last)) = chars.last() {
                        // SAFETY: Always starts with ^[ and ends with m.
                        self.update_with_csi(last, &sequence[2..i])
                    } else {
                        false
                    }
                }
                _ => self.update_with_unsupported(sequence),
            }
        } else {
            false
        }
    }

    fn sgr_reset(&mut self) {
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

        while let Some(p) = iter.next() {
            match p {
                0 => self.sgr_reset(),
                1 => self.bold = format!("\x1B[{}m", parameters),
                2 => self.dim = format!("\x1B[{}m", parameters),
                3 => self.italic = format!("\x1B[{}m", parameters),
                4 => self.underline = format!("\x1B[{}m", parameters),
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
                100..=107 => self.foreground = Self::parse_color(p, &mut iter),
                _ => {
                    // Unsupported SGR sequence.
                    // Be compatible and pretend one just wasn't was provided.
                }
            }
        }

        true
    }

    fn update_with_csi(&mut self, finalizer: char, sequence: &str) -> bool {
        if finalizer == 'm' {
            self.update_with_sgr(sequence)
        } else {
            false
        }
    }

    fn update_with_unsupported(&mut self, sequence: &str) -> bool {
        self.unknown_buffer.push_str(sequence);
        false
    }

    fn update_with_charset(&mut self, kind: char, set: impl Iterator<Item = char>) -> bool {
        self.charset = format!("\x1B{}{}", kind, set.take(1).collect::<String>());
        true
    }

    fn parse_color(color: u16, parameters: &mut dyn Iterator<Item = u16>) -> String {
        match color % 10 {
            8 => match parameters.next() {
                Some(5) /* 256-color */ => format!("\x1B[{};5;{}m", color, join(";", 1, parameters)),
                Some(2) /* 24-bit color */ => format!("\x1B[{};2;{}m", color, join(";", 3, parameters)),
                Some(c) => format!("\x1B[{};{}m", color, c),
                _ => "".to_owned(),
            },
            9 => "".to_owned(),
            _ => format!("\x1B[{}m", color),
        }
    }
}

impl Display for Attributes {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}{}{}{}{}{}{}",
            self.foreground,
            self.background,
            self.underlined,
            self.charset,
            self.bold,
            self.dim,
            self.underline,
            self.italic,
            self.strike,
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
