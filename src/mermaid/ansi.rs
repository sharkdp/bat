//! Color output for themed diagrams: ANSI terminal modes (16/256/truecolor)
//! and HTML `<span>` tags for browser rendering.

use std::{fmt::Write, io::IsTerminal};

/// Role of a character in the rendered diagram; each role maps to a theme
/// color when colors are enabled.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CharRole {
    /// Node labels, edge labels.
    Text,
    /// Node box borders, subgraph borders.
    Border,
    /// Edge lines (paths between nodes).
    Line,
    /// Arrowheads (`▲▼◄►` or `^v<>`).
    Arrow,
    /// Corner characters at path bends.
    Corner,
    /// Junction characters (`┬┴├┤`) where edges meet boxes.
    Junction,
}

/// Output color mode.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum ColorMode {
    /// Plain text.
    #[default]
    None,
    /// 16-color ANSI.
    Ansi16,
    /// 256-color xterm palette.
    Ansi256,
    /// 24-bit RGB.
    Truecolor,
    /// HTML `<span>` tags with inline color styles.
    Html,
}

/// Theme colors as hex strings (`#rgb` or `#rrggbb`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Theme {
    /// Text color (node labels, edge labels).
    pub fg: String,
    /// Box border color (node borders, subgraph borders).
    pub border: String,
    /// Edge line color.
    pub line: String,
    /// Arrowhead color.
    pub arrow: String,
    /// Accent color used by xycharts for series 0.
    pub accent: Option<String>,
    /// Background color used by xycharts for dark-mode-aware shading.
    pub bg: Option<String>,
    /// Corner character color; defaults to `line`.
    pub corner: Option<String>,
    /// Junction character color; defaults to `border`.
    pub junction: Option<String>,
}

impl Default for Theme {
    /// Zinc palette derived from the SVG renderer's default theme.
    fn default() -> Self {
        Self {
            fg: "#27272a".into(),
            border: "#a1a1aa".into(),
            line: "#71717a".into(),
            arrow: "#52525b".into(),
            accent: None,
            bg: None,
            corner: Some("#71717a".into()),
            junction: Some("#a1a1aa".into()),
        }
    }
}

impl Theme {
    /// Hex color for a character role.
    pub fn role_color(&self, role: CharRole) -> &str {
        match role {
            CharRole::Text => &self.fg,
            CharRole::Border => &self.border,
            CharRole::Line => &self.line,
            CharRole::Arrow => &self.arrow,
            CharRole::Corner => self.corner.as_deref().unwrap_or(&self.line),
            CharRole::Junction => self.junction.as_deref().unwrap_or(&self.border),
        }
    }
}

/// Detect the best color mode for the current process.
///
/// `none` when stdout is not a terminal, `truecolor` for
/// `COLORTERM=truecolor|24bit`, `ansi256` when `TERM` mentions 256 colors,
/// `ansi16` for any other non-dumb `TERM`.
pub fn detect_color_mode() -> ColorMode {
    if !std::io::stdout().is_terminal() {
        return ColorMode::None;
    }
    let color_term = std::env::var("COLORTERM")
        .map(|v| v.to_lowercase())
        .unwrap_or_default();
    let term = std::env::var("TERM")
        .map(|v| v.to_lowercase())
        .unwrap_or_default();
    if color_term == "truecolor" || color_term == "24bit" {
        ColorMode::Truecolor
    } else if term.contains("256") {
        ColorMode::Ansi256
    } else if !term.is_empty() && term != "dumb" {
        ColorMode::Ansi16
    } else {
        ColorMode::None
    }
}

const ESC: &str = "\x1b[";
/// Reset all attributes.
pub const RESET: &str = "\x1b[0m";

/// Parse `#rgb` / `#rrggbb` (with or without `#`) into channels. Invalid hex
/// digits parse as 0 per channel, matching lenient `parseInt` behavior only
/// insofar as valid inputs are concerned; callers supply theme colors.
fn parse_hex(hex: &str) -> (u32, u32, u32) {
    let h = hex.strip_prefix('#').unwrap_or(hex);
    let digit = |s: &str| u32::from_str_radix(s, 16).unwrap_or(0);
    if h.len() == 3 {
        let mut chars = h.chars();
        let mut channel = || {
            let c = chars.next().unwrap_or('0');
            digit(&format!("{c}{c}"))
        };
        let r = channel();
        let g = channel();
        let b = channel();
        return (r, g, b);
    }
    let slice = |from: usize, to: usize| h.get(from..to).map_or(0, digit);
    (slice(0, 2), slice(2, 4), slice(4, 6))
}

fn truecolor_fg(hex: &str) -> String {
    let (r, g, b) = parse_hex(hex);
    format!("{ESC}38;2;{r};{g};{b}m")
}

/// Closest xterm-256 palette index: grayscale ramp (232–255) for near-gray
/// colors, otherwise the 6×6×6 cube (16–231).
fn rgb_to_256(r: u32, g: u32, b: u32) -> u32 {
    let avg = f64::from(r + g + b) / 3.0;
    let max_diff = [r, g, b]
        .into_iter()
        .map(|v| (f64::from(v) - avg).abs())
        .fold(0.0, f64::max);
    if max_diff < 10.0 {
        let gray = (avg / 255.0 * 23.0).round() as i64;
        return 232 + gray.clamp(0, 23) as u32;
    }
    let to_index = |v: u32| -> u32 {
        if v < 48 {
            0
        } else if v < 115 {
            1
        } else {
            ((v - 35) / 40).min(5)
        }
    };
    16 + 36 * to_index(r) + 6 * to_index(g) + to_index(b)
}

fn ansi256_fg(hex: &str) -> String {
    let (r, g, b) = parse_hex(hex);
    format!("{ESC}38;5;{}m", rgb_to_256(r, g, b))
}

/// Map a color to the closest of the 16 basic ANSI colors by dominant
/// channel, using the bright variant for dark colors.
#[allow(clippy::suboptimal_flops)]
fn ansi16_fg(hex: &str) -> String {
    let (r, g, b) = parse_hex(hex);
    let luma = 0.299 * f64::from(r) + 0.587 * f64::from(g) + 0.114 * f64::from(b);
    let bright = if luma > 100.0 { 0 } else { 60 };
    let code = if r > 180 && g < 100 && b < 100 {
        31
    } else if g > 180 && r < 100 && b < 100 {
        32
    } else if r > 150 && g > 150 && b < 100 {
        33
    } else if b > 180 && r < 100 && g < 100 {
        34
    } else if r > 150 && b > 150 && g < 100 {
        35
    } else if g > 150 && b > 150 && r < 100 {
        36
    } else if luma > 200.0 {
        37
    } else if luma < 50.0 {
        30
    } else {
        37
    };
    format!("{ESC}{}m", code + bright)
}

fn push_escaped_html(out: &mut String, text: &str) {
    for c in text.chars() {
        match c {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            _ => out.push(c),
        }
    }
}

fn push_html_span(out: &mut String, hex: &str, text: &str) {
    let _ = write!(out, "<span style=\"color:{hex}\">");
    push_escaped_html(out, text);
    out.push_str("</span>");
}

/// ANSI foreground escape for a hex color in `mode`; empty for `None`/`Html`.
fn ansi_fg(hex: &str, mode: ColorMode) -> String {
    match mode {
        ColorMode::Truecolor => truecolor_fg(hex),
        ColorMode::Ansi256 => ansi256_fg(hex),
        ColorMode::Ansi16 => ansi16_fg(hex),
        ColorMode::None | ColorMode::Html => String::new(),
    }
}

/// ANSI foreground escape for a role's theme color; empty for `None`/`Html`.
pub fn role_ansi_color(role: CharRole, theme: &Theme, mode: ColorMode) -> String {
    ansi_fg(theme.role_color(role), mode)
}

/// One serialized canvas row cell: its text and role.
pub type RoleCell<'a> = (&'a str, Option<CharRole>);

/// Colorize one row by grouping consecutive same-role cells into a single
/// escape (ANSI) or span (HTML). Spaces are emitted bare, ending any group.
pub fn colorize_line(cells: &[RoleCell<'_>], theme: &Theme, mode: ColorMode) -> String {
    let mut out = String::new();
    if mode == ColorMode::None {
        for (text, _) in cells {
            out.push_str(text);
        }
        return out;
    }
    let html = mode == ColorMode::Html;
    let mut current: Option<CharRole> = None;
    let mut buffer = String::new();
    let flush = |out: &mut String, buffer: &mut String, role: Option<CharRole>| {
        if buffer.is_empty() {
            return;
        }
        match (role, html) {
            (Some(role), true) => push_html_span(out, theme.role_color(role), buffer),
            (None, true) => push_escaped_html(out, buffer),
            (Some(role), false) => {
                out.push_str(&role_ansi_color(role, theme, mode));
                out.push_str(buffer);
                out.push_str(RESET);
            }
            (None, false) => out.push_str(buffer),
        }
        buffer.clear();
    };
    for &(text, role) in cells {
        if text == " " {
            flush(&mut out, &mut buffer, current);
            current = None;
            out.push(' ');
            continue;
        }
        if role == current {
            buffer.push_str(text);
            continue;
        }
        flush(&mut out, &mut buffer, current);
        buffer.push_str(text);
        current = role;
    }
    flush(&mut out, &mut buffer, current);
    out
}

/// Colorize `text` with a direct hex color in any output mode. Used by
/// renderers with per-cell color control (multi-series xycharts).
pub fn colorize_text(text: &str, hex: &str, mode: ColorMode) -> String {
    if mode == ColorMode::None || text.is_empty() {
        return text.to_owned();
    }
    if mode == ColorMode::Html {
        let mut out = String::new();
        push_html_span(&mut out, hex, text);
        return out;
    }
    let mut out = ansi_fg(hex, mode);
    out.push_str(text);
    out.push_str(RESET);
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn palette_mapping_matches_reference() {
        assert_eq!(rgb_to_256(0x27, 0x27, 0x2a), 232 + 4);
        assert_eq!(rgb_to_256(255, 0, 0), 196);
        assert_eq!(ansi16_fg("#ff0000"), "\x1b[91m");
        assert_eq!(ansi16_fg("#ff4040"), "\x1b[31m");
        assert_eq!(ansi16_fg("#27272a"), "\x1b[90m");
        assert_eq!(truecolor_fg("#abc"), "\x1b[38;2;170;187;204m");
    }

    #[test]
    fn line_groups_runs_and_leaves_spaces_bare() {
        let theme = Theme::default();
        let cells = [
            ("─", Some(CharRole::Line)),
            ("─", Some(CharRole::Line)),
            (" ", None),
            ("A", Some(CharRole::Text)),
            ("<", None),
        ];
        assert_eq!(
            colorize_line(&cells, &theme, ColorMode::Truecolor),
            "\x1b[38;2;113;113;122m──\x1b[0m \x1b[38;2;39;39;42mA\x1b[0m<"
        );
        assert_eq!(
            colorize_line(&cells, &theme, ColorMode::Html),
            "<span style=\"color:#71717a\">──</span> <span style=\"color:#27272a\">A</span>&lt;"
        );
    }
}
