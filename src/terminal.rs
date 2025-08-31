use nu_ansi_term::Color::{self, Fixed, Rgb};
use nu_ansi_term::{self, Style};

use syntect::highlighting::{self, FontStyle};

pub fn to_ansi_color(color: highlighting::Color, true_color: bool) -> Option<nu_ansi_term::Color> {
    if color.a == 0 {
        // Themes can specify one of the user-configurable terminal colors by
        // encoding them as #RRGGBBAA with AA set to 00 (transparent) and RR set
        // to the 8-bit color palette number. The built-in themes ansi, base16,
        // and base16-256 use this.
        Some(match color.r {
            // For the first 8 colors, use the Color enum to produce ANSI escape
            // sequences using codes 30-37 (foreground) and 40-47 (background).
            // For example, red foreground is \x1b[31m. This works on terminals
            // without 256-color support.
            0x00 => Color::Black,
            0x01 => Color::Red,
            0x02 => Color::Green,
            0x03 => Color::Yellow,
            0x04 => Color::Blue,
            0x05 => Color::Purple,
            0x06 => Color::Cyan,
            0x07 => Color::White,
            // For all other colors, use Fixed to produce escape sequences using
            // codes 38;5 (foreground) and 48;5 (background). For example,
            // bright red foreground is \x1b[38;5;9m. This only works on
            // terminals with 256-color support.
            //
            // TODO: When ansi_term adds support for bright variants using codes
            // 90-97 (foreground) and 100-107 (background), we should use those
            // for values 0x08 to 0x0f and only use Fixed for 0x10 to 0xff.
            n => Fixed(n),
        })
    } else if color.a == 1 {
        // Themes can specify the terminal's default foreground/background color
        // (i.e. no escape sequence) using the encoding #RRGGBBAA with AA set to
        // 01. The built-in theme ansi uses this.
        None
    } else if true_color {
        Some(Rgb(color.r, color.g, color.b))
    } else {
        Some(Fixed(ansi_colours::ansi256_from_rgb((
            color.r, color.g, color.b,
        ))))
    }
}

// Generate a rainbow color based on position and line number
pub fn rainbow_color(position: usize, line_number: usize, true_color: bool) -> nu_ansi_term::Color {
    let freq_h = 0.23;
    let freq_v = 0.1;

    let offx = (std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs()
        % 300) as f64
        / 300.0;

    if true_color {
        // Use 24-bit RGB colors for true color terminals
        let theta = position as f64 * freq_h / 5.0
            + line_number as f64 * freq_v
            + offx * std::f64::consts::PI;

        let offset = 0.1;
        let r = ((offset + (1.0 - offset) * (0.5 + 0.5 * (theta + 0.0).sin())) * 255.0) as u8;
        let g = ((offset
            + (1.0 - offset) * (0.5 + 0.5 * (theta + 2.0 * std::f64::consts::PI / 3.0).sin()))
            * 255.0) as u8;
        let b = ((offset
            + (1.0 - offset) * (0.5 + 0.5 * (theta + 4.0 * std::f64::consts::PI / 3.0).sin()))
            * 255.0) as u8;

        nu_ansi_term::Color::Rgb(r, g, b)
    } else {
        // Use 256-color palette
        let codes = [
            39, 38, 44, 43, 49, 48, 84, 83, 119, 118, 154, 148, 184, 178, 214, 208, 209, 203, 204,
            198, 199, 163, 164, 128, 129, 93, 99, 63, 69, 33,
        ];

        let ncc =
            offx * codes.len() as f64 + position as f64 * freq_h + line_number as f64 * freq_v;
        let index = ncc as usize % codes.len();

        nu_ansi_term::Color::Fixed(codes[index])
    }
}

// Process text character by character with rainbow colors
pub fn rainbow_text(
    text: &str,
    start_position: usize,
    line_number: usize,
    true_color: bool,
) -> String {
    let mut result = String::new();

    for (i, c) in text.chars().enumerate() {
        let position = start_position + i;
        let color = rainbow_color(position, line_number, true_color);

        // Create a style with the rainbow color
        let style = nu_ansi_term::Style::new().fg(color);
        result.push_str(&style.paint(c.to_string()).to_string());
    }

    result
}

pub fn as_terminal_escaped(
    style: highlighting::Style,
    text: &str,
    true_color: bool,
    colored: bool,
    italics: bool,
    background_color: Option<highlighting::Color>,
) -> String {
    if text.is_empty() {
        return text.to_string();
    }

    let mut style = if !colored {
        Style::default()
    } else {
        let mut color = Style {
            foreground: to_ansi_color(style.foreground, true_color),
            ..Style::default()
        };
        if style.font_style.contains(FontStyle::BOLD) {
            color = color.bold();
        }
        if style.font_style.contains(FontStyle::UNDERLINE) {
            color = color.underline();
        }
        if italics && style.font_style.contains(FontStyle::ITALIC) {
            color = color.italic();
        }
        color
    };

    style.background = background_color.and_then(|c| to_ansi_color(c, true_color));
    style.paint(text).to_string()
}
