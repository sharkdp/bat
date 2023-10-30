use std::borrow::Cow;

use syntect::highlighting::{Color, Style};

pub fn render_hex_colors(line: &mut Vec<(Style, Cow<str>)>) {
    let mut hex = String::with_capacity(6);
    let mut inserts = Vec::new();

    let mut collecting_hex = false;
    let mut region_offset = 0;

    for (region_index, region) in line.iter().enumerate() {
        let mut char_offset = 0;

        for (char_index, c) in region.1.chars().enumerate() {
            let mut decode_hex = false;

            if collecting_hex {
                match c {
                    '0'..='9' | 'a'..='f' | 'A'..='F' => {
                        hex.push(c);

                        if hex.len() == 6 {
                            decode_hex = true;
                            collecting_hex = false;
                        }
                    }
                    _ => {
                        if hex.len() == 3 {
                            decode_hex = true;
                        }

                        collecting_hex = false;
                    }
                }
            }

            let is_last_in_region = char_index == (region.1.len() - 1);
            let is_last_on_line = region_index == (line.len() - 1) && is_last_in_region;
            let hex_is_short = hex.len() == 3;
            let hex_is_long = hex.len() == 6;

            if is_last_on_line && hex_is_short && collecting_hex {
                decode_hex = true;
            }

            if decode_hex {
                let (r, g, b) = if hex_is_short {
                    let r = u8::from_str_radix(&hex[0..=0], 16).unwrap();
                    let g = u8::from_str_radix(&hex[1..=1], 16).unwrap();
                    let b = u8::from_str_radix(&hex[2..=2], 16).unwrap();

                    (r * 16 + r, g * 16 + g, b * 16 + b)
                } else if hex_is_long {
                    (
                        u8::from_str_radix(&hex[0..=1], 16).unwrap(),
                        u8::from_str_radix(&hex[2..=3], 16).unwrap(),
                        u8::from_str_radix(&hex[4..=5], 16).unwrap(),
                    )
                } else {
                    unreachable!("When decode_hex == true, the hex is either short (len == 3) or long (len == 6).")
                };

                inserts.push((
                    region_index + 1 + region_offset,
                    if (is_last_on_line && collecting_hex) || hex_is_long {
                        char_index - char_offset + 1
                    } else {
                        char_index - char_offset
                    },
                    Color { r, g, b, a: 255 },
                ));

                if !is_last_in_region {
                    region_offset += 2;

                    if hex_is_long {
                        char_offset = char_index + 1;
                    } else {
                        char_offset = char_index;
                    }
                } else {
                    region_offset += 1;
                }
            }

            if !collecting_hex && c == '#' {
                hex.clear();
                collecting_hex = true;
            }
        }
    }

    for (index, char_index, color) in inserts {
        let (base_style, base_text) = line.get(index - 1).cloned().unwrap();

        let left_text = base_text
            .chars()
            .enumerate()
            .filter(|(i, _)| i < &char_index)
            .map(|(_, c)| c)
            .collect::<String>();

        let right_text = base_text
            .chars()
            .enumerate()
            .filter(|(i, _)| i >= &char_index)
            .map(|(_, c)| c)
            .collect::<String>();

        let left = (base_style, Cow::Owned(left_text));
        let right = (base_style, Cow::Owned(right_text));

        let rendered_color = (
            Style {
                foreground: color,
                ..Default::default()
            },
            "\u{00A0}██\u{00A0}".into(),
        );

        line.remove(index - 1);

        line.insert(index - 1, left);
        line.insert(index, rendered_color);
        line.insert(index + 1, right);
    }
}
