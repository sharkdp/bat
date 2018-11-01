use console::AnsiCodeIterator;

/// Expand tabs like an ANSI-enabled expand(1).
pub fn expand_tabs(line: &str, width: usize, cursor: &mut usize) -> String {
    let mut buffer = String::with_capacity(line.len() * 2);

    for chunk in AnsiCodeIterator::new(line) {
        match chunk {
            (text, true) => buffer.push_str(text),
            (mut text, false) => {
                while let Some(index) = text.find('\t') {
                    // Add previous text.
                    if index > 0 {
                        *cursor += index;
                        buffer.push_str(&text[0..index]);
                    }

                    // Add tab.
                    let spaces = width - (*cursor % width);
                    *cursor += spaces;
                    buffer.push_str(&*" ".repeat(spaces));

                    // Next.
                    text = &text[index + 1..text.len()];
                }

                *cursor += text.len();
                buffer.push_str(text);
            }
        }
    }

    buffer
}

pub fn replace_nonprintable(input: &mut Vec<u8>, output: &mut Vec<u8>, tab_width: usize) {
    output.clear();

    let tab_width = if tab_width == 0 { 4 } else { tab_width };

    for chr in input {
        match *chr {
            // space
            b' ' => output.extend_from_slice("•".as_bytes()),
            // tab
            b'\t' => {
                if tab_width == 1 {
                    output.extend_from_slice("↹".as_bytes());
                } else {
                    output.extend_from_slice("├".as_bytes());
                    output.extend_from_slice("─".repeat(tab_width - 2).as_bytes());
                    output.extend_from_slice("┤".as_bytes());
                }
            }
            // new line
            b'\n' => output.extend_from_slice("␤".as_bytes()),
            // carriage return
            b'\r' => output.extend_from_slice("␍".as_bytes()),
            // null
            0x00 => output.extend_from_slice("␀".as_bytes()),
            // bell
            0x07 => output.extend_from_slice("␇".as_bytes()),
            // backspace
            0x08 => output.extend_from_slice("␈".as_bytes()),
            // escape
            0x1B => output.extend_from_slice("␛".as_bytes()),
            // anything else
            _ => output.push(*chr),
        }
    }
}
