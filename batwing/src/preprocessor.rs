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

pub fn replace_nonprintable(input: &str, tab_width: usize) -> String {
    let mut output = String::new();

    let tab_width = if tab_width == 0 { 4 } else { tab_width };

    for chr in input.chars() {
        match chr {
            // space
            ' ' => output.push('•'),
            // tab
            '\t' => {
                if tab_width == 1 {
                    output.push('↹');
                } else {
                    output.push('├');
                    output.push_str(&"─".repeat(tab_width - 2));
                    output.push('┤');
                }
            }
            // line feed
            '\x0A' => output.push('␊'),
            // carriage return
            '\x0D' => output.push('␍'),
            // null
            '\x00' => output.push('␀'),
            // bell
            '\x07' => output.push('␇'),
            // backspace
            '\x08' => output.push('␈'),
            // escape
            '\x1B' => output.push('␛'),
            // anything else
            _ => output.push(chr),
        }
    }

    output
}
