//! Minimal cursor over one line of Mermaid source, shared by every parser.
//!
//! Mermaid is line-oriented with a handful of sigils per line, so a
//! backtracking cursor with `eat`/`take_while` is all the grammar needs.

/// Byte-offset cursor over a `&str` with cheap save/restore for backtracking.
#[derive(Clone, Copy, Debug)]
pub struct Cursor<'a> {
    src: &'a str,
    pos: usize,
}

impl<'a> Cursor<'a> {
    pub const fn new(src: &'a str) -> Self {
        Self { src, pos: 0 }
    }

    /// Unconsumed remainder.
    #[inline]
    pub fn rest(&self) -> &'a str {
        &self.src[self.pos..]
    }

    #[inline]
    pub const fn at_end(&self) -> bool {
        self.pos >= self.src.len()
    }

    /// Current byte offset, for `reset` after a failed alternative.
    #[inline]
    pub const fn pos(&self) -> usize {
        self.pos
    }

    #[inline]
    pub const fn reset(&mut self, pos: usize) {
        self.pos = pos;
    }

    /// Text consumed since `start`.
    #[inline]
    pub fn since(&self, start: usize) -> &'a str {
        &self.src[start..self.pos]
    }

    #[inline]
    pub fn peek(&self) -> Option<char> {
        self.rest().chars().next()
    }

    /// Second unconsumed char.
    pub fn peek2(&self) -> Option<char> {
        self.rest().chars().nth(1)
    }

    /// Consume and return one char.
    pub fn bump(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.pos += c.len_utf8();
        Some(c)
    }

    /// Consume `lit` if the input starts with it.
    pub fn eat(&mut self, lit: &str) -> bool {
        if self.rest().starts_with(lit) {
            self.pos += lit.len();
            true
        } else {
            false
        }
    }

    /// Consume `lit` if the input starts with it, ignoring ASCII case.
    pub fn eat_ignore_ascii_case(&mut self, lit: &str) -> bool {
        let ok = self
            .rest()
            .get(..lit.len())
            .is_some_and(|head| head.eq_ignore_ascii_case(lit));
        if ok {
            self.pos += lit.len();
        }
        ok
    }

    pub fn eat_char(&mut self, c: char) -> bool {
        if self.peek() == Some(c) {
            self.pos += c.len_utf8();
            true
        } else {
            false
        }
    }

    /// Consume the first literal that matches, longest alternatives first as
    /// listed by the caller.
    pub fn eat_any(&mut self, lits: &[&'static str]) -> Option<&'static str> {
        lits.iter().copied().find(|lit| self.eat(lit))
    }

    /// Consume chars while `pred` holds; returns the consumed slice.
    pub fn take_while(&mut self, pred: impl Fn(char) -> bool) -> &'a str {
        let start = self.pos;
        while let Some(c) = self.peek() {
            if !pred(c) {
                break;
            }
            self.pos += c.len_utf8();
        }
        self.since(start)
    }

    /// Consume up to (not including) the first `c`; `None` (nothing consumed)
    /// when `c` never occurs.
    pub fn take_until_char(&mut self, c: char) -> Option<&'a str> {
        let end = self.rest().find(c)?;
        let start = self.pos;
        self.pos += end;
        Some(self.since(start))
    }

    /// Consume up to (not including) the first occurrence of `lit`.
    pub fn take_until(&mut self, lit: &str) -> Option<&'a str> {
        let end = self.rest().find(lit)?;
        let start = self.pos;
        self.pos += end;
        Some(self.since(start))
    }

    /// Consume the rest of the input.
    pub fn take_rest(&mut self) -> &'a str {
        let start = self.pos;
        self.pos = self.src.len();
        self.since(start)
    }

    /// Skip Unicode whitespace; returns how many chars were skipped.
    pub fn skip_ws(&mut self) -> usize {
        self.take_while(char::is_whitespace).chars().count()
    }

    /// Skip whitespace and require at least one char of it.
    pub fn expect_ws(&mut self) -> bool {
        self.skip_ws() > 0
    }

    /// Consume a run of non-whitespace chars (JavaScript `\S+`).
    pub fn word(&mut self) -> Option<&'a str> {
        let w = self.take_while(|c| !c.is_whitespace());
        (!w.is_empty()).then_some(w)
    }

    /// Consume `[A-Za-z0-9_]+`.
    pub fn ident(&mut self) -> Option<&'a str> {
        let w = self.take_while(is_word_char);
        (!w.is_empty()).then_some(w)
    }

    /// Consume a double-quoted string, returning its body (no escapes in
    /// Mermaid).
    pub fn quoted(&mut self) -> Option<&'a str> {
        let start = self.pos;
        if !self.eat_char('"') {
            return None;
        }
        if let Some(body) = self.take_until_char('"') {
            self.pos += 1;
            Some(body)
        } else {
            self.pos = start;
            None
        }
    }

    /// Whether the previous and next chars form a JavaScript `\b` boundary.
    pub fn at_word_boundary(&self) -> bool {
        let before = self.src[..self.pos]
            .chars()
            .next_back()
            .is_some_and(is_word_char);
        let after = self.peek().is_some_and(is_word_char);
        before != after
    }
}

/// JavaScript `\w`.
#[inline]
pub const fn is_word_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

/// Split on the first run of whitespace (JavaScript `\s+`); `None` when
/// there is none.
pub fn split_ws_once(s: &str) -> Option<(&str, &str)> {
    let start = s.find(char::is_whitespace)?;
    let rest = s[start..].trim_start();
    Some((&s[..start], rest))
}

/// JavaScript `String.prototype.trim` (Unicode whitespace + BOM).
pub fn js_trim(s: &str) -> &str {
    s.trim_matches(|c: char| c.is_whitespace() || c == '\u{feff}')
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cursor_backtracks_and_lexes_primitives() {
        let mut c = Cursor::new(r#"A -->|go| B["x y"]"#);
        assert_eq!(c.ident(), Some("A"));
        assert!(c.expect_ws());
        let save = c.pos();
        assert!(!c.eat("==>"));
        assert!(c.eat("-->"));
        assert!(c.eat_char('|'));
        assert_eq!(c.take_until_char('|'), Some("go"));
        c.reset(save);
        assert_eq!(c.eat_any(&["-->", "--"]), Some("-->"));
        assert!(c.eat("|go|"));
        c.skip_ws();
        assert_eq!(c.word(), Some("B[\"x"));
        let mut q = Cursor::new(r#""x y" tail"#);
        assert_eq!(q.quoted(), Some("x y"));
        assert_eq!(q.rest(), " tail");
        assert_eq!(split_ws_once("a  b c"), Some(("a", "b c")));
    }
}
