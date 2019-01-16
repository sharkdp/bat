use std::borrow::Cow;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct SyntaxMapping(HashMap<String, String>);

impl SyntaxMapping {
    pub fn new() -> SyntaxMapping {
        SyntaxMapping(HashMap::new())
    }

    pub fn insert(&mut self, from: impl Into<String>, to: impl Into<String>) -> Option<String> {
        self.0.insert(from.into(), to.into())
    }

    pub fn replace<'a>(&self, input: impl Into<Cow<'a, str>>) -> Cow<'a, str> {
        let input = input.into();
        match self.0.get(input.as_ref()) {
            Some(s) => Cow::from(s.clone()),
            None => input,
        }
    }
}

#[test]
fn basic() {
    let mut map = SyntaxMapping::new();
    map.insert("Cargo.lock", "toml");
    map.insert(".ignore", ".gitignore");

    assert_eq!("toml", map.replace("Cargo.lock"));
    assert_eq!("other.lock", map.replace("other.lock"));

    assert_eq!(".gitignore", map.replace(".ignore"));
}
