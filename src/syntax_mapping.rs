use std::borrow::Cow;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct SyntaxMapping(HashMap<String, String>);

impl SyntaxMapping {
    pub fn new() -> SyntaxMapping {
        SyntaxMapping(HashMap::new())
    }

    pub fn insert(&mut self, from: String, to: String) -> Option<String> {
        self.0.insert(from, to)
    }

    pub fn replace<'a>(&self, input: &'a str) -> Cow<'a, str> {
        let mut out = Cow::from(input);
        if let Some(value) = self.0.get(input) {
            out = Cow::from(value.clone())
        }
        out
    }
}

#[test]
fn basic() {
    let mut map = SyntaxMapping::new();
    map.insert("Cargo.lock".into(), "toml".into());
    map.insert(".ignore".into(), ".gitignore".into());

    assert_eq!("toml", map.replace("Cargo.lock"));
    assert_eq!("other.lock", map.replace("other.lock"));

    assert_eq!(".gitignore", map.replace(".ignore"));
}
