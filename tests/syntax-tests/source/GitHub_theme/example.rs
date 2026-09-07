// A light theme keeps comments separate from code.
use std::collections::HashMap;

fn greet(name: &str) -> String {
    let count = 42;
    let ready = true;
    format!("Hello, {name}: {count} {ready}")
}

struct Entry {
    name: String,
    enabled: bool,
}
