use bat::mermaid::{render, RenderOptions};

#[test]
fn test_render_flowchart() {
    let source = "graph TD\n  A[Start] --> B[Stop]";
    let rendered = render(source, &RenderOptions::default()).expect("render flowchart");
    assert!(rendered.contains("Start"));
    assert!(rendered.contains("Stop"));
    assert!(rendered.contains("│") || rendered.contains("|"));
}

#[test]
fn test_render_sequence() {
    let source = "sequenceDiagram\n  Alice->>Bob: Hello";
    let rendered = render(source, &RenderOptions::default()).expect("render sequence");
    assert!(rendered.contains("Alice"));
    assert!(rendered.contains("Bob"));
    assert!(rendered.contains("Hello"));
}

#[test]
fn test_render_class() {
    let source = "classDiagram\n  class Animal {\n    +String name\n  }";
    let rendered = render(source, &RenderOptions::default()).expect("render class");
    assert!(rendered.contains("Animal"));
    assert!(rendered.contains("name"));
}

#[test]
fn test_render_er() {
    let source = "erDiagram\n  CUSTOMER ||--o{ ORDER : places";
    let rendered = render(source, &RenderOptions::default()).expect("render er");
    assert!(rendered.contains("CUSTOMER"));
    assert!(rendered.contains("ORDER"));
}

#[test]
fn test_render_xychart() {
    let source = "xychart-beta\n  x-axis [Q1, Q2]\n  bar [10, 20]";
    let rendered = render(source, &RenderOptions::default()).expect("render xychart");
    assert!(rendered.contains("Q1"));
    assert!(rendered.contains("Q2"));
}

#[test]
fn test_render_invalid_source_returns_error() {
    let source = "not_a_diagram\nfoo bar baz";
    let err = render(source, &RenderOptions::default());
    assert!(err.is_err());
}
