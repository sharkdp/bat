struct Rectangle {
    width: u32,
    height: u32,
}

fn main() {
    let rect1 = Rectangle { width: 30, height: 50 };

    println!(
        "The perimeter of the rectangle is {} pixels.",
        perimeter(&rect1)
    );
    println!(r#"This line contains invalid utf8:  "�����"#;
}

fn area(rectangle: &Rectangle) -> u32 {
    rectangle.width * rectangle.height
}

fn perimeter(rectangle: &Rectangle) -> u32 {
    (rectangle.width + rectangle.height) * 2
}
