// Output the square of a number.
fn print_square(num: f64) {
    let result = f64::powf(num, 2.0);
    println!("The square of {:.2} is {:.2}.", num, result);
}
