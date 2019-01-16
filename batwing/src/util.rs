/// Helper function that might appear in Rust stable at some point
/// (https://doc.rust-lang.org/stable/std/option/enum.Option.html#method.transpose)
pub fn transpose<T, E>(opt: Option<Result<T, E>>) -> Result<Option<T>, E> {
    opt.map_or(Ok(None), |res| res.map(Some))
}

#[cfg(test)]
mod tests {
    use super::transpose;

    #[derive(Debug, PartialEq)]
    struct TestError;

    type TestResult<T> = Result<T, TestError>;

    #[test]
    fn basic() {
        let a: Option<TestResult<i32>> = Some(Ok(2));
        assert_eq!(Ok(Some(2)), transpose(a));

        let b: Option<TestResult<i32>> = Some(Err(TestError));
        assert_eq!(Err(TestError), transpose(b));

        let c: Option<TestResult<i32>> = None;
        assert_eq!(Ok(None), transpose(c));
    }
}
