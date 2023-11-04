use std::env;

use globset::GlobMatcher;
use once_cell::sync::Lazy;

use crate::syntax_mapping::{make_glob_matcher, MappingTarget};

// Static syntax mappings generated from /src/syntax_mapping/builtins/ by the
// build script (/build/syntax_mapping.rs).
include!(concat!(
    env!("OUT_DIR"),
    "/codegen_static_syntax_mappings.rs"
));

// The defined matcher strings are analysed at compile time and converted into
// lazily-compiled `GlobMatcher`s. This is so that the string searches are moved
// from run time to compile time, thus improving startup performance.
//
// To any future maintainer (including possibly myself) wondering why there is
// not a `BuiltinMatcher` enum that looks like this:
//
// ```
// enum BuiltinMatcher {
//     Fixed(&'static str),
//     Dynamic(Lazy<Option<String>>),
// }
// ```
//
// Because there was. I tried it and threw it out.
//
// Naively looking at the problem from a distance, this may seem like a good
// design (strongly typed etc. etc.). It would also save on compiled size by
// extracting out common behaviour into functions. But while actually
// implementing the lazy matcher compilation logic, I realised that it's most
// convenient for `BUILTIN_MAPPINGS` to have the following type:
//
// `[(Lazy<Option<GlobMatcher>>, MappingTarget); N]`
//
// The benefit for this is that operations like listing all builtin mappings
// would be effectively memoised. The caller would not have to compile another
// `GlobMatcher` for rules that they have previously visited.
//
// Unfortunately, this means we are going to have to store a distinct closure
// for each rule anyway, which makes a `BuiltinMatcher` enum a pointless layer
// of indirection.
//
// In the current implementation, the closure within each generated rule simply
// calls either `build_matcher_fixed` or `build_matcher_dynamic`, depending on
// whether the defined matcher contains dynamic segments or not.

/// Compile a fixed glob string into a glob matcher.
///
/// A failure to compile is a fatal error.
///
/// Used internally by `Lazy<Option<GlobMatcher>>`'s lazy evaluation closure.
fn build_matcher_fixed(from: &str) -> GlobMatcher {
    make_glob_matcher(from).expect("A builtin fixed glob matcher failed to compile")
}

/// Join a list of matcher segments to create a glob string, replacing all
/// environment variables, then compile to a glob matcher.
///
/// Returns `None` if any replacement fails, or if the joined glob string fails
/// to compile.
///
/// Used internally by `Lazy<Option<GlobMatcher>>`'s lazy evaluation closure.
fn build_matcher_dynamic(segs: &[MatcherSegment]) -> Option<GlobMatcher> {
    // join segments
    let mut buf = String::new();
    for seg in segs {
        match seg {
            MatcherSegment::Text(s) => buf.push_str(s),
            MatcherSegment::Env(var) => {
                let replaced = env::var(var).ok()?;
                buf.push_str(&replaced);
            }
        }
    }
    // compile glob matcher
    let matcher = make_glob_matcher(&buf).ok()?;
    Some(matcher)
}

/// A segment of a dynamic builtin matcher.
///
/// Used internally by `Lazy<Option<GlobMatcher>>`'s lazy evaluation closure.
#[derive(Clone, Debug)]
enum MatcherSegment {
    Text(&'static str),
    Env(&'static str),
}
