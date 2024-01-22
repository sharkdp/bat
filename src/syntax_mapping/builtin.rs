use std::{env, iter::Enumerate, slice, sync::mpsc, thread};

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

/// The maximum number of offload workers.
const MAX_OFFLOAD_WORKERS: usize = 8;
/// The minimum number of built glob matchers remaining before offload workers
/// start building the next batch.
const OFFLOAD_PREEMPT_MARGIN: usize = 2;

/// An iterator over the builtin mappings that offloads glob matcher building to
/// worker threads.
#[derive(Debug)]
pub struct OffloadIter {
    /// The iterator tracking the item returned by `next()`.
    next_iter: Enumerate<slice::Iter<'static, (Lazy<Option<GlobMatcher>>, MappingTarget<'static>)>>,

    /// The index of the next item that is to be fed to workers.
    ///
    /// Used to determine whether the next offload batch should be triggered.
    build_idx: usize,
    /// The iterator tracking the next item that is to be fed to workers.
    build_iter: slice::Iter<'static, (Lazy<Option<GlobMatcher>>, MappingTarget<'static>)>,

    /// Control channels for worker threads.
    workers: Vec<mpsc::Sender<&'static Lazy<Option<GlobMatcher>>>>,
}

impl OffloadIter {
    pub fn new() -> Self {
        let worker_count = match thread::available_parallelism() {
            Ok(n) => (n.get() - 1).min(MAX_OFFLOAD_WORKERS), // leave 1 for main thread
            Err(_) => 0,
        };

        let workers = (0..worker_count)
            .map(|_| {
                let (cmd_tx, cmd_rx) = mpsc::channel();
                thread::spawn(move || loop {
                    match cmd_rx.recv() {
                        Ok(cell) => {
                            Lazy::force(cell);
                        }
                        Err(_) => break, // cmd_tx dropped; nothing more to do
                    }
                }); // no need for the join handle; thread will halt when cmd_tx is dropped
                cmd_tx
            })
            .collect();

        Self {
            next_iter: BUILTIN_MAPPINGS.iter().enumerate(),
            build_idx: 0,
            build_iter: BUILTIN_MAPPINGS.iter(),
            workers,
        }
    }
}

impl Iterator for OffloadIter {
    type Item = &'static (Lazy<Option<GlobMatcher>>, MappingTarget<'static>);

    fn next(&mut self) -> Option<Self::Item> {
        let (idx, item) = self.next_iter.next()?;

        if idx + OFFLOAD_PREEMPT_MARGIN < self.build_idx
            || idx + OFFLOAD_PREEMPT_MARGIN >= BUILTIN_MAPPINGS.len()
        {
            // no further work needed at this point
            return Some(item);
        }

        // feed jobs to workers
        for cmd_tx in self.workers.iter() {
            match self.build_iter.next() {
                Some((unbuilt, _)) => {
                    cmd_tx
                        .send(unbuilt)
                        .expect("Offload worker should not hang up before main thread");
                    self.build_idx += 1;
                }
                None => {
                    break;
                }
            }
        }

        // halt workers if no longer needed
        if self.build_idx >= BUILTIN_MAPPINGS.len() {
            // workers stop when their current job is finished and cmd_tx is dropped
            self.workers.clear();
        }

        Some(item)
    }
}
