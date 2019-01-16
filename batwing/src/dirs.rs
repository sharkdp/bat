use directories::ProjectDirs;

// this is from when bat did not have batwing yet.  With some refactoring this
// can be moved elsewhere.
lazy_static! {
    pub static ref PROJECT_DIRS: ProjectDirs =
        ProjectDirs::from("", "", "bat").expect("Could not get home directory");
}
