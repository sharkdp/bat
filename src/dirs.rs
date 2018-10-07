use directories::ProjectDirs;

lazy_static! {
    pub static ref PROJECT_DIRS: ProjectDirs =
        ProjectDirs::from("", "", crate_name!()).expect("Could not get home directory");
}
