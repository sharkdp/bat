Release checklist:

- [ ] Check for outdated dependencies (`cargo outdated`)
- [ ] Optional: update dependencies with `cargo update`.
      See also https://deps.rs/repo/github/sharkdp/bat
- [ ] Update syntaxes and themes (`cargo install -f --path .; assets/create.sh`).
- [ ] Update README (features, usage, languages, ..).
- [ ] Update man page


- [ ] Update version in `Cargo.toml`. Run `cargo build` to update `Cargo.lock`
- [ ] Update version in README and possibly update minimum Rust version
- [ ] Run `cargo fmt`
- [ ] Run `cargo test`
- [ ] Run `cargo install --path . -f`
- [ ] Test new features & command-line options
- [ ] Check `-h` and `--help` texts


- [ ] `cargo publish --dry-run --allow-dirty`.
- [ ] write GitHub release notes
- [ ] check if CI succeeds
- [ ] `git tag vX.Y.Z; git push --tags`
- [ ] check binaries (that were uploaded via Travis/AppVeyor)
- [ ] publish to crates.io by cloning a fresh repo and calling `cargo publish`.
- [ ] Inform package maintainers about the update:
    - https://www.archlinux.org/packages/community/x86_64/bat/
