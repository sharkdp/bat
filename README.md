# bat

[![Build Status](https://travis-ci.org/sharkdp/bat.svg?branch=master)](https://travis-ci.org/sharkdp/bat)
[![Version info](https://img.shields.io/crates/v/bat.svg)](https://crates.io/crates/bat)

*A cat(1) clone with syntax highlighting and Git integration.*

## Features

### Syntax highlighting

`bat` supports syntax highlighting for a large number of programming and markup languages:

![Syntax highlighting example](https://imgur.com/rGsdnDe.png)

### Git integration

`bat` communicates with `git` to show modifications with respect to the index (see left side bar):

![Git integration example](https://i.imgur.com/2lSW4RE.png)

## Installation

Check out the [Release page](https://github.com/sharkdp/bat/releases) for binary builds and Debian packages.

### From source

If you want to build to compile `bat` from source, you need Rust 1.22 or higher.
Make sure that you have the devel-version of libopenssl installed (see instructions
[here](https://github.com/sfackler/rust-openssl)). You can then use `cargo` to build everything:

``` bash
cargo install bat
```
