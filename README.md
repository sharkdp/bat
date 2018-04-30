# bat

[![Build Status](https://travis-ci.org/sharkdp/bat.svg?branch=master)](https://travis-ci.org/sharkdp/bat)
[![Version info](https://img.shields.io/crates/v/bat.svg)](https://crates.io/crates/bat)

*A cat(1) clone with syntax highlighting and Git integration.*

## Features

### Syntax highlighting

`bat` supports a large number of programming and markup languages (see below):

![Syntax highlighting example](https://imgur.com/rGsdnDe.png)

### Git integration

`bat` communicates with `git` to show modifications with respect to the index (see left side bar):

![Git integration example](https://i.imgur.com/2lSW4RE.png)

## Installation

Check out the [Release page](https://github.com/sharkdp/bat/releases) for binary builds and Debian packages.

If you want to build to compile `bat` from source, you need Rust 1.22 or higher.
You can then use `cargo` to build everything:

``` bash
cargo install bat
```

### Get themes and syntax highlighting definitions

`bat` uses the excellent [`syntect`](https://github.com/trishume/syntect/) library for syntax highlighting. `syntect` uses Sublime Text language definitions and themes.

To get a big package of languages and a Monokai-variant as a theme, run:

``` bash
mkdir -p ~/.config/bat/themes
cd ~/.config/bat/themes
git clone https://github.com/jonschlinkert/sublime-monokai-extended
ln -s "sublime-monokai-extended/Monokai Extended.tmTheme" Default.tmTheme

mkdir -p ~/.config/bat/syntax
cd ~/.config/bat/syntax
git clone https://github.com/sublimehq/Packages/
rm -rf Packages/Markdown
git clone https://github.com/jonschlinkert/sublime-markdown-extended
```
