# bat

[![Build Status](https://travis-ci.org/sharkdp/bat.svg?branch=master)](https://travis-ci.org/sharkdp/bat)
[![Version info](https://img.shields.io/crates/v/bat.svg)](https://crates.io/crates/bat)

A *cat(1)* clone with syntax highlighting and Git integration.

## Features

### Syntax highlighting

`bat` supports syntax highlighting for a large number of programming and markup languages:

![Syntax highlighting example](https://imgur.com/rGsdnDe.png)

### Git integration

`bat` communicates with `git` to show modifications with respect to the index (see left side bar):

![Git integration example](https://i.imgur.com/2lSW4RE.png)

## Installation

### From binaries

Check out the [Release page](https://github.com/sharkdp/bat/releases) for binary builds and Debian packages.

### Arch Linux

On Arch Linux, you can install [the AUR package](https://aur.archlinux.org/packages/bat/) via yaourt, or manually:

```bash
git clone https://aur.archlinux.org/bat.git
cd bat
makepkg -si
```

### From source

If you want to build to compile `bat` from source, you need Rust 1.22 or higher.
You can then use `cargo` to build everything:

``` bash
cargo install bat
```

## Customization

`bat` uses the excellent [`syntect`](https://github.com/trishume/syntect/) library for syntax highlighting. `syntect` can read any [Sublime Text `.sublime-syntax` files](https://www.sublimetext.com/docs/3/syntax.html) and themes.

To build your own language-set and theme, follow these steps:

Create a folder with a syntax highlighting theme:
``` bash
mkdir -p ~/.config/bat/themes
cd ~/.config/bat/themes

# Download a theme, for example:
git clone https://github.com/jonschlinkert/sublime-monokai-extended

# Create a 'Default.tmTheme' link
ln -s "sublime-monokai-extended/Monokai Extended.tmTheme" Default.tmTheme
```

Create a folder with language definition files:
``` bash
mkdir -p ~/.config/bat/syntax
cd ~/.config/bat/syntax

# Download some language definition files, for example:
git clone https://github.com/sublimehq/Packages/
rm -rf Packages/Markdown
git clone https://github.com/jonschlinkert/sublime-markdown-extended
```

Finally, use the following command to parse all these files into a binary
cache:
``` bash
bat init-cache
```
