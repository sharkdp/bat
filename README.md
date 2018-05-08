# bat

[![Build Status](https://travis-ci.org/sharkdp/bat.svg?branch=master)](https://travis-ci.org/sharkdp/bat)
[![Version info](https://img.shields.io/crates/v/bat.svg)](https://crates.io/crates/bat)

A *cat(1)* clone with syntax highlighting and Git integration.

## Features

### Syntax highlighting

`bat` supports syntax highlighting for a large number of programming and markup
languages:

![Syntax highlighting example](https://imgur.com/rGsdnDe.png)

### Git integration

`bat` communicates with `git` to show modifications with respect to the index
(see left side bar):

![Git integration example](https://i.imgur.com/2lSW4RE.png)

### Automatic paging

`bat` can pipe its own output to `less` if the output is too large for one screen.

### File concatenation

Oh.. you can also use it to concatenate files :wink:. Whenever
`bat` detects a non-interactive terminal, it will fall back to printing
the plain file contents.

## Usage

Display a single file on the terminal

``` bash
> bat README.md
```

Display multiple files at once

``` bash
> bat src/*.rs
```

Explicitly specify the language

``` bash
> yaml2json .travis.yml | json_pp | bat -l json
```

``` bash
> curl -s https://raw.githubusercontent.com/sharkdp/bat/master/src/main.rs | bat -l rs
```


## Installation

### From binaries

Check out the [Release page](https://github.com/sharkdp/bat/releases) for
binary builds and Debian packages.

#### Arch Linux

On Arch Linux, you can install
[the AUR package](https://aur.archlinux.org/packages/bat/) via yaourt, or
manually:

```bash
git clone https://aur.archlinux.org/bat.git
cd bat
makepkg -si
```

### From source

If you want to build to compile `bat` from source, you need Rust 1.24 or
higher. You can then use `cargo` to build everything:

``` bash
cargo install bat
```

On macOS, you might have to install `cmake` (`brew install cmake`) in order for
some dependencies to be built.

## Customization

`bat` uses the excellent [`syntect`](https://github.com/trishume/syntect/)
library for syntax highlighting. `syntect` can read any
[Sublime Text `.sublime-syntax` file](https://www.sublimetext.com/docs/3/syntax.html)
and theme.

To build your own language-set and theme, follow these steps:

Create a folder with a syntax highlighting theme:

``` bash
BAT_CONFIG_DIR="$(bat cache --config-dir)"

mkdir -p "$BAT_CONFIG_DIR/themes"
cd "$BAT_CONFIG_DIR/themes"

# Download a theme, for example:
git clone https://github.com/jonschlinkert/sublime-monokai-extended

# Create a 'Default.tmTheme' link
ln -s "sublime-monokai-extended/Monokai Extended.tmTheme" Default.tmTheme
```

Create a folder with language definition files:

``` bash
mkdir -p "$BAT_CONFIG_DIR/syntax"
cd "$BAT_CONFIG_DIR/syntax"

# Download some language definition files, for example:
git clone https://github.com/sublimehq/Packages/
rm -rf Packages/Markdown
git clone https://github.com/jonschlinkert/sublime-markdown-extended
```

Finally, use the following command to parse all these files into a binary
cache:

``` bash
bat cache --init
```

If you ever want to go back to the default settings, call:

``` bash
bat cache --clear
```
