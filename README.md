<p align="center">
  <img src="doc/logo-header.svg" alt="bat - a cat clone with wings"><br>
  <a href="Version info"><img src="https://travis-ci.org/sharkdp/bat.svg?branch=master" alt="Build Status"></a>
  <a href="https://crates.io/crates/bat"><img src="https://img.shields.io/crates/v/bat.svg" alt="Version info"></a><br>
  A <i>cat(1)</i> clone with syntax highlighting and Git integration.
</p>

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

#### On Arch Linux

You can install [the AUR package](https://aur.archlinux.org/packages/bat/)
via yaourt, or manually:

```bash
git clone https://aur.archlinux.org/bat.git
cd bat
makepkg -si
```

#### On FreeBSD

You can install a precompiled [`bat` package](https://www.freshports.org/textproc/bat) with pkg:

```sh
pkg install bat
```

or build it on your own from the FreeBSD ports:

```sh
cd /usr/ports/textproc/bat
make install
```

#### On macOS

You can install `bat` with [Homebrew](http://braumeister.org/formula/bat):

``` bash
brew install bat
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
mkdir -p "$BAT_CONFIG_DIR/syntaxes"
cd "$BAT_CONFIG_DIR/syntaxes"

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

## Project goals and alternatives

`bat` tries to achieve the following goals:

- Provide beautiful, advanced syntax highlighting
- Integrate with Git to show file modifications
- Be a drop-in replacement for (POSIX) `cat`
- Offer a user-friendly command-line interface

There are a lot of alternatives, if you are looking for similar programs. See
[this document](doc/alternatives.md) for a comparison.
