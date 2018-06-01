<p align="center">
  <img src="doc/logo-header.svg" alt="bat - a cat clone with wings"><br>
  <a href="Version info"><img src="https://travis-ci.org/sharkdp/bat.svg?branch=master" alt="Build Status"></a>
  <img src="https://img.shields.io/crates/l/bat.svg" alt="license">
  <a href="https://crates.io/crates/bat"><img src="https://img.shields.io/crates/v/bat.svg?colorB=319e8c" alt="Version info"></a><br>
  A <i>cat(1)</i> clone with syntax highlighting and Git integration.
</p>

<p align="center">
  <a href="#syntax-highlighting">Key Features</a> •
  <a href="#how-to-use">How To Use</a> •
  <a href="#installation">Installation</a> •
  <a href="#customization">Customization</a> •
  <a href="#project-goals-and-alternatives">Project goals, alternatives</a>
</p>

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

## How to use

Display a single file on the terminal

```bash
> bat README.md
```

Display multiple files at once

```bash
> bat src/*.rs
```

Read from stdin, explicitly specify the language

```bash
> yaml2json .travis.yml | json_pp | bat -l json
```

```bash
> curl -s https://raw.githubusercontent.com/sharkdp/bat/master/src/main.rs | bat -l rs
```

As a replacement for `cat`:

```bash
bat > note.md  # quickly create a new file

bat header.md content.md footer.md > document.md

bat -n main.rs  # show line numbers (only)

bat f - g  # output 'f', then stdin, then 'g'.
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

```bash
pkg install bat
```

or build it on your own from the FreeBSD ports:

```bash
cd /usr/ports/textproc/bat
make install
```

#### On macOS

You can install `bat` with [Homebrew](http://braumeister.org/formula/bat):

```bash
brew install bat
```

### From source

If you want to build to compile `bat` from source, you need Rust 1.24 or
higher. You can then use `cargo` to build everything:

```bash
cargo install bat
```

On macOS, you might have to install `cmake` (`brew install cmake`) in order for
some dependencies to be built.

## Customization

### Highlighting theme

Use `bat --list-themes` to get a list of all available themes for syntax
highlighting. To select the `TwoDark` theme, for example, call `bat` with
the `--theme=TwoDark` option. Use `alias bat="bat --theme=TwoDark"` in your
shells startup file to make the change permanent.

### Output style

You can use the `--style` option to control the appearance of `bat`s output.
You can use `--style=numbers,changes`, for example, to show only Git changes
and line numbers but no grid and no file header.

### Add new syntaxes and highlighting themes

`bat` uses the excellent [`syntect`](https://github.com/trishume/syntect/)
library for syntax highlighting. `syntect` can read any
[Sublime Text `.sublime-syntax` file](https://www.sublimetext.com/docs/3/syntax.html)
and theme.

To build your own language-set and theme, follow these steps:

Create a folder with a syntax highlighting theme:

```bash
BAT_CONFIG_DIR="$(bat cache --config-dir)"

mkdir -p "$BAT_CONFIG_DIR/themes"
cd "$BAT_CONFIG_DIR/themes"

# Download a theme, for example:
git clone https://github.com/greggb/sublime-snazzy

# Create a link for the default theme
ln -sf "sublime-snazzy/Sublime Snazzy.tmTheme" Default.tmTheme
```

Create a folder with language definition files:

```bash
mkdir -p "$BAT_CONFIG_DIR/syntaxes"
cd "$BAT_CONFIG_DIR/syntaxes"

# Download some language definition files, for example:
git clone https://github.com/sublimehq/Packages
git clone https://github.com/danro/LESS-sublime
```

Finally, use the following command to parse all these files into a binary
cache:

```bash
bat cache --init
```

Use `bat --list-languages` and `bat --list-themes` to check if all languages and themes are
available.

If you ever want to go back to the default settings, call:

```bash
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
