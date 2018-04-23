# bat

[![Build Status](https://travis-ci.org/sharkdp/bat.svg?branch=master)](https://travis-ci.org/sharkdp/bat)
[![Version info](https://img.shields.io/crates/v/bat.svg)](https://crates.io/crates/bat)

*A cat(1) clone with wings.*

## Features

### Syntax highlighting

![bat src/main.rs](https://i.imgur.com/bcnFRSl.png)

![bat src/example.js](https://i.imgur.com/uyBewNX.png)

### Git integration

![bat src/index.html](https://i.imgur.com/yBQ31jm.png)

## Installation

``` bash
cargo install bat

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
