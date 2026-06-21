<p align="center">
  <img src="logo-header.svg" alt="bat - a cat clone with wings"><br>
  <a href="https://github.com/sharkdp/bat/actions?query=workflow%3ACICD"><img src="https://github.com/sharkdp/bat/workflows/CICD/badge.svg" alt="Build Status"></a>
  <img src="https://img.shields.io/crates/l/bat.svg" alt="license">
  <a href="https://crates.io/crates/bat"><img src="https://img.shields.io/crates/v/bat.svg?colorB=319e8c" alt="Version info"></a><br>
  一個具備語法高亮與 Git 整合功能的 <i>cat(1)</i> 替代品。
</p>

<p align="center">
  <a href="#語法高亮">主要功能</a> •
  <a href="#如何使用">如何使用</a> •
  <a href="#安裝">安裝</a> •
  <a href="#自訂">自訂</a> •
  <a href="#專案目標與替代方案">專案目標與替代方案</a><br>
  [<a href="../README.md">English</a>]
  [<a href="README-zh.md">简体中文</a>]
  [繁體中文]
  [<a href="README-ja.md">日本語</a>]
  [<a href="README-ko.md">한국어</a>]
  [<a href="README-ru.md">Русский</a>]
</p>

### 贊助者

特別感謝我們最大的 <a href="sponsors.md">贊助者</a>：<br>

<p>
<a href="https://www.warp.dev/bat">
  <img src="sponsors/warp-logo.png" width="200" alt="Warp">
  <br>
  <strong>Warp, the intelligent terminal</strong>
  <br>
  <sub>Available on MacOS, Linux, Windows</sub>
</a>
</p>

### 語法高亮

`bat` 支援大量程式語言與標記語言的語法高亮：

![Syntax highlighting example](https://imgur.com/rGsdnDe.png)

### Git 整合

`bat` 會與 `git` 溝通，以顯示相對於索引的修改內容（見左側邊欄）：

![Git integration example](https://i.imgur.com/2lSW4RE.png)

### 顯示不可列印字元

你可以使用 `-A`/`--show-all` 選項來顯示並高亮不可列印字元：

![Non-printable character example](https://i.imgur.com/WndGp9H.png)

### 自動分頁

預設情況下，如果輸出內容大於一個畫面可顯示的範圍，`bat` 會將自己的輸出導向分頁器（例如 `less`）。
如果你想讓 `bat` 永遠像 `cat` 一樣工作（永不分頁輸出），可以在命令列或設定檔中將 `--paging=never` 設為選項。
如果你打算在 shell 設定中把 `cat` 設成 `bat` 的別名，可以使用 `alias cat='bat --paging=never'` 來保留預設行為。

#### 檔案串接

即使已設定分頁器，你仍然可以用 `bat` 來串接檔案 :wink:。
每當 `bat` 偵測到非互動式終端機（也就是當你把輸出管線到另一個程序或檔案時），它就會作為 `cat` 的直接替代品，無論 `--pager` 選項的值為何，都會退回為輸出純文字檔案內容。

## 如何使用

在終端機中顯示單一檔案

```bash
bat README.md
```

一次顯示多個檔案

```bash
bat src/*.rs
```

從 stdin 讀取，自動判斷語法（注意：只有在能從檔案第一行判斷語法時，高亮才會生效，通常是透過像 `#!/bin/sh` 這樣的 shebang）

```bash
curl -s https://sh.rustup.rs | bat
```

從 stdin 讀取，明確指定語言

```bash
yaml2json .travis.yml | json_pp | bat -l json
```

顯示並高亮不可列印字元：
```bash
bat -A /etc/hosts
```

把它當成 `cat` 的替代品使用：

```bash
bat > note.md  # quickly create a new file

bat header.md content.md footer.md > document.md

bat -n main.rs  # show line numbers (only)

bat f - g  # output 'f', then stdin, then 'g'.
```

### 與其他工具整合

#### `fzf`

你可以把 `bat` 當成 [`fzf`](https://github.com/junegunn/fzf) 的預覽器。要這樣做，請使用 `bat` 的 `--color=always` 選項來強制輸出彩色內容。你也可以使用 `--line-range` 選項來縮短大型檔案的載入時間：

```bash
fzf --preview "bat --color=always --style=numbers --line-range=:500 {}"
```

更多資訊請參考 [`fzf` 的 README](https://github.com/junegunn/fzf#preview-window)。

#### `find` 或 `fd`

你可以使用 `find` 的 `-exec` 選項，以 `bat` 預覽所有搜尋結果：

```bash
find … -exec bat {} +
```

如果你剛好在使用 [`fd`](https://github.com/sharkdp/fd)，也可以使用 `-X`/`--exec-batch` 選項達成相同效果：

```bash
fd … -X bat
```

#### `ripgrep`

搭配 [`batgrep`](https://github.com/eth-p/bat-extras/blob/master/doc/batgrep.md)，`bat` 可以作為 [`ripgrep`](https://github.com/BurntSushi/ripgrep) 搜尋結果的輸出工具。

```bash
batgrep needle src/
```

#### `tail -f`

`bat` 可以和 `tail -f` 結合，持續監看指定檔案並顯示語法高亮。

```bash
tail -f /var/log/pacman.log | bat --paging=never -l log
```

請注意，為了讓這樣的用法正常運作，我們必須關閉分頁。同時我們也明確指定了語法（`-l log`），因為在這種情況下無法自動偵測。

#### `git`

你可以將 `bat` 和 `git show` 搭配使用，以適當的語法高亮檢視指定檔案的舊版本：

```bash
git show v0.6.0:src/main.rs | bat -l rs
```

#### `git diff`

你可以將 `bat` 和 `git diff` 搭配使用，以適當的語法高亮檢視程式碼變更周圍的內容：
```bash
batdiff() {
    git diff --name-only --relative --diff-filter=d -z | xargs -0 bat --diff
}
```
如果你偏好把這當成獨立工具使用，可以看看 [`bat-extras`](https://github.com/eth-p/bat-extras) 裡的 `batdiff`。

如果你想要更多對 Git 與 diff 操作的支援，請看看 [`delta`](https://github.com/dandavison/delta)。

#### `xclip`

`bat` 輸出中的行號與 Git 修改標記，可能會讓複製檔案內容變得困難。為了避免這個問題，你可以使用 `-p`/`--plain` 選項呼叫 `bat`，或直接把輸出透過管線送給 `xclip`：
```bash
bat main.cpp | xclip
```
`bat` 會偵測輸出已被重新導向，並改為輸出純文字檔案內容。

#### `man`

只要設定 `MANPAGER` 環境變數，`bat` 就能作為 `man` 的彩色分頁器：

```bash
export MANPAGER="bat -plman"
man 2 select
```
（在某些較舊的 Debian 或 Ubuntu 版本中，可執行檔名稱是 `batcat` 而不是 `bat`）

如果你希望把它包裝成新的命令，也可以使用 [`batman`](https://github.com/eth-p/bat-extras/blob/master/doc/batman.md)。

請注意，[Manpage 語法](../assets/syntaxes/02_Extra/Manpage.sublime-syntax) 是在這個儲存庫中開發的，目前仍需要一些改進。

#### `prettier` / `shfmt` / `rustfmt`

[`prettybat`](https://github.com/eth-p/bat-extras/blob/master/doc/prettybat.md) 腳本是一個包裝器，會先格式化程式碼，再用 `bat` 印出。

#### `Warp`

<a href="https://app.warp.dev/drive/folder/-Bat-Warp-Pack-lxhe7HrEwgwpG17mvrFSz1">
  <img src="sponsors/warp-pack-header.png" alt="Warp">
</a>

#### 高亮 `--help` 訊息

你可以用 `bat` 為 help 文字加上色彩：`$ cp --help | bat -plhelp`

你也可以為此建立包裝函式：

```bash
# in your .bashrc/.zshrc/*rc
alias bathelp='bat --plain --language=help'
help() {
    "$@" --help 2>&1 | bathelp
}
```

之後你就可以執行 `$ help cp` 或 `$ help git commit`。

當你使用 `zsh` 時，也可以使用全域別名，完整覆蓋 `-h` 與 `--help`：

```bash
alias -g -- -h='-h 2>&1 | bat --language=help --style=plain'
alias -g -- --help='--help 2>&1 | bat --language=help --style=plain'
```

對於 `fish`，你可以使用縮寫：

```fish
abbr -a --position anywhere -- --help '--help | bat -plhelp'
abbr -a --position anywhere -- -h '-h | bat -plhelp'
```

這樣一來，你就能繼續使用 `cp --help`，但得到彩色的 help 頁面。

> [!TIP]
> 若之後要移除這些縮寫，請執行：
> ```fish
> abbr -e -- --help
> abbr -e -- -h
> ```
> 縮寫名稱前面的 `--` 是必要的，因為 `--help` 和 `-h` 以破折號開頭，否則會被視為 `abbr` 自己的旗標。

請注意，在某些情況下，`-h` 可能不是 `--help` 的簡寫（例如 `ls`）。如果你需要把 `-h`
當作命令參數使用，可以在參數前加上 `\`（例如 `ls \-h`）來跳脫上面定義的別名。

若 help 語法有任何問題，請在[這個儲存庫](https://github.com/victor-gp/cmd-help-sublime-syntax)回報。


## 安裝

<!--

Installation instructions need to:
* be for widely used systems
* be non-obvious
* be from somewhat official sources

-->

[![Packaging status](https://repology.org/badge/vertical-allrepos/bat-cat.svg?columns=3&exclude_unsupported=1)](https://repology.org/project/bat-cat/versions)

### 在 Ubuntu 上（使用 `apt`）
*... 以及其他基於 Debian 的 Linux 發行版。*

`bat` 可在 [Ubuntu 20.04（"Focal"）起](https://packages.ubuntu.com/search?keywords=bat&exact=1)與 [Debian 2021 年 8 月起（Debian 11 - "Bullseye"）](https://packages.debian.org/bullseye/bat)取得。

如果你的 Ubuntu/Debian 安裝版本夠新，只要執行：

```bash
sudo apt install bat
```

**重要**：在某些較舊的 Ubuntu/Debian 版本上，可執行檔會以 `batcat` 而不是 `bat` 安裝（因為[與另一個套件名稱衝突](https://github.com/sharkdp/bat/issues/982)）。在較新的版本中，可執行檔名稱則是 `bat`。如果安裝後 `bat --version` 無法執行，請改試 `batcat --version`。你可以設定 `bat -> batcat` 的符號連結或別名，以避免因此產生問題，並與其他發行版保持一致：
``` bash
mkdir -p ~/.local/bin
ln -s /usr/bin/batcat ~/.local/bin/bat
```

將 `batcat` 設為 `bat` 的別名範例：
```bash
alias bat="batcat"
```

### 在 Ubuntu 上（使用最新的 `.deb` 套件）
*... 以及其他基於 Debian 的 Linux 發行版。*

如果套件尚未推送到你的 Ubuntu/Debian 安裝來源，或你想要最新版的 `bat`，請從[發行頁面](https://github.com/sharkdp/bat/releases)下載最新的 `.deb` 套件，並透過以下方式安裝：

```bash
sudo dpkg -i bat_0.18.3_amd64.deb  # adapt version number and architecture
```

### 在 Alpine Linux 上

只要已啟用適當的套件庫，你就可以從官方來源安裝 [`bat` 套件](https://pkgs.alpinelinux.org/packages?name=bat)：

```bash
apk add bat
```

### 在 Arch Linux 上

你可以從官方來源安裝 [`bat` 套件](https://www.archlinux.org/packages/extra/x86_64/bat/)：

```bash
pacman -S bat
```

### 在 Fedora 上

你可以從官方的 [Fedora Modular](https://docs.fedoraproject.org/en-US/modularity/using-modules/) 套件庫安裝 [`bat` 套件](https://koji.fedoraproject.org/koji/packageinfo?packageID=27506)。

```bash
dnf install bat
```

### 在 Gentoo Linux 上

你可以從官方來源安裝 [`bat` 套件](https://packages.gentoo.org/packages/sys-apps/bat)：

```bash
emerge sys-apps/bat
```

### 在 FreeBSD 上

你可以使用 pkg 安裝預先編譯好的 [`bat` 套件](https://www.freshports.org/textproc/bat)：

```bash
pkg install bat
```

或自行從 FreeBSD ports 建置：

```bash
cd /usr/ports/textproc/bat
make install
```

### 在 OpenBSD 上

你可以使用 [`pkg_add(1)`](https://man.openbsd.org/pkg_add.1) 安裝 `bat` 套件：

```bash
pkg_add bat
```

### 透過 nix

你可以使用 [nix 套件管理器](https://nixos.org/nix) 安裝 `bat`：

```bash
nix-env -i bat
```

### 在 openSUSE 上

你可以使用 zypper 安裝 `bat`：

```bash
zypper install bat
```

### 透過 snap 套件

目前沒有推薦使用的 snap 套件。
可能已有現有套件可用，但它們並非官方支援，且可能包含[問題](https://github.com/sharkdp/bat/issues/1519)。

### 在 macOS（或 Linux）上透過 Homebrew

你可以使用 [Homebrew](https://formulae.brew.sh/formula/bat) 安裝 `bat`：

```bash
brew install bat
```

### 在 macOS 上透過 MacPorts

或者使用 [MacPorts](https://ports.macports.org/port/bat/summary) 安裝 `bat`：

```bash
port install bat
```

### 在 Windows 上

在 Windows 上安裝 `bat` 有幾種方式。安裝完成後，請參考[「在 Windows 上使用 `bat`」](#在-windows-上使用-bat)一節。

#### 先決條件

你需要安裝 [Visual C++ Redistributable](https://learn.microsoft.com/en-us/cpp/windows/latest-supported-vc-redist#latest-microsoft-visual-c-redistributable-version)

#### 使用 WinGet

你可以透過 [WinGet](https://learn.microsoft.com/en-us/windows/package-manager/winget) 安裝 `bat`：

```bash
winget install sharkdp.bat
```

#### 使用 Chocolatey

你可以透過 [Chocolatey](https://chocolatey.org/packages/Bat) 安裝 `bat`：
```bash
choco install bat
```

#### 使用 Scoop

你可以透過 [scoop](https://scoop.sh/) 安裝 `bat`：
```bash
scoop install bat
```

#### 使用預先編譯的二進位檔：

你可以從[發行頁面](https://github.com/sharkdp/bat/releases)下載預先編譯的二進位檔，

你需要安裝 [Visual C++ Redistributable](https://support.microsoft.com/en-us/help/2977003/the-latest-supported-visual-c-downloads) 套件。

### 從二進位檔安裝

請查看[發行頁面](https://github.com/sharkdp/bat/releases)，其中提供許多不同架構的 `bat` 預先編譯版本。也有靜態連結的二進位檔可供使用：請尋找檔名中包含 `musl` 的壓縮檔。

### 從原始碼安裝

如果你想從原始碼建置 `bat`，需要 Rust 1.79.0 或更高版本。之後可以使用 `cargo` 來建置所有內容：

#### 從本機原始碼安裝
```bash
cargo install --path . --locked
```
> [!NOTE]
> 上面的 `--path .` 指的是原始碼所在的目錄，而不是 `bat` 將會安裝到的位置。
> 更多資訊請參考 [`cargo install`](https://doc.rust-lang.org/cargo/commands/cargo-install.html) 文件。

#### 從 `crates.io` 安裝
```bash
cargo install --locked bat
```

請注意，像 man page 或 shell completion 這類額外檔案，無法透過這兩種方式自動安裝。
如果是從本機原始碼安裝，它們會由 `cargo` 產生，並應可在 cargo target 資料夾下的 `build` 目錄中找到。

此外，也可以執行以下命令來取得 shell completion：
```bash
bat --completion <shell>
# see --help for supported shells
```

## 自訂

### 高亮主題

使用 `bat --list-themes` 可取得所有可用語法高亮主題的清單。預設情況下，`bat` 在深色與淺色主題時，分別使用 `Monokai Extended` 與 `Monokai Extended Light`。若要選擇 `TwoDark` 主題，請在執行 `bat` 時使用 `--theme=TwoDark` 選項，或將 `BAT_THEME` 環境變數設為 `TwoDark`。在 shell 啟動檔中使用 `export BAT_THEME="TwoDark"` 可使這項變更永久生效。或者，也可以使用 `bat` 的[設定檔](#設定檔)。

如果你想在自訂檔案上預覽不同主題，可以使用以下命令（需要 [`fzf`](https://github.com/junegunn/fzf)）：
```bash
bat --list-themes | fzf --preview="bat --theme={} --color=always /path/to/file"
```

`bat` 會根據終端機背景色自動挑選合適的主題。
你可以使用 `--theme-dark` / `--theme-light` 選項，或 `BAT_THEME_DARK` / `BAT_THEME_LIGHT` 環境變數，自訂所使用的主題。這在你經常切換深色與淺色模式時特別有用。

你也可以依照[下方的「新增主題」章節](#新增主題)來使用自訂主題。

### 8-bit 主題

`bat` 有三個主題會始終使用 [8-bit 色彩](https://en.wikipedia.org/wiki/ANSI_escape_code#Colors)，即使終端機支援 truecolor 也是如此：

- `ansi` 在任何終端機上看起來都不錯。它使用 3-bit 色彩：黑、紅、綠、黃、藍、洋紅、青與白。
- `base16` 是為 [base16](https://github.com/tinted-theming/home) 終端機主題設計的。它依照 [base16 styling guidelines](https://github.com/tinted-theming/home/blob/main/styling.md) 使用 4-bit 色彩（3-bit 色彩加上亮色變體）。
- `base16-256` 是為 [tinted-shell](https://github.com/tinted-theming/tinted-shell) 設計的。它會以 16 到 21 的 8-bit 色彩取代某些亮色。**請勿**只是因為你使用 256 色終端機、卻沒有使用 tinted-shell 就選用它。

雖然這些主題的色彩選擇較受限，但它們比 truecolor 主題有三個優點：

- 相容性最佳。有些終端工具不支援超過 3-bit 的色彩。
- 會隨終端機主題變更自動調整。即使是已經輸出的內容也一樣。
- 在視覺上與其他終端機軟體更協調。

### 輸出樣式

你可以使用 `--style` 選項來控制 `bat` 輸出的外觀。例如，你可以使用 `--style=numbers,changes`，只顯示 Git 變更與行號，而不顯示格線與檔案標頭。若要讓這些變更永久生效，請設定 `BAT_STYLE` 環境變數，或使用 `bat` 的[設定檔](#設定檔)。

> [!TIP]
> 如果你在 `bat` 的設定檔中指定了預設樣式，就可以在單次執行 `bat` 時，透過 `--style` 命令列參數變更顯示元件。
> 在元件前加上 `+` 或 `-`，即可在目前樣式上新增或移除它。
>
> 例如，如果你的設定檔包含 `--style=full,-snip`，你就可以使用
> `--style=-grid,+snip` 執行 bat，以移除格線並重新加入 `snip` 元件。
> 或者，如果你想完全覆寫樣式，可以使用 `--style=numbers` 只顯示行號。

### 裝飾元素

預設情況下，`bat` 只會在輸出到互動式終端機時顯示裝飾元素（例如行號、檔案標頭、格線邊框等）。你可以透過 `--decorations` 選項控制這個行為。使用 `--decorations=always` 可讓輸出管線到其他命令時仍顯示裝飾，或使用 `--decorations=never` 完全停用。可用值為 `auto`（預設）、`never` 與 `always`。

另外還有 `--force-colorization` 選項，它是 `--decorations=always --color=always` 的別名。如果你想在把 `bat` 的輸出管線到其他程式時仍保留色彩與裝飾，這會很有用。

### 新增語法 / 語言定義

如果你發現 `bat` 沒有提供某個特定語法，可以依照下列說明，輕鬆地將新語法加入目前的 `bat` 安裝中。

`bat` 使用優秀的 [`syntect`](https://github.com/trishume/syntect/) 程式庫來進行語法高亮。`syntect` 能讀取任何 [Sublime Text `.sublime-syntax` 檔案](https://www.sublimetext.com/docs/3/syntax.html)與主題。

尋找 Sublime Syntax 套件的一個好資源是 [Package Control](https://packagecontrol.io/)。找到語法之後：

1. 建立一個放置語法定義檔的資料夾：

   ```bash
   mkdir -p "$(bat --config-dir)/syntaxes"
   cd "$(bat --config-dir)/syntaxes"

   # Put new '.sublime-syntax' language definition files
   # in this folder (or its subdirectories), for example:
   git clone https://github.com/tellnobody1/sublime-purescript-syntax
   ```

2. 接著使用以下命令，將這些檔案解析進二進位快取：

   ```bash
   bat cache --build
   ```

3. 最後，使用 `bat --list-languages` 檢查新語言是否可用。

   如果你之後想回到預設設定，請執行：

   ```bash
   bat cache --clear
   ```

4. 如果你認為某個特定語法應該預設包含在 `bat` 中，請在閱讀[這裡](assets.md)的政策與說明後，考慮開一張「syntax request」工單：[Open Syntax Request](https://github.com/sharkdp/bat/issues/new?labels=syntax-request&template=syntax_request.md)。

### 新增主題

這和新增語法定義的方式非常相似。
> [!NOTE]
> 自訂主題必須儲存在 [`.tmTheme` 檔案](https://www.sublimetext.com/docs/color_schemes_tmtheme.html)中。
> 目前尚不支援較新的 `.sublime-color-scheme` 檔案。

首先，建立一個放置新語法高亮主題的資料夾：
```bash
mkdir -p "$(bat --config-dir)/themes"
cd "$(bat --config-dir)/themes"

# Download a theme in '.tmTheme' format, for example:
git clone https://github.com/greggb/sublime-snazzy

# Update the binary cache
bat cache --build
```

最後，使用 `bat --list-themes` 檢查新主題是否可用。
> [!NOTE]
> `bat` 會使用 `.tmTheme` 檔名作為主題名稱。

### 新增或變更檔案類型關聯

你可以使用 `--map-syntax` 命令列選項來新增（或變更現有）檔名模式。這個選項接受 `pattern:syntax` 形式的參數，其中 `pattern` 是會比對檔名與絕對檔案路徑的 glob 模式，而 `syntax` 則是支援語言的完整名稱（可使用 `bat --list-languages` 查看總覽）。

**注意：** 為了持久化設定，你可能會想把這個選項加到 `bat` 的[設定檔](#設定檔)中，而不是每次都在命令列上單次傳入。一般來說，如果你只是想手動指定某個檔案的語言，直接使用 `-l` 即可。

範例：若要讓所有副檔名為 `.conf` 的檔案使用 "INI" 語法高亮，請使用
```bash
--map-syntax='*.conf:INI'
```

範例：若要讓所有名為 `.ignore` 的檔案（完全符合）以 "Git Ignore" 語法開啟，請使用：
```bash
--map-syntax='.ignore:Git Ignore'
```

範例：若要讓 `/etc/apache2` 子目錄下所有 `.conf` 檔案使用 "Apache Conf" 語法，請使用（這個映射已內建）：
```bash
--map-syntax='/etc/apache2/**/*.conf:Apache Conf'
```

### 使用不同的分頁器

`bat` 會使用 `PAGER` 環境變數所指定的分頁器。如果這個變數沒有設定，預設會使用 `less`。你也可以使用 bat 內建的分頁器 `--pager=builtin`，或將 `BAT_PAGER` 環境變數設為 `"builtin"`。

如果你想使用不同的分頁器，可以修改 `PAGER` 變數，或設定 `BAT_PAGER` 環境變數來覆蓋 `PAGER` 中指定的值。

>[!NOTE]
> 若 `PAGER` 為 `more` 或 `most`，`bat` 會靜默改用 `less`，以確保色彩支援。

如果你想把命令列參數傳給分頁器，也可以透過 `PAGER`/`BAT_PAGER` 變數設定：

```bash
export BAT_PAGER="less -RFK"
```

除了使用環境變數外，你也可以使用 `bat` 的[設定檔](#設定檔)來設定分頁器（`--pager` 選項）。


### 使用 `less` 作為分頁器

當使用 `less` 作為分頁器時，`bat` 會自動額外傳遞一些選項給 `less` 以改善使用體驗。具體來說會傳入 `-R`/`--RAW-CONTROL-CHARS`、`-F`/`--quit-if-one-screen`、`-K`/`--quit-on-intr`，以及在某些情況下的 `-X`/`--no-init` 和/或 `-S`/`--chop-long-lines`。

>[!IMPORTANT]
> 若符合以下任一情況，就不會加入這些選項：
> - 分頁器名稱不是 `less`。
> - `--pager` 參數包含任何命令列參數（例如 `--pager="less -R"`）。
> - `BAT_PAGER` 環境變數包含任何命令列參數（例如 `export BAT_PAGER="less -R"`）
>
> 若符合以下情況，也不會加入 `--quit-if-one-screen`：
> - 使用了 `--paging=always` 參數。
> - `BAT_PAGING` 環境變數被設為 `always`。

需要 `-R`/`--RAW-CONTROL-CHARS` 選項，才能正確解譯 ANSI 色彩。

`-F`/`--quit-if-one-screen` 選項會指示 `less`，若輸出高度小於終端機可視高度時立即退出。這對小檔案很方便，因為你不用按 `q` 才能離開分頁器。

`-K`/`--quit-on-intr` 選項會指示 `less` 在收到中斷訊號時立即退出。這有助於確保在發生 SIGINT 時，`less` 會和 `bat` 一起結束。

對於舊於 530 的 `less` 版本（Windows 上則是舊於 558），會加入 `-X`/`--no-init` 選項，以修正 `-F`/`--quit-if-one-screen` 功能的 bug。不幸的是，這也會破壞 `less` 的滑鼠滾輪支援。如果你想在較舊版本的 `less` 啟用滑鼠滾動，且不介意失去 quit-if-one-screen 功能，可以將分頁器（透過 `--pager` 或 `BAT_PAGER`）設為 `less -R`。對於 `less` 530 或更新版本，通常開箱即用。

當使用 `bat` 的 `-S`/`--chop-long-lines` 選項時，會加入 `-S`/`--chop-long-lines` 給 `less`。這會告訴 `less` 截斷所有超過終端機寬度的長行。

### 縮排

`bat` 會自行將 tab 展開為 4 個空白，而不是依賴分頁器。若要修改這個設定，只要加上 `--tabs` 參數，指定你想顯示的空白數量即可。

**注意**：為分頁器設定 tab stop（例如透過 `bat` 的 `--pager` 參數，或 `less` 的 `LESS` 環境變數）將不會生效，因為分頁器收到的已經是展開成空白的內容，而不是 tab。加入這個行為是為了避免側邊欄導致的縮排問題。使用 `--tabs=0` 呼叫 `bat` 可覆寫此行為，讓 tab 交由分頁器處理。

### 深色模式

如果你在 **macOS** 上使用深色模式功能，可能會想根據作業系統主題為 `bat` 設定不同主題。以下範例會在 _深色模式_ 下使用 `default` 主題，在 _淺色模式_ 下使用 `GitHub` 主題。

```bash
alias cat="bat --theme auto:system --theme-dark default --theme-light GitHub"
```

同樣的深色模式功能現在也可在 **GNOME** 中使用，會影響 `org.gnome.desktop.interface color-scheme` 設定。以下程式碼將上面的設定轉換為使用該設定。

```bash
# .bashrc
sys_color_scheme_is_dark() {
    condition=$(gsettings get org.gnome.desktop.interface color-scheme)
    condition=$(echo "$condition" | tr -d "[:space:]'")
    if [ $condition == "prefer-dark" ]; then
        return 0
    else
        return 1
    fi
}

bat_alias_wrapper() {
    #get color scheme
    sys_color_scheme_is_dark
    if [[ $? -eq 0 ]]; then
        # bat command with dark color scheme
        bat --theme=default "$@"
    else
        # bat command with light color scheme
        bat --theme=GitHub "$@"
    fi
}
alias cat='bat_alias_wrapper'
```


## 設定檔

`bat` 也可以透過設定檔進行自訂。檔案的位置取決於你的作業系統。若要取得系統上的預設路徑，請執行
```bash
bat --config-file
```

或者，你可以使用 `BAT_CONFIG_PATH` 或 `BAT_CONFIG_DIR` 環境變數，分別讓 `bat` 指向非預設位置的設定檔或設定目錄：
```bash
export BAT_CONFIG_PATH="/path/to/bat/bat.conf"
export BAT_CONFIG_DIR="/path/to/bat"
```

你也可以使用 `--generate-config-file` 選項建立預設設定檔。
```bash
bat --generate-config-file
```

現在也有系統層級的設定檔，在 Linux 與 Mac OS 上位於 `/etc/bat/config`，在 Windows 上位於 `C:\ProgramData\bat\config`。如果系統層級設定檔存在，使用者設定內容會直接附加在它後面。

### 格式

設定檔其實就是命令列參數的簡單清單。使用 `bat --help` 可查看所有可用選項與值的完整列表。另外，你也可以在某行前加上 `#` 字元來加入註解。

範例設定檔：
```bash
# Set the theme to "TwoDark"
--theme="TwoDark"

# Show line numbers, Git modifications and file header (but no grid)
--style="numbers,changes,header"

# Use italic text on the terminal (not supported on all terminals)
--italic-text=always

# Use C++ syntax for Arduino .ino files
--map-syntax "*.ino:C++"
```

## 在 Windows 上使用 `bat`

`bat` 在 Windows 上大多能直接使用，但有少數功能可能需要額外設定。

### 先決條件

你需要安裝 [Visual C++ Redistributable](https://support.microsoft.com/en-us/help/2977003/the-latest-supported-visual-c-downloads) 套件。

### 分頁

Windows 內建的分頁器只有功能相當有限的 `more`。你可以從 [less 官方首頁](http://www.greenwoodsoftware.com/less/download.html) 或透過 [Chocolatey](https://chocolatey.org/packages/Less) 下載 Windows 版 `less`。要使用它，請把二進位檔放進 `PATH` 中的目錄，或[定義環境變數](#使用不同的分頁器)。[Chocolatey 套件](#在-windows-上)會自動安裝 `less`。

### 色彩

自 [v1511](https://en.wikipedia.org/wiki/Windows_10_version_history#Version_1511_(November_Update)) 起，Windows 10 的 `conhost.exe`（命令提示字元）與 PowerShell，以及較新版本的 bash，都原生支援色彩。在更早版本的 Windows 上，你可以使用 [Cmder](http://cmder.app/)，它內含 [ConEmu](https://conemu.github.io/)。

**注意：** 舊版 `less` 在 Windows 上無法正確解譯色彩。要修正此問題，你可以在安裝 Git 時，把可選的 Unix 工具加入 PATH。如果你沒有安裝其他分頁器，也可以透過傳入 `--paging=never` 或將 `BAT_PAGER` 設為空字串，完全停用分頁。

### Cygwin

Windows 上的 `bat` 不原生支援 Cygwin 的 Unix 風格路徑（`/cygdrive/*`）。當把絕對的 Cygwin 路徑當作參數傳給 `bat` 時，會遇到以下錯誤：`The system cannot find the path specified. (os error 3)`

這可以透過建立包裝器，或把下列函式加入你的 `.bash_profile` 來解決：

```bash
bat() {
    local index
    local args=("$@")
    for index in $(seq 0 ${#args[@]}) ; do
        case "${args[index]}" in
        -*) continue;;
        *)  [ -e "${args[index]}" ] && args[index]="$(cygpath --windows "${args[index]}")";;
        esac
    done
    command bat "${args[@]}"
}
```

## 疑難排解

### 輸出亂碼

如果輸入檔案包含色彩碼、其他 ANSI 跳脫序列或控制字元，`bat` 在進行語法高亮與文字換行時就可能出問題，導致輸出變得混亂。

如果你的 `bat` 版本支援 `--strip-ansi=auto` 選項，可以用它在語法高亮前移除這類序列。或者，你也可以傳入 `--color=never --wrap=never` 選項，同時停用語法高亮與換行。

> [!NOTE]
> `--strip-ansi` 的 `auto` 選項會在語法為純文字時，避免移除跳脫序列。

### 終端機與色彩

`bat` 能處理*支援*與*不支援* truecolor 的終端機。不過，大多數語法高亮主題的色彩並未針對 8-bit 色彩最佳化。因此，強烈建議你使用支援 24-bit truecolor 的終端機（`terminator`、`konsole`、`iTerm2` 等），或改用專為受限色彩集設計的基本 [8-bit 主題](#8-bit-主題)。
更多細節與支援 truecolor 的終端機完整清單，請參閱[這篇文章](https://gist.github.com/XVilka/8346728)。

請確認你的 truecolor 終端機有將 `COLORTERM` 變數設為 `truecolor` 或 `24bit`。否則，`bat` 將無法判斷是否支援 24-bit 跳脫序列（並退回到 8-bit 色彩）。

### 行號與格線幾乎看不清楚

請嘗試不同的主題（可使用 `bat --list-themes` 查看清單）。`OneHalfDark` 與 `OneHalfLight` 主題提供較亮的格線與行號顏色。

### 檔案編碼

`bat` 原生支援 UTF-8 與 UTF-16。對於其他檔案編碼，你可能需要先轉換成 UTF-8，因為這些編碼通常無法自動偵測。你可以使用 `iconv` 來完成。
範例：如果你有一個使用 Latin-1（ISO-8859-1）編碼的 PHP 檔案，可以執行：
``` bash
iconv -f ISO-8859-1 -t UTF-8 my-file.php | bat
```
注意：如果 `bat` 無法自動偵測語法，你可能還需要使用 `-l`/`--language` 選項。

## 開發

```bash
# Recursive clone to retrieve all submodules
git clone --recursive https://github.com/sharkdp/bat

# Build (debug version)
cd bat
cargo build --bins

# Run unit tests and integration tests
cargo test

# Install (release version)
cargo install --path . --locked

# Build a bat binary with modified syntaxes and themes
bash assets/create.sh
cargo install --path . --locked --force
```

如果你想建置一個把 `bat` 的美化列印功能當成函式庫使用的應用程式，請參考 [API 文件](https://docs.rs/bat/)。
請注意，當你把 `bat` 當作函式庫相依時，必須啟用 `regex-onig` 或 `regex-fancy` 其中之一的 feature。

## 貢獻

請參考 [`CONTRIBUTING.md`](../CONTRIBUTING.md) 指南。

## 維護者

- [sharkdp](https://github.com/sharkdp)
- [eth-p](https://github.com/eth-p)
- [keith-hall](https://github.com/keith-hall)
- [Enselic](https://github.com/Enselic)

## 安全性弱點

請參閱 [`SECURITY.md`](../SECURITY.md)。

## 專案目標與替代方案

`bat` 致力於達成以下目標：

- 提供美觀、進階的語法高亮
- 與 Git 整合以顯示檔案修改
- 成為（POSIX）`cat` 的直接替代品
- 提供對使用者友善的命令列介面

如果你在尋找類似的程式，還有很多替代方案。請參考[這份文件](alternatives.md)進行比較。

## 授權條款
Copyright (c) 2018-2025 [bat-developers](https://github.com/sharkdp/bat).

你可以依自己的選擇，依據 MIT License 或 Apache License 2.0 的條款使用 `bat`。
