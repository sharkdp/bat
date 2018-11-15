<p align="center">
  <img src="logo-header.svg" alt="bat - a cat clone with wings"><br>
  <a href="https://travis-ci.org/sharkdp/bat"><img src="https://travis-ci.org/sharkdp/bat.svg?branch=master" alt="Build Status"></a>
  <a href="https://ci.appveyor.com/project/sharkdp/bat"><img src="https://ci.appveyor.com/api/projects/status/cptsmtbiwbnr2vhf?svg=true"></a>
  <img src="https://img.shields.io/crates/l/bat.svg" alt="license">
  <a href="https://crates.io/crates/bat"><img src="https://img.shields.io/crates/v/bat.svg?colorB=319e8c" alt="Version info"></a><br>
  A <i>cat(1)</i> clone with syntax highlighting and Git integration.
</p>

<p align="center">
  <a href="#syntax-highlighting">特徴</a> •
  <a href="#how-to-use">使い方</a> •
  <a href="#installation">インストール</a> •
  <a href="#customization">カスタマイズ</a> •
  <a href="#project-goals-and-alternatives">プロジェクトの目標と既存の類似したOSS</a> •
  翻訳 [<a href="https://github.com/chinanf-boy/bat-zh">中文</a>][<a href="README-ja">日本文</a>]
</p>

### シンタックスハイライト

`bat` は多くのプログラミング言語やマークアップ言語のシンタックスハイライトに対応しています。

![Syntax highlighting example](https://imgur.com/rGsdnDe.png)

### Gitの統合


`bat` は `git` とも連携しており、差分を表現する記号が表示されます（図の左端）:

![Git integration example](https://i.imgur.com/2lSW4RE.png)

### 空白文字の可視化

`-A`/`--show-all` オプションをつけることで、空白文字を可視化できます:

![Non-printable character example](https://i.imgur.com/WndGp9H.png)

### 自動呼び出し

出力が1つの画面に対して大きすぎる場合、`bat` は自身の出力を `less` にパイプで繋げることが可能です。

### ファイルの連結

あなたはさらにファイルを連結させるに使うことも可能です:wink:。
`bat` は非対話型のターミナルを検出すると、いつでも `cat` の完全互換として振る舞い、
プレーンなファイルを表示します。

## 使い方

単一のファイルを表示させたい

```bash
> bat README.md
```

複数のファイルを一度に表示させたい場合

```bash
> bat src/*.rs
```

標準入力から自動的に構文を決定させたい場合

```bash
> curl -s https://sh.rustup.rs | bat
```

Read from stdin, specify the language explicitly
標準入力から明示的に言語を指定したい場合

```bash
> yaml2json .travis.yml | json_pp | bat -l json
```

空白文字を可視化させたい場合:
```bash
> bat -A /etc/hosts
```

`cat` の代わりに `bat` を使用する際の例:

```bash
bat > note.md  # quickly create a new file

bat header.md content.md footer.md > document.md

bat -n main.rs  # show line numbers (only)

bat f - g  # output 'f', then stdin, then 'g'.
```

## インストール

###  Ubuntu
*... や他のDebianベースのLinuxディストリビューション*

最新の `.deb` パッケージを[こちら](https://github.com/sharkdp/bat/releases)からダウンロードして、インストールしてください:

``` bash
sudo dpkg -i bat_0.9.0_amd64.deb  # adapt version number and architecture
```

###  Arch Linux

[Arch Linuxの公式リソース](https://www.archlinux.org/packages/community/x86_64/bat/)からインストールできます。

```bash
pacman -S bat
```

###  Void Linux

xbps-ininstall経由で `bat` をインストールできます。

```
xbps-install -S bat
```

###  FreeBSD

事前にコンパイル済みである[pkg](https://www.freshports.org/textproc/bat)をインストールできます:

```bash
pkg install bat
```

または、FreeBSDのportsから自身でビルドすることも可能です:

```bash
cd /usr/ports/textproc/bat
make install
```

###  nix

`bat` を[nix package manager](https://nixos.org/nix)経由でインストールすることができます:

```bash
nix-env -i bat
```

###  openSUSE

You can install `bat` with zypper:

`bat` をzypperでインストールすることができます:

```
zypper install bat
```

###  macOS

[Homebrew](http://braumeister.org/formula/bat)でインストールすることができます:


```bash
brew install bat
```

###  Windows

You can download prebuilt binaries from the [Release page],
or install it with :

事前にビルド済みのバイナリを[リリースページ](https://github.com/sharkdp/bat/releases)からダウンロードすることができます。または、[scoop](https://scoop.sh/)でインストールすることも可能です。

```bash
scoop install bat
```

[See below](#using-bat-on-windows) for notes.

###  Docker


コンテナ内で`bat` を使いたい方のために[Docker image](https://hub.docker.com/r/danlynn/bat/)が用意されています。

```bash
docker pull danlynn/bat
alias bat='docker run -it --rm -e BAT_THEME -e BAT_STYLE -e BAT_TABS -v "$(pwd):/myapp" danlynn/bat'
```

###  Ansible

[Ansible](https://www.ansible.com/)でインストールすることができます:

```bash
# Install role on local machine
ansible-galaxy install aeimer.install_bat
```

```yaml
---
# Playbook to install bat
- host: all
  roles:
    - aeimer.install_bat
```

- [Ansible Galaxy](https://galaxy.ansible.com/aeimer/install_bat)
- [GitHub](https://github.com/aeimer/ansible-install-bat)

これは以下のディストリビューションで動作するはずです:
- Debian/Ubuntu
- ARM (eg. Raspberry PI)
- Arch Linux
- Void Linux
- FreeBSD
- MacOS

### From binaries


多くの異なるアーキテクチャのためのプレビルドバージョンを[リリースページ](https://github.com/sharkdp/bat/releases)からチェックしてみてください。静的にリンクされている多くのバイナリも利用できます: ファイル名に `musl` を含むアーカイブを探します。

### From source


`bat` をソースからビルドしたいならば、Rust 1.29 以上の環境が必要です。そして、 `cargo` をビルドに対して使用します:

```bash
cargo install bat
```


`cmake` と `libz` 開発パッケージ（`libz-dev` または `libz-devel`）が正しくビルドをするためにインストールする必要があるかもしれません。

## カスタマイズ

### ハイライト テーマ

`bat --list-themes` を使うと現在利用可能なシンタックスハイライトのテーマを入手できます。 `TwoDark` テーマを選ぶためには `--theme=TwoDark` オプションをつけるか `BAT_THEME` という環境変数に `TwoDark` を代入する必要があります。 シェルの起動ファイルに `export BAT_THEME="TwoDark"` と定義すればその設定が変わることはないでしょう。あるいは、 `bat` の[設定ファイル](https://github.com/sharkdp/bat#configuration-file)を利用してください。


もし、ファイルにより異なるテーマを利用したい場合は、以下の[`fzf`](https://github.com/junegunn/fzf)を利用した以下のコマンドを使うことができます:
``` bash
bat --list-themes | fzf --preview="bat --theme={} --color=always /path/to/file"
```

`bat` はデフォルトだと黒い背景色のターミナルに適しています。しかし、`GitHub` や `OneHalfLight` のような白い背景色のテーマでもいい感じにすることができます。
以下のカスタムテーマを参考にしてみてください。
['Adding new themes' section below](https://github.com/sharkdp/bat#adding-new-themes)

### 出力のスタイル

`--style` を使うことで `bat` の表示の見た目を変更することができます。例えば、 `--style=numbers,changes` と入力します。すると、Gitの差分と行番号だけが表示され、グリッド線とファイルヘッダーは表示されません。
環境変数に `BAT_STYLE` を定義するとこれらの設定を永続的に使用することができます。
[設定ファイル](https://github.com/sharkdp/bat#configuration-file)を参考にしても良いでしょう。

### 新しい構文の追加 / 言語の定義

`bat` はシンタックスハイライトのための[`syntext`](https://github.com/trishume/syntect/)という最高にイケてるライブラリを使用しています。`syntect` はSublime Textのために作られた[`.sublime-syntax` file](https://www.sublimetext.com/docs/3/syntax.html)テーマです。新しい構文を定義するために以下の手順を行います。

構文定義ファイルを入れておくためのフォルダを作る:

```bash
BAT_CONFIG_DIR="$(bat cache --config-dir)"

mkdir -p "$BAT_CONFIG_DIR/syntaxes"
cd "$BAT_CONFIG_DIR/syntaxes"

# Put new '.sublime-syntax' language definition files
# in this folder (or its subdirectories), for example:
git clone https://github.com/tellnobody1/sublime-purescript-syntax
```

次のコマンドを使用して、これらのファイルをバイナリキャッシュに解析します:

```bash
bat cache --init
```

最後に `bat --list-languages` と入力すると新しい言語が利用可能かどうかチェックします。
デフォルトの設定に戻したいときは以下のコマンドを実行します:

```bash
bat cache --clear
```

### 新しいテーマの追加

これは構文を新しく定義するやり方と非常に似ています。

まず、新しいシンタックスハイライトのテーマのフォルダを作ります:
```bash
BAT_CONFIG_DIR="$(bat cache --config-dir)"

mkdir -p "$BAT_CONFIG_DIR/themes"
cd "$BAT_CONFIG_DIR/themes"

# Download a theme in '.tmTheme' format, for example:
git clone https://github.com/greggb/sublime-snazzy

# Update the binary cache
bat cache --init
```

最後に、 `bat --list-themes` で新しいテーマが利用可能がチェックします

### 異なるページャーの使用

`bat` は環境変数 `PAGER` に使用するページャーを明記します。
この環境変数が定義されていない場合、デフォルトで `less` が使用されます。もし、異なるページャーが使用したい場合は、`PAGER` を修正してください。または、`PAGER` を上書きする環境変数として `BAT_PAGER` を定義することも可能です。

もし、ページャーにコマンドライン引数を渡したい場合は、 `PAGER`/`BAT_PAGER` 環境変数を定義してください:
```bash
export BAT_PAGER="less -RF"
```

環境変数を利用する代わりに、 `bat` の[設定ファイル](https://github.com/sharkdp/bat#configuration-file)の `--pager` オプションを使用することも可能です。

**注意**: デフォルトにより、ページャーが `less` にセットされているならば `bat` は以下のコマンドラインを受け付けるでしょう。
ページャーのオプション： `-R`/`--RAW-CONTROL-CHARS`, `-F`/`--quit-if-one-screen` , `-X`/`--no-init`
最初の `-R` はANSIカラーを正しく解釈するために必要です。2番目の `-F` は出力サイズがターミナルの縦のサイズよりも小さいならば `less` に即座に終了するように命令します。したがって、少量のファイルのためにページャーを終了する `q` を押す必要がありません。3番目のオプションの `-X` は `less` の古いバージョンの機能である `--quit-if-one-screen` でバグを修正するために必要です。運の悪いことに、これは `less` のマウス-ホイールのサポートを無効にします。もし、マウス-ホイール スクローリングを有効にしたいなら、`-R` だけをオプションとしてつけてください（ただし、これは `--quit-if-one-screen` を無効にするでしょう）。または、 `less` の最近バージョンを利用し、 `-RF` をつけてください。 これは望ましいことに `--quit-if-one-screen` と マウス-ホイールスクローリングの両方を有効にするでしょう。

スクロールが未だ動作しないのであれば、加えて `-S` オプションを追加してみてください。

## 設定ファイル


`bat` は設定ファイルでカスタマイズすることが可能です。ファイルの場所はOSに依存します。使用しているOSのデフォルトパスを調べるには以下のコマンドを実行してください:
```
bat --config-file
```

また、環境変数 `BAT_CONFIG_PATH` に設定ファイルの場所を明記することもできます:
```bash
export BAT_CONFIG_PATH="/path/to/bat.conf"
```

### フォーマット


この設定ファイルはコマンドライン引数の単純なリストです。 `bat --help` を利用すると、利用可能なオプションとその値を閲覧することができます。さらに、`#` でコメント文を加えることができます。

設定ファイルの例:
```bash
# Set the theme to "TwoDark"
--theme="TwoDark"

# Show line numbers, Git modifications and file header (but no grid)
--style="numbers,changes,header"

# Use italic text on the terminal (not supported on all terminals)
--italic-text=always

# Add mouse scrolling support in less (does not work with older
# versions of "less")
--pager="less -FR"

# Use C++ syntax (instead of C) for .h header files
--map-syntax h:cpp

# Use "gitignore" highlighting for ".ignore" files
--map-syntax .ignore:.gitignore
```

## Windows での `bat` の利用

Windows 上で `bat` はほとんど動作しますが、いくつかの機能は設定を必要をする場合があります。

### ページング

Windows only includes a very limited pager in the form of `more`. You can download a Windows binary
for `less` [from its homepage](http://www.greenwoodsoftware.com/less/download.html) or [through
Chocolatey](https://chocolatey.org/packages/Less). To use it, place the binary in a directory in
your `PATH` or [define an environment variable](#using-a-different-pager).



### 色

Windows 10 は[v1151](https://en.wikipedia.org/wiki/Windows_10_version_history#Version_1511_(November_Update))からbashの新しいバージョンと同様に `conhost.exe`（コマンドプロンプト）と PowerShell の色をネイティブサポートしていません。Windows の新しいバージョンでは、[Cmder](http://cmder.net/)を使うことができます。これは[ConEmu](https://conemu.github.io/)を含んでいます。

**注意:** Git と MSYS の `less` はWindows上で色を正しく解釈しません。もし、あなたが他のページャーをインストールしていないのであれば、 `--paging=never` オプションを付け加えるまたは `BAT_PAGER` に空文字を設定することでページングを完全に無効にできます。

### Cygwin

Windows上の `bat` は Cygwin のunix風のpath(`/cygdrive/*`)をネイティブサポートしていません。
絶対的なcygwinパスを引数として受けたときに、 `bat` は以下のエラーを返すでしょう: `The system cannot find the path specified. (os error 3)`

wrapperを作成するまたは以下の関数を `.bash_profile` に追記することでこの問題を解決することができます:
```shell
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

## トラブルシューティング

### ターミナルと色

`bat` はターミナルがトゥルーカラーをサポートしている/していない関係なくサポートします。しかし、シンタックスハイライトのテーマの色が8-bitカラーに最適化されていない場合、24-bitであるトゥルーカラーをサポートしているターミナルを使用することを強く推奨します（`terminator`, `konsole`, `iTerm2`, ...）。
この[記事](https://gist.github.com/XVilka/8346728)には24-bitカラーがサポートされているターミナルの一覧が掲載されています。

本当の色をターミナルにセットするために、環境変数 `COLORTERM` に `truecolor` か `24bit` のどちらかを代入してください。さもなければ、 `bat` はどの色を使うのか決定することができません。または、24-bit エスケープシーケンスがサポートされません（そして、8-bit colorに戻ります）。

### 行番号とグリッド線がほとんど見えない

異なるテーマを試してみてください(`bat --list-themes` でテーマを閲覧できます)。 `OneHalfDark` と `OneHalfLight` テーマはグリッド線と線の色を明るくします。


## 開発

```bash
# Recursive clone to retrieve all submodules
git clone --recursive https://github.com/sharkdp/bat

# Build (debug version)
cd bat
cargo build

# Run unit tests and integration tests
cargo test

# Install (release version)
cargo install

# Build a bat binary with modified syntaxes and themes
bash assets/create.sh
cargo install -f
```

## プロジェクトの目標と既存の類似したOSS

`bat` は以下の目標を達成しようと試みています:

- 美しい高度なシンタックスハイライトの提供
- ファイルの差分を表示するためのGitとの連携
-  (POSIX) `cat` との完全互換
- ユーザーフレンドリーがコマンドラインインターフェースの提供

既存の類似したOSSはたくさんあります。 
`bat` とこれらを比較した表が[こちら](alternatives.md)にあります。
