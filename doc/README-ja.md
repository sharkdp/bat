<p align="center">
  <img src="logo-header.svg" alt="bat - a cat clone with wings"><br>
  <a href="https://travis-ci.org/sharkdp/bat"><img src="https://travis-ci.org/sharkdp/bat.svg?branch=master" alt="Build Status"></a>
  <a href="https://ci.appveyor.com/project/sharkdp/bat"><img src="https://ci.appveyor.com/api/projects/status/cptsmtbiwbnr2vhf?svg=true"></a>
  <img src="https://img.shields.io/crates/l/bat.svg" alt="license">
  <a href="https://crates.io/crates/bat"><img src="https://img.shields.io/crates/v/bat.svg?colorB=319e8c" alt="Version info"></a><br>
  シンタックスハイライトとGitとの連携機能付きの <i>cat(1)</i> クローン。
</p>

<p align="center">
  <a href="#シンタックスハイライト">特徴</a> •
  <a href="#使い方">使い方</a> •
  <a href="#インストール">インストール</a> •
  <a href="#カスタマイズ">カスタマイズ</a> •
  <a href="#プロジェクトの目標と既存の類似したOSS">プロジェクトの目標と既存の類似したOSS</a> •
  翻訳 [<a href="https://github.com/chinanf-boy/bat-zh">中文</a>][<a href="README-ja.md">日本語</a>][<a href="README-ko.md">한국어</a>]
</p>

### シンタックスハイライト

`bat` は多くのプログラミング言語やマークアップ言語の
シンタックスハイライトに対応しています:

![Syntax highlighting example](https://imgur.com/rGsdnDe.png)

### Gitの統合

`bat` は `git` とも連携しており、差分を表現する記号が表示されます
（図の左端）:

![Git integration example](https://i.imgur.com/2lSW4RE.png)

### 印刷できない文字の表示

`-A`/`--show-all` オプションをつけることで
印刷できない文字を可視化できます:

![Non-printable character example](https://i.imgur.com/WndGp9H.png)

### 自動ページング

出力が1つの画面に対して大きすぎる場合、`bat` は自身の出力を `less` にパイプで繋げることが可能です。

### ファイルの連結

あなたはさらにファイルを連結させるために使うことも可能です:wink:。
`bat` は非対話型のターミナルを検出すると、
いつでも `cat` の完全互換として振る舞い、
プレーンなファイルを表示します。

## 使い方

単一のファイルを表示させたい場合

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

### 他のツールとの統合

#### `find` or `fd`

`find` の `-exec` オプションを使用して、`bat` ですべての検索結果をプレビューできます:
```bash
find … -exec bat {} +
```

[`fd`](https://github.com/sharkdp/fd) を使用している場合は、`-X` /`-exec-batch` オプションを使用して同じことを行うことができます:
```bash
fd … -X bat
```

#### `ripgrep`

[`batgrep`](https://github.com/eth-p/bat-extras/blob/master/doc/batgrep.md) では、[`ripgrep`](https://github.com/BurntSushi/ripgrep) 検索結果のプリンターとして `bat` を使用できます。

```bash
batgrep needle src/
```

#### `tail -f`

`bat` を `tail -f` と組み合わせて、構文強調表示を使用して特定のファイルを継続的に監視できます。
```bash
tail -f /var/log/pacman.log | bat --paging=never -l log
```
注意事項：`tail -f`と組み合わせるには、ページングをオフにしなければなりません。また、この場合は構文が自動検出されないため、明示的に指定（`-l log`）しています。

#### `git`

`bat` を `git show` と組み合わせて、
適切な構文強調表示を使用して特定のファイルの古いバージョンを表示できます:
```bash
git show v0.6.0:src/main.rs | bat -l rs
```

差分内の構文強調表示は現在サポートされていないことに注意してください。 これを探しているなら、[`delta`](https://github.com/dandavison/delta) をチェックしてください。

#### `xclip`

`bat` の出力の行番号と Git 変更マーカーにより、ファイルの内容をコピーするのが難しくなる場合があります。
これを防ぐには、`-p` / `-plain` オプションを使用して `bat` を呼び出すか、
単に出力を `xclip` にパイプします:
```bash
bat main.cpp | xclip
```
`bat` は出力がリダイレクトされていることを検出し、プレーンファイルの内容を出力します。

#### `man`

`bat` は `MANPAGER` 環境変数を設定することにより、
`man` の色付けページャーとして使用できます:

```bash
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
man 2 select
```

フォーマットの問題が発生した場合は
`MANROFFOPT = "-c"` を設定する必要もあります。

これを新しいコマンドにバンドルしたい場合は [`batman`](https://github.com/eth-p/bat-extras/blob/master/doc/batman.md) も使用できます。

[Manpage syntax](assets/syntaxes/Manpage.sublime-syntax) はこのリポジトリで開発されており、まだ作業が必要であることに注意してください。

#### `prettier` / `shfmt` / `rustfmt`

[`prettybat`](https://github.com/eth-p/bat-extras/blob/master/doc/prettybat.md) スクリプトは、コードをフォーマットし、`bat` で印刷するラッパーです。


## インストール

[![Packaging status](https://repology.org/badge/vertical-allrepos/bat.svg)](https://repology.org/project/bat/versions)

###  On Ubuntu (`apt` を使用)
*... や他のDebianベースのLinuxディストリビューション*

Ubuntu Eoan 19.10 または Debian 不安定版 sid 以降の [the Ubuntu `bat` package](https://packages.ubuntu.com/eoan/bat) または [the Debian `bat` package](https://packages.debian.org/sid/bat) からインストールできます:

```bash
apt install bat
```

`apt` を使用して `bat` をインストールした場合、実行可能ファイルの名前が `bat` ではなく `batcat` になることがあります([他のパッケージとの名前衝突のため](https://github.com/sharkdp/bat/issues/982))。`bat -> batcat` のシンボリックリンクまたはエイリアスを設定することで、実行可能ファイル名が異なることによる問題の発生を防ぎ、他のディストリビューションと一貫性を保てます。

``` bash
mkdir -p ~/.local/bin
ln -s /usr/bin/batcat ~/.local/bin/bat
```

### On Ubuntu (最新の `.deb` パッケージを使用)
*... や他のDebianベースのLinuxディストリビューション

batの最新リリースを実行する場合、または Ubuntu/Debian の古いバージョンを使用している場合は、[release page](https://github.com/sharkdp/bat/releases) から最新の `.deb` パッケージをダウンロードし、
次の方法でインストールします:
```bash
sudo dpkg -i bat_0.16.0_amd64.deb  # adapt version number and architecture
```

### On Alpine Linux

適切なリポジトリが有効になっている場合は、
公式のソースから [`bat` package](https://pkgs.alpinelinux.org/packages?name=bat) をインストールできます:

```bash
apk add bat
```

###  On Arch Linux

[Arch Linuxの公式リソース](https://www.archlinux.org/packages/community/x86_64/bat/)
からインストールできます。

```bash
pacman -S bat
```

### On Fedora

公式の [Fedora Modular](https://docs.fedoraproject.org/en-US/modularity/using-modules/) リポジトリから [the `bat` package](https://koji.fedoraproject.org/koji/packageinfo?packageID=27506) をインストールできます。

```bash
dnf install bat
```

### On Gentoo Linux

公式ソースから
[the `bat` package](https://packages.gentoo.org/packages/sys-apps/bat) をインストールできます。

```bash
emerge sys-apps/bat
```

###  On Void Linux

xbps-install経由で `bat` をインストールできます。
```
xbps-install -S bat
```

### On FreeBSD

pkg を使用してプリコンパイルされた [`bat` package](https://www.freshports.org/textproc/bat) をインストールできます:

```bash
pkg install bat
```

または FreeBSD ポートから自分でビルドすることもできます:

```bash
cd /usr/ports/textproc/bat
make install
```

### Via nix

`bat` を [nix package manager](https://nixos.org/nix) 経由でインストールすることができます:

```bash
nix-env -i bat
```

### On openSUSE

`bat` をzypperでインストールすることができます:

```bash
zypper install bat
```

###  On macOS

[Homebrew](http://braumeister.org/formula/bat)で `bat` をインストールできます:

```bash
brew install bat
```

または [MacPorts](https://ports.macports.org/port/bat/summary) で `bat` をインストールします:

```bash
port install bat
```

###  On Windows

Windowsにbatをインストールするいくつかのオプションがあります。
batをインストールしたら [Windowsでのbatの使用](#windows-での-bat-の利用) セクションをご覧ください。

#### With Chocolatey

[Chocolatey](https://chocolatey.org/packages/Bat) から `bat` をインストールできます:
```bash
choco install bat
```

#### With Scoop

[scoop](https://scoop.sh/) から `bat` をインストールできます:
```bash
scoop install bat
```

[Visual C ++再頒布可能](https://support.microsoft.com/en-us/help/2977003/the-latest-supported-visual-c-downloads) パッケージをインストールする必要があります。

#### From prebuilt binaries:

[リリースページ](https://github.com/sharkdp/bat/releases) からビルド済みのバイナリをダウンロードできます。

[Visual C ++再頒布可能](https://support.microsoft.com/en-us/help/2977003/the-latest-supported-visual-c-downloads) パッケージをインストールする必要があります。

###  Via Docker

コンテナ内で `bat` を使いたい方のために [Docker image](https://hub.docker.com/r/danlynn/bat/) が用意されています:
```bash
docker pull danlynn/bat
alias bat='docker run -it --rm -e BAT_THEME -e BAT_STYLE -e BAT_TABS -v "$(pwd):/myapp" danlynn/bat'
```

###  Via Ansible

[Ansible](https://www.ansible.com/) でインストールすることができます:

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


多くの異なるアーキテクチャのためのプレビルドバージョンを[リリースページ](https://github.com/sharkdp/bat/releases)からチェックしてみてください。静的にリンクされている多くのバイナリも利用できます: ファイル名に `musl` を含むアーカイブを探してみてください。

### From source


`bat` をソースからビルドしたいならば、Rust 1.36 以上の環境が必要です。
`cargo` を使用してビルドすることができます:

```bash
cargo install --locked bat
```

一部のプラットフォームでは `llvm` および/または `libclang-dev` のインストールが必要になる場合があります。

## カスタマイズ

### ハイライト テーマ

`bat --list-themes` を使うと現在利用可能なシンタックスハイライトのテーマを入手できます。
`TwoDark` テーマを選ぶためには
`--theme=TwoDark` オプションをつけるか `BAT_THEME` という環境変数に
`TwoDark` を代入する必要があります。 シェルの起動ファイルに `export BAT_THEME="TwoDark"`
と定義すればその設定が変わることはないでしょう。あるいは、 `bat` の
[設定ファイル](#設定ファイル)を利用してください。

カスタムファイルでさまざまなテーマをプレビューする場合は、
次のコマンドを使用できます（これには [`fzf`](https://github.com/junegunn/fzf) が必要です）。
``` bash
bat --list-themes | fzf --preview="bat --theme={} --color=always /path/to/file"
```

`bat` はデフォルトだと黒い背景色のターミナルに適しています。
しかし、`GitHub` や `OneHalfLight` のような白い背景色のテーマでもいい感じにすることができます。
['新しいテーマの追加' セクションに従って](#新しいテーマの追加)
カスタムテーマを使用することもできます。

### 出力のスタイル

`--style` を使うことで `bat` の表示の見た目を変更することができます。
例えば、 `--style=numbers,changes` と入力します。
すると、Gitの差分と行番号だけが表示され、グリッド線とファイルヘッダーは表示されません。
環境変数に `BAT_STYLE` を定義するとこれらの設定を永続的に使用することができます。
[設定ファイル](#設定ファイル) を参考にしても良いでしょう。

### 新しい構文の追加 / 言語の定義

`bat` はシンタックスハイライトのための [`syntext`](https://github.com/trishume/syntect/)
という素晴らしいライブラリを使用しています。`syntect` は、
[Sublime Text の `.sublime-syntax` ファイル](https://www.sublimetext.com/docs/3/syntax.html)
とテーマを読み取ることができます。新しい構文を定義するために以下の手順を行います。

構文定義ファイルを入れておくためのフォルダを作ります:

```bash
mkdir -p "$(bat --config-dir)/syntaxes"
cd "$(bat --config-dir)/syntaxes"

# Put new '.sublime-syntax' language definition files
# in this folder (or its subdirectories), for example:
git clone https://github.com/tellnobody1/sublime-purescript-syntax
```

次のコマンドを使用して、これらのファイルをバイナリキャッシュに解析します:

```bash
bat cache --build
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
mkdir -p "$(bat --config-dir)/themes"
cd "$(bat --config-dir)/themes"

# Download a theme in '.tmTheme' format, for example:
git clone https://github.com/greggb/sublime-snazzy

# Update the binary cache
bat cache --build
```

最後に、 `bat --list-themes` で新しいテーマが利用可能かチェックします

### 異なるページャーの使用

`bat` は環境変数 `PAGER` に使用するページャーを明記します。
この環境変数が定義されていない場合、デフォルトで `less` が使用されます。
もし、異なるページャーを使用したい場合は、`PAGER` を修正してください。
または、`PAGER` を上書きする環境変数として `BAT_PAGER` を定義することも可能です。

もし、ページャーにコマンドライン引数を渡したい場合は、
`PAGER`/`BAT_PAGER` 環境変数を定義してください:

```bash
export BAT_PAGER="less -RF"
```

環境変数を利用する代わりに、 `bat` の [設定ファイル](#設定ファイル) を使用して設定も可能です（`--pager` オプション）

**注意**: デフォルトにより、ページャーが `less` にセットされているならば
`bat` はページャーの以下のコマンドラインオプション を受け付けるでしょう:
`-R`/`--RAW-CONTROL-CHARS`, `-F`/`--quit-if-one-screen` そして `-X`/`--no-init`。
最後のオプション（-X）は、530 より古いバージョンにのみ使用されます。

`-R` オプションは、ANSIカラーを正しく解釈するために必要です。
2番目のオプション（`-F`）は、出力サイズが端末の垂直サイズよりも小さい場合、すぐに終了するようにlessに指示します。
これは、ページャーを終了するために `q` を押す必要がないため、小さなファイルに便利です。
3番目のオプション（`-X`）は、`less` の古いバージョンの `--quit-if-one-screen` 機能のバグを修正するために必要です。 
残念ながら、`less` のマウスホイールのサポートも少なくなります。

`less` の古いバージョンでマウスホイールのスクロールを有効にしたい場合は、
`-R` だけを渡すことができます（上記の例のように、これは1画面終了機能を無効にします）。 
530以下の場合は、そのまま使用できます。

### Dark mode

macOSでダークモード機能を使用する場合、OSテーマに基づいて異なるテーマを使用するように `bat` を構成することができます。 
次のスニペットは、ライトモードの場合は `デフォルト` のテーマを使用し、
ダークモードの場合は `GitHub` テーマを使用します。

```bash
alias cat="bat --theme=\$(defaults read -globalDomain AppleInterfaceStyle &> /dev/null && echo default || echo GitHub)"
```

## 設定ファイル

`bat` は設定ファイルでカスタマイズすることが可能です。ファイルの場所はOSに依存します。
使用しているOSのデフォルトパスを調べるには以下のコマンドを実行してください:
```
bat --config-file
```

または、`BAT_CONFIG_PATH` 環境変数を使用して、`bat` が
構成ファイルのデフォルト以外の場所を指すようにすることができます:
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

# Use C++ syntax for .ino files
--map-syntax "*.ino:C++"

# Use ".gitignore"-style highlighting for ".ignore" files
--map-syntax ".ignore:Git Ignore"
```

## Windows での `bat` の利用

Windows 上で `bat` はほとんど動作しますが、いくつかの機能は設定を必要をする場合があります。

### ページング

Windowsには、`more` 形式の非常に限られたページャーしか含まれていません。
`less` 用のWindowsバイナリは、[ホームページ](http://www.greenwoodsoftware.com/less/download.html) または
[Chocolatey](https://chocolatey.org/packages/Less) からダウンロードできます。
これを使用するには、バイナリを `PATH` のディレクトリに配置するか、環境変数を定義します。[Chocolateyパッケージ](#on-windows) は `less` を自動的にインストールします。

### 色

Windows 10では、`conhost.exe` （コマンドプロンプト）と
[v1511](https://en.wikipedia.org/wiki/Windows_10_version_history#Version_1511_(November_Update)) 以降の PowerShell の両方、
およびbashの新しいバージョンの色がネイティブにサポートされています。 以前のバージョンのWindowsでは、
[ConEmu](https://conemu.github.io/) を含む [Cmder](http://cmder.net/) を使用できます。

**注意:** Git と MSYS の `less` はWindows上で色を正しく解釈しません。
もし、あなたが他のページャーをインストールしていないのであれば、 `--paging=never` オプションを付け加えるか
`BAT_PAGER` に空文字を設定することでページングを完全に無効にできます。

### Cygwin

Windows上の `bat` は Cygwin のunix風のpath(`/cygdrive/*`)をネイティブサポートしていません。絶対的なcygwinパスを引数として受けたときに、 `bat` は以下のエラーを返すでしょう: `The system cannot find the path specified. (os error 3)`

wrapperを作成するか、以下の関数を `.bash_profile` に追記することで、この問題を解決することができます:

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

## トラブルシューティング

### ターミナルと色

`bat` はターミナルがトゥルーカラーをサポートしている/していないに関係なくサポートします。
しかし、シンタックスハイライトのテーマの色が8-bitカラーに最適化されていない場合、
24-bitであるトゥルーカラーをサポートしているターミナルを使用することを強く推奨します（`terminator`, `konsole`, `iTerm2`, ...）。
この [記事](https://gist.github.com/XVilka/8346728) には
24-bitカラーがサポートされているターミナルの一覧が掲載されています。

本当の色をターミナルにセットするために、環境変数 `COLORTERM` に `truecolor` か
`24bit` のどちらかを代入してください。さもなければ、`bat` はどの色を使うのか決定することができません。または、24-bit エスケープシーケンスがサポートされません
（そして、8-bit colorに戻ります）。

### 行番号とグリッド線がほとんど見えない

異なるテーマを試してみてください（`bat --list-themes` でテーマを閲覧できます）。
`OneHalfDark` と `OneHalfLight` テーマはグリッド線と線の色を明るくします。

### ファイルエンコーディング

`bat` は UTF-16 と同様に UTF-8 をネイティブにサポートします。
他のすべてのファイルエンコーディングでは、エンコーディングは通常自動検出できないため、最初に UTF-8 に変換する必要があります。
これを行うには `iconv` を使用できます。
例: Latin-1（ISO-8859-1）エンコーディングの PHP ファイルがある場合、次のように呼び出すことができます:
``` bash
iconv -f ISO-8859-1 -t UTF-8 my-file.php | bat
```
注: `bat` が構文を自動検出できない場合は
`-l` / `-language` オプションを使用する必要がある場合があります。

## 開発

```bash
# Recursive clone to retrieve all submodules
git clone --recursive https://github.com/sharkdp/bat

# Build (debug version)
cd bat
cargo build --bins

# Run unit tests and integration tests
cargo test

# Install (release version)
cargo install --locked

# Build a bat binary with modified syntaxes and themes
bash assets/create.sh
cargo install --locked --force
```

## Maintainers

- [sharkdp](https://github.com/sharkdp)
- [eth-p](https://github.com/eth-p)

## プロジェクトの目標と既存の類似したOSS

`bat` は以下の目標を達成しようと試みています:

- 美しく高度なシンタックスハイライトの提供
- ファイルの差分を表示するためのGitとの連携
-  (POSIX) `cat` との完全互換
- ユーザーフレンドリーなコマンドラインインターフェースの提供

あなたが同様のプログラムを探しているなら、多くの選択肢があります。
比較については [このドキュメント](alternatives.md) を参照してください。

## ライセンス
Copyright (c) 2018-2020 [bat-developers](https://github.com/sharkdp/bat).

`bat` は MIT License 及び Apache License 2.0 の両方の条件の下で配布されています。

ライセンスの詳細については [LICENSE-APACHE](../LICENSE-APACHE) 及び [LICENSE-MIT](../LICENSE-MIT) ファイルを参照して下さい。
