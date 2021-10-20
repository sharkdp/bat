# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

# Unreleased

## Features

- `$BAT_CONFIG_DIR` is now a recognized environment variable. It has precedence over `$XDG_CONFIG_HOME`, see #1727 (@billrisher)
- Support for `x:+delta` syntax in line ranges (e.g. `20:+10`). See  #1810 (@bojan88)

## Bugfixes

- Python syntax highlighting no longer suffers from abysmal performance in specific scenarios. See #1688 (@keith-hall)
- First line not shown in diff context. See #1891 (@divagant-martian)

## Performance

- Load cached assets as fast as integrated assets, see #1753 (@Enselic)
- Greatly reduce startup time in loop-through mode, e.g. when redirecting output. Instead of *50 ms* - *100 ms*, startup takes *5 ms* - *10 ms*. See #1747 (@Enselic)
- Reduce startup time by approximately 80% for 91 out of 168 syntaxes when using `--language`. See #1787 (@Enselic)

## Other

- Add PowerShell completion, see #1826 (@rashil2000)
- Minimum supported Rust version (MSRV) bumped to 1.46

## Syntaxes

- Groff, see #1685 (@scop)
- HTTP Requests and Responses, see #1748 (@keith-hall)
- LLVM, see #1777 (@ioncodes)
- Highlight for `vimrc` and `gvimrc` files, see #1763 (@SuperSandro2000)
- Syslog highlighting improvements, see #1793 (@scop)
- Added support for `slim` syntax, see #1693 (@mfinelli)
- Racket, see #1884 (@jubnzv)

## New themes

## `bat` as a library

- Deprecate `HighlightingAssets::syntaxes()` and `HighlightingAssets::syntax_for_file_name()`. Use `HighlightingAssets::get_syntaxes()` and `HighlightingAssets::get_syntax_for_path()` instead. They return a `Result` which is needed for upcoming lazy-loading work to improve startup performance. They also return which `SyntaxSet` the returned `SyntaxReference` belongs to. See #1747, #1755, #1776, #1862 (@Enselic)
- Remove `HighlightingAssets::from_files` and `HighlightingAssets::save_to_cache`. Instead of calling the former and then the latter you now make a single call to `bat::assets::build`. See #1802 (@Enselic)
- Replace  the `error::Error(error::ErrorKind, _)` struct and enum with an `error::Error` enum. `Error(ErrorKind::UnknownSyntax, _)` becomes `Error::UnknownSyntax`, etc. Also remove the `error::ResultExt` trait. These changes stem from replacing `error-chain` with `thiserror`. See #1820 (@Enselic)



## Commit Statistics

<csr-read-only-do-not-edit/>

 - 80 commits contributed to the release over the course of 58 calendar days.
 - 17 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 15 unique issues were worked on: [#1787](https://github.com//sharkdp/bat/issues/1787), [#1802](https://github.com//sharkdp/bat/issues/1802), [#1815](https://github.com//sharkdp/bat/issues/1815), [#1820](https://github.com//sharkdp/bat/issues/1820), [#1822](https://github.com//sharkdp/bat/issues/1822), [#1825](https://github.com//sharkdp/bat/issues/1825), [#1826](https://github.com//sharkdp/bat/issues/1826), [#1829](https://github.com//sharkdp/bat/issues/1829), [#1831](https://github.com//sharkdp/bat/issues/1831), [#1850](https://github.com//sharkdp/bat/issues/1850), [#1852](https://github.com//sharkdp/bat/issues/1852), [#1883](https://github.com//sharkdp/bat/issues/1883), [#1887](https://github.com//sharkdp/bat/issues/1887), [#1890](https://github.com//sharkdp/bat/issues/1890), [#1905](https://github.com//sharkdp/bat/issues/1905)

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **[#1787](https://github.com//sharkdp/bat/issues/1787)**
    - Load independent and minimal syntax sets when using --language ([`9124271`](https://github.com//sharkdp/bat/commit/9124271eaf237519f9381b78681d71113e308a58))
 * **[#1802](https://github.com//sharkdp/bat/issues/1802)**
    - Don't take a HighlightingAssets detour to build assets ([`f1c0fd7`](https://github.com//sharkdp/bat/commit/f1c0fd7343c6c4c6a5ec198e533a6e8de3369472))
 * **[#1815](https://github.com//sharkdp/bat/issues/1815)**
    - doc/release-checklist.md: Recommend git push origin tag vX.Y.Z ([`12dfbdc`](https://github.com//sharkdp/bat/commit/12dfbdc400ac37d990d2443b9b9fc8f71bc7758d))
 * **[#1820](https://github.com//sharkdp/bat/issues/1820)**
    - Replace deprecated 'error-chain' with 'thiserror' ([`19c3e82`](https://github.com//sharkdp/bat/commit/19c3e82abf70324dcf9d1c310aed7881f44fd35f))
 * **[#1822](https://github.com//sharkdp/bat/issues/1822)**
    - src/build_assets.rs: Refactor into smaller functions ([`27fa55d`](https://github.com//sharkdp/bat/commit/27fa55d2745e9be0f00a98b75d88f129f22e2c22))
 * **[#1825](https://github.com//sharkdp/bat/issues/1825)**
    - Make asset compression optional at compile time ([`87978e7`](https://github.com//sharkdp/bat/commit/87978e7755d614bbfc6e6939df9ebbcd04925cac))
 * **[#1826](https://github.com//sharkdp/bat/issues/1826)**
    - Add PowerShell completion file ([`43afae3`](https://github.com//sharkdp/bat/commit/43afae34bef9a015d70a9977e159462ae0c4d492))
 * **[#1829](https://github.com//sharkdp/bat/issues/1829)**
    - Add regression testing for the custom assets functionality ([`d935ea1`](https://github.com//sharkdp/bat/commit/d935ea1cda8a2f59b8f832ed197b3c10552a11bd))
 * **[#1831](https://github.com//sharkdp/bat/issues/1831)**
    - Put documentation testing in its own job ([`8ca852c`](https://github.com//sharkdp/bat/commit/8ca852c728b18f0f278e064e4e90871afb530f98))
 * **[#1850](https://github.com//sharkdp/bat/issues/1850)**
    - Extract some private submodules from 'bat::assets' ([`e84b702`](https://github.com//sharkdp/bat/commit/e84b702309471e31621cf0bc06f8754511a5cbbc))
 * **[#1852](https://github.com//sharkdp/bat/issues/1852)**
    - Simplify HighlightingAssets::get_syntax() first_line logic ([`9ed9a6f`](https://github.com//sharkdp/bat/commit/9ed9a6fc3d3bc985a41d613e93848a50f103e8e3))
 * **[#1883](https://github.com//sharkdp/bat/issues/1883)**
    - Make the 'cargo fmt' check a toplevel job ([`043f338`](https://github.com//sharkdp/bat/commit/043f3381b094374c62dc15ae21c3e9d3bea21735))
 * **[#1887](https://github.com//sharkdp/bat/issues/1887)**
    - syntax-tests: Make CpuInfo test actually work ([`994c21a`](https://github.com//sharkdp/bat/commit/994c21a5e14588dd252f1503ae06683baf8f3ffa))
 * **[#1890](https://github.com//sharkdp/bat/issues/1890)**
    - Add missing style values in fish & zsh completions ([`6fc7ebf`](https://github.com//sharkdp/bat/commit/6fc7ebf37a63645d9d19e71ca252f67f7074a2e9))
 * **[#1905](https://github.com//sharkdp/bat/issues/1905)**
    - Sync README header across translations ([`aed4ea1`](https://github.com//sharkdp/bat/commit/aed4ea144f8d25d5c4648e38b4fdce1b06d1e0c6))
 * **Uncategorized**
    - Changelog enriched with `cargo changelog` (a binary of `cargo-smart-release`) ([`57647f0`](https://github.com//sharkdp/bat/commit/57647f08cbdc014675f63b2c0edfea6c89ff5ea8))
    - Make grep-cli optional dependency ([`ed3246c`](https://github.com//sharkdp/bat/commit/ed3246c423932561435d45c50fd8cd9e06add7f5))
    - update snapshot tests ([`2339d78`](https://github.com//sharkdp/bat/commit/2339d78bf4cb32a5aa54699bc97ffe6c369eec69))
    - changelog ([`3a3cd0a`](https://github.com//sharkdp/bat/commit/3a3cd0acba70cee80b70d910a539b8ddd9e706ae))
    - use saturating substraction to calculate Line ranges ([`ce4ddc0`](https://github.com//sharkdp/bat/commit/ce4ddc0911ab9201e34b5a8ae55fb6bec2537077))
    - Add install instructions on OpenBSD ([`eea061c`](https://github.com//sharkdp/bat/commit/eea061c1d9e21ee29eb0adb2c5e5d03d06006fef))
    - Add Русский link ([`10288e3`](https://github.com//sharkdp/bat/commit/10288e309e0a650156c5a2505c97e7e597025057))
    - add security vulnerabilities in ko doc ([`ebdb00d`](https://github.com//sharkdp/bat/commit/ebdb00d4fc24d0d66568926a34fbc4aac349f8d8))
    - update Korean readme ([`8f6a0cd`](https://github.com//sharkdp/bat/commit/8f6a0cd9e259a2e8d4dc86283530407e7fa8b0ba))
    - Deny unsafe code in lib and bin ([`5543746`](https://github.com//sharkdp/bat/commit/554374667e928e908393a4e6f21254af260b9eed))
    - Add Racket syntax ([`d04a83d`](https://github.com//sharkdp/bat/commit/d04a83de7b9ed1ed8ff23f505b08212fea6086a3))
    - add funtoo linux instructions ([`b622a4d`](https://github.com//sharkdp/bat/commit/b622a4d8907a7f8fa05d8eeca22219eab6ce7c4e))
    - List available Ubuntu packages more precisely. Ref: https://github.com/sharkdp/bat/pull/1865#issuecomment-931709001 ([`b551d28`](https://github.com//sharkdp/bat/commit/b551d28a2fbbb0467186e63d31274d0075538610))
    - Minor rephrasing ([`e6caa04`](https://github.com//sharkdp/bat/commit/e6caa04209be9431a982fbaac178c57e73d1ad92))
    - Update the doc for Ubuntu and Debian ([`adadede`](https://github.com//sharkdp/bat/commit/adadedeab16108e718805ed4f31d92efcffa5b0c))
    - Bump assert_cmd from 2.0.0 to 2.0.1 ([`a6cf523`](https://github.com//sharkdp/bat/commit/a6cf5235aaef07cb8f66e87b411f85363c5de87b))
    - Bump nix from 0.22.1 to 0.23.0 ([`1477338`](https://github.com//sharkdp/bat/commit/1477338106d0620384d08d1d1dfb14f42e01343b))
    - Bump MSRV to 1.46 ([`418fce5`](https://github.com//sharkdp/bat/commit/418fce56835240ca9989a841d667eb02d5992c5d))
    - Bump assets/syntaxes/02_Extra/Julia from `48639e1` to `1e55f32` ([`7a15ba3`](https://github.com//sharkdp/bat/commit/7a15ba3796038eb44518458cd305a2b2f88e5d61))
    - Bump serde_yaml from 0.8.20 to 0.8.21 ([`0f002a5`](https://github.com//sharkdp/bat/commit/0f002a5b06947fd0362aec34be2fabe122eec0cf))
    - Bump unicode-width from 0.1.8 to 0.1.9 ([`5344a32`](https://github.com//sharkdp/bat/commit/5344a32d3447286a4b809789d015930c150bf73d))
    - Bump flate2 from 1.0.20 to 1.0.22 ([`c964569`](https://github.com//sharkdp/bat/commit/c9645693a4549f26c26a91dbf3abdf37dc6bc805))
    - Bump thiserror from 1.0.28 to 1.0.29 ([`f607263`](https://github.com//sharkdp/bat/commit/f607263bdccce628d8999562c912a3e499c6b0ff))
    - Bump serde from 1.0.127 to 1.0.130 ([`f309d2f`](https://github.com//sharkdp/bat/commit/f309d2fbd2af55b0a4d232bb13e9d315973390c6))
    - Add bash completion to deb package ([`47283f2`](https://github.com//sharkdp/bat/commit/47283f226a828b0573f6a771d05139861f1c3e36))
    - src/printer.rs: Simplify Plain Text fallback code ([`aefc8fd`](https://github.com//sharkdp/bat/commit/aefc8fd8243b8270849aeecee7dc443210cd9b3b))
    - Turn get_syntax_for_path() into public API ([`405a80f`](https://github.com//sharkdp/bat/commit/405a80f3eeab25870249943ceb4f209e6599eca1))
    - Implement get_syntax_for_file_name() with get_syntax_for_path() ([`ad98d35`](https://github.com//sharkdp/bat/commit/ad98d35a48cb45948342a3e626dcefb91d9c47e4))
    - Extract get_syntax_for_path() method ([`b69ab21`](https://github.com//sharkdp/bat/commit/b69ab219d7ab1296348c54cce5e494d00d9ae8e6))
    - src/assets.rs: Extract helper method OpenedInput::path() ([`dc8225f`](https://github.com//sharkdp/bat/commit/dc8225f6828e30a7acad09b0a9f9b3f1c30fc533))
    - build_assets.rs: Enable dump of syntax dependencies to Graphviz dot file ([`9d9b266`](https://github.com//sharkdp/bat/commit/9d9b266f543c770d5f45098cab2a72c882ca4f48))
    - build_assets.rs: Ignore explicit contexts when tracking dependencies ([`b9d01c1`](https://github.com//sharkdp/bat/commit/b9d01c1a6189951738ef40b06f529148df809408))
    - build_assets.rs: Sort first to make dependencies.dedup() actually useful ([`122cae7`](https://github.com//sharkdp/bat/commit/122cae7902db9cafa138d115b7546f72cdf6b3ae))
    - Parallelize syntax regression tests ([`44a332c`](https://github.com//sharkdp/bat/commit/44a332c1c4e904eada9fc53f2e6dfede43c83d7f))
    - build_assets.rs: Add code to track dependents of each syntax ([`5143f3a`](https://github.com//sharkdp/bat/commit/5143f3ad43c54151ef421b8cb9cf6eeacbf11493))
    - build_assets.rs: Make OtherSyntaxLookup come before SyntaxToDependencies ([`a6dc25a`](https://github.com//sharkdp/bat/commit/a6dc25a2169432245edf2d62472a34e2193a55ae))
    - build_assets.rs: Rename 'Dependency' to 'OtherSyntax' ([`f04d2a9`](https://github.com//sharkdp/bat/commit/f04d2a9d6a45739861168074d6f3dd84544c318c))
    - src/printer.rs: Add HighlighterFromSet helper ([`eb3b3b9`](https://github.com//sharkdp/bat/commit/eb3b3b9f8da9b340c540f361ead971cebaf1167a))
    - Move common get_extension_syntax() code into find_syntax_by_extension() ([`0994f37`](https://github.com//sharkdp/bat/commit/0994f3783f13464d0d2d78ddefa4f454cd623489))
    - Inline find_syntax_by_file_name() and find_syntax_by_file_name_extension() ([`974dec3`](https://github.com//sharkdp/bat/commit/974dec38e324373bd8580c13f995932c79f96c90))
    - src/assets.rs: Use /// not // for COMPRESS_* consts ([`a0c3636`](https://github.com//sharkdp/bat/commit/a0c363647f35f3e365fcd812e312fcba90e144c1))
    - Inline absolute_path ([`d989224`](https://github.com//sharkdp/bat/commit/d989224a8a5bd6a4a1bbc54ff7b63a813b27979c))
    - Simplify absolute_path with .map_or_else() ([`82f439e`](https://github.com//sharkdp/bat/commit/82f439e71561a423f23e1d09bde98e72569c68b6))
    - No need for both path and path_str ([`b034879`](https://github.com//sharkdp/bat/commit/b034879eaec93e5b658055f56a94b53614fce029))
    - Add find_syntax_by_extension() helper ([`6226eba`](https://github.com//sharkdp/bat/commit/6226eba52a05f53026abb927500629074279ef51))
    - Add find_syntax_by_name() helper ([`9e0ea06`](https://github.com//sharkdp/bat/commit/9e0ea06435f6d5f0e2faacea121626019faa9b32))
    - Add various other code refactorings ([`863d9ca`](https://github.com//sharkdp/bat/commit/863d9cacd0ba0284f7b138608be04a0a137fbe21))
    - Use deref coercion to simplify some argument passing ([`4baa346`](https://github.com//sharkdp/bat/commit/4baa346aaea6bc9f1eff7f903385db3041dd64b6))
    - Improve iterator usage ([`7956485`](https://github.com//sharkdp/bat/commit/7956485e37c8fc48506df1484f34811104eb882b))
    - Reduce nesting in if blocks by short circuiting ([`372e42f`](https://github.com//sharkdp/bat/commit/372e42f3509c3ec87504a054271ad1289ce3b4ac))
    - Add context to .ino configuration ([`156dec2`](https://github.com//sharkdp/bat/commit/156dec2737db4759f41a30650cfe938de3c65d5c))
    - Consolidate environment variable lists ([`27f046e`](https://github.com//sharkdp/bat/commit/27f046ec036b6b3e0688048969db25ae4b0bfdd4))
    - Bump serde_yaml from 0.8.17 to 0.8.20 ([`74ae3de`](https://github.com//sharkdp/bat/commit/74ae3dee91d3aa3439c64951d36f70c871d40a17))
    - Bump assert_cmd from 1.0.8 to 2.0.0 ([`9602195`](https://github.com//sharkdp/bat/commit/960219591028984053a80fa6aeb3e506e9d791e7))
    - Bump predicates from 2.0.1 to 2.0.2 ([`df067f7`](https://github.com//sharkdp/bat/commit/df067f7d1fa4b4b4e5afe941f62c2214215c427b))
    - Bump nix from 0.22.0 to 0.22.1 ([`a8a81e9`](https://github.com//sharkdp/bat/commit/a8a81e99d2fc064382025564821997b36cff700c))
    - Bump thiserror from 1.0.26 to 1.0.28 ([`65e7c53`](https://github.com//sharkdp/bat/commit/65e7c531de5e855fc3cd5a42e3d9e904e3423407))
    - add Debian ucf backups to ignored suffixes ([`7c41bd7`](https://github.com//sharkdp/bat/commit/7c41bd72dac7960f40b545002ddc1b0b0aae13d8))
    - Sort build matrix by target ([`355a82d`](https://github.com//sharkdp/bat/commit/355a82db542dd66539fc0497aad854cdfa668cbe))
    - Remove disabled windows-2019 i686-pc-windows-gnu from build matrix ([`b3e17bd`](https://github.com//sharkdp/bat/commit/b3e17bde8275ab2fdf70f055029bdbdf55a803f4))
    - Swap target and os in matrix job name ([`4b38e7b`](https://github.com//sharkdp/bat/commit/4b38e7b1d72c0eb7f836074e070aae65c1577ff3))
    - PR comments addressed for line range +delta syntax ([`c86a179`](https://github.com//sharkdp/bat/commit/c86a1794121ab3460d496a55556abc17b03b71a2))
    - Support for line range plus syntax ([`0748783`](https://github.com//sharkdp/bat/commit/07487834044a1e24247d4c1631447419fdd463d2))
    - Merge the v0.18.3 hotfix release into master ([`b3247d9`](https://github.com//sharkdp/bat/commit/b3247d93648265d67b1f82291e2c226ae3fbc135))
    - Add LANG and LC_ALL to --diagnostics output ([`ba8a694`](https://github.com//sharkdp/bat/commit/ba8a694314b95e7792e042bf687d77675ba6ff84))
</details>

# v0.18.3 (2021-08-22)

## Bugfixes

- Bump `git2` dependency to fix build with Rust 1.54, see #1761


## Bug Fixes

 - <csr-id-5197ef90486c93a2121fe9b9e33c6055bc1c524d/> spelling

## New Features

 - <csr-id-bf78288e9ec28cfbec7610de19c20bbe04f33f98/> added recognition of $BAT_CONFIG_DIR

## Commit Statistics

<csr-read-only-do-not-edit/>

 - 78 commits contributed to the release over the course of 40 calendar days.
 - 11 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 1 unique issue was worked on: [#1776](https://github.com//sharkdp/bat/issues/1776)

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **[#1776](https://github.com//sharkdp/bat/issues/1776)**
    - When returning a SyntaxReference, also return the SyntaxSet that contains it ([`d8b813c`](https://github.com//sharkdp/bat/commit/d8b813c0bf1c88a11f1f49ffbd4b7328a02807a6))
 * **Uncategorized**
    - Bump version to v0.18.3 ([`b146958`](https://github.com//sharkdp/bat/commit/b146958ecbb8c8c926159509ca7fb32a8573f897))
    - Add statically linked binaries for ARM ([`ff70a80`](https://github.com//sharkdp/bat/commit/ff70a80741d77fc229434d83b4b1aa4348d541ec))
    - Use Ubuntu 20.04 instead of 18.04 ([`ecdb171`](https://github.com//sharkdp/bat/commit/ecdb17148df60743460ecdb258d86c049f0c3667))
    - Remove post-release section ([`11bd523`](https://github.com//sharkdp/bat/commit/11bd523f7e7715dc1954687ad57ddab15d19d211))
    - Formatting ([`01fbedc`](https://github.com//sharkdp/bat/commit/01fbedc246e6df15907357e4de5cb94d0914704a))
    - Add refined version of release checklist ([`05e4e1f`](https://github.com//sharkdp/bat/commit/05e4e1f2f2c62bcdf6f9a483cb18d61eb5f5044b))
    - Add old release checklist ([`20223ad`](https://github.com//sharkdp/bat/commit/20223ad77c4037cedf1da85efcd2b04d87469bcc))
    - trim excess whitespace ([`51edacb`](https://github.com//sharkdp/bat/commit/51edacb5eb2c166f65d0950e36a00e09db5348ef))
    - spelling ([`5197ef9`](https://github.com//sharkdp/bat/commit/5197ef90486c93a2121fe9b9e33c6055bc1c524d))
    - chore(find-slow-to-highlight-files.py): be explicit about using python3 ([`1967852`](https://github.com//sharkdp/bat/commit/19678527e5ef0e9982eb319acfebabf95de62780))
    - style(create.sh): remove non-POSIX keyword ([`5d319de`](https://github.com//sharkdp/bat/commit/5d319dee94a830be5d4cba486e0b5959d5b24c07))
    - Bump bugreport to 0.4.1 ([`15c701b`](https://github.com//sharkdp/bat/commit/15c701b19625bf8f8646238eecb28c2f8f43afeb))
    - Update git2 dependency to fix incompatibility with Rust 1.54 ([`d833980`](https://github.com//sharkdp/bat/commit/d8339808a1e0d25717f5239f7896818cc9c68f80))
    - Fix typo in README ([`3b263f0`](https://github.com//sharkdp/bat/commit/3b263f09170e396121955d0ddd49f8f4a32560de))
    - Make Package Control reference a link ([`43e1a11`](https://github.com//sharkdp/bat/commit/43e1a11ad86af47d1a5fb304e973541dab91e16d))
    - Fix all lints that are new with Rust 1.54 ([`ed09f90`](https://github.com//sharkdp/bat/commit/ed09f90e5e3b0b2bed93a591157b8b3a73e07ef8))
    - Add 'cargo fmt' check ([`cbd9623`](https://github.com//sharkdp/bat/commit/cbd96237fde1f1ef94d60c0bc6e6e5e754ac1879))
    - Run 'cargo fmt' ([`f5c1cb2`](https://github.com//sharkdp/bat/commit/f5c1cb2dff1f82a55d9212e6878867372e87eda4))
    - Merge pull request #1800 from sharkdp/syslog_no_colon_after_process ([`5eb93a6`](https://github.com//sharkdp/bat/commit/5eb93a6eae6469a57a8b3ac8db4538321ee6c7f0))
    - Make 'build-assets' an optional capability for application ([`25fa577`](https://github.com//sharkdp/bat/commit/25fa577cd06c9753756d116410aaa19735c95085))
    - src/assets.rs: Use ThemeSet::new() instead of BTreeMap::new() ([`deddc81`](https://github.com//sharkdp/bat/commit/deddc81426b155531cade6e755679c88f5bb8e8f))
    - Fix syslog syntax highlighting when no colon after "process" ([`133b06e`](https://github.com//sharkdp/bat/commit/133b06e94560ea0d94d201259543b4ad79823b0a))
    - Add slim syntax test ([`699f1e6`](https://github.com//sharkdp/bat/commit/699f1e65cc0bfad3af620c8b9ccffe6c2bd6f217))
    - Convert tmLanguage into sublime-syntax ([`9ef87da`](https://github.com//sharkdp/bat/commit/9ef87dab272dcacd0e2687d163c8c065022d930d))
    - Add support for ruby-slim syntax ([`5125e9c`](https://github.com//sharkdp/bat/commit/5125e9c941e0f37b8611d9138adec7d5b9464680))
    - revamped integration test, made CHANGELOG changes ([`6c62ed5`](https://github.com//sharkdp/bat/commit/6c62ed5608b842e3052805ea1f13856b071f11b7))
    - added recognition of $BAT_CONFIG_DIR ([`bf78288`](https://github.com//sharkdp/bat/commit/bf78288e9ec28cfbec7610de19c20bbe04f33f98))
    - Merge pull request #1793 from scop/syslog-improvements ([`f8498b2`](https://github.com//sharkdp/bat/commit/f8498b260b0dc82e12e0486dcb2d86d039cac54e))
    - Mention syslog highlight improvements in change log ([`79f0858`](https://github.com//sharkdp/bat/commit/79f08588c62c4d772a6c36760ed983824991923e))
    - Allow colon in syslog loghost ([`2d92a4d`](https://github.com//sharkdp/bat/commit/2d92a4dbb3fdf944ddcbc09730a29248d0b45524))
    - Allow period in syslog loghost ([`f508ddf`](https://github.com//sharkdp/bat/commit/f508ddf66d7d9e08616d8541ca7a090dee850c97))
    - Allow period in syslog process name ([`02218c9`](https://github.com//sharkdp/bat/commit/02218c916c7cdc754a463480eba5b42377c807b9))
    - Make --no-paging and --no-pager work again ([`89217e0`](https://github.com//sharkdp/bat/commit/89217e0d58a9a371540d11eded538721d71c90c7))
    - Cargo.toml: Introduce 'quick-build-application' feature ([`cb49739`](https://github.com//sharkdp/bat/commit/cb4973987b238bfc7cb63e47950a13e815706592))
    - Allow to build without bugreport ([`905902d`](https://github.com//sharkdp/bat/commit/905902d81160a4abce06840293c5ec1d90ca1a96))
    - Cargo.toml: Only build bugreport with the app ([`c83e382`](https://github.com//sharkdp/bat/commit/c83e382eac32edfba49fefefc0eacd4d41445dff))
    - Bump bugreport to 0.4.1 ([`f6975e2`](https://github.com//sharkdp/bat/commit/f6975e2acdb10a7c60a7668fbb8b3b43af1c0309))
    - Fix typo in unreachable!(..) message for --wrap ([`5236ed1`](https://github.com//sharkdp/bat/commit/5236ed135e56bff956162bedf442c480521e6b54))
    - Add code for analyzing dependencies between syntaxes ([`47d955a`](https://github.com//sharkdp/bat/commit/47d955a2ab1ae93141abea1bb6348c39856f87c7))
    - integration_tests: Add diagnostic_sanity_check() ([`bd797c7`](https://github.com//sharkdp/bat/commit/bd797c75a4016f7866f2971cc7ebd79fbf7107a4))
    - add patch for Python syntax to help improve performance ([`05c1196`](https://github.com//sharkdp/bat/commit/05c11964fc13b039f3797a8161aac642e3be30c7))
    - Make --style docs reflect that 'full' is default ([`8ecd23e`](https://github.com//sharkdp/bat/commit/8ecd23eab4f30dc02e3954c6ff05fa18a5b81e34))
    - CHANGELOG.md: Highlight for `vimrc` and `gvimrc` files ([`1ef0206`](https://github.com//sharkdp/bat/commit/1ef0206f248bb68c13816d398c46412ef3c5fb04))
    - Bump assets/syntaxes/02_Extra/VimL from `7ebcaa1` to `c91fe3a` ([`6694aa3`](https://github.com//sharkdp/bat/commit/6694aa369ea9cf2c674e8a737788906ce97bfa99))
    - cargo fmt ([`0331d28`](https://github.com//sharkdp/bat/commit/0331d28ee4c7f957cc7072c3dbd18dd1186893ef))
    - Included LLVM syntax highlighting submodule and added regression tests ([`51c7eb7`](https://github.com//sharkdp/bat/commit/51c7eb7ac14568ee4752361e0cea2a6e568b4799))
    - Merge pull request #1779 from sharkdp/http-request-response-syntax-update ([`5516bcb`](https://github.com//sharkdp/bat/commit/5516bcb8391093bd312b5cb486f3c3f09cb1aac0))
    - Bump assets/syntaxes/02_Extra/http-request-response from f58bffe to 93b9326 ([`056b966`](https://github.com//sharkdp/bat/commit/056b96650156b2473a1470d732aa12abdda38021))
    - Merge pull request #1771 from sharkdp/warn_when_missing_contexts ([`3b020fd`](https://github.com//sharkdp/bat/commit/3b020fd95a1e111aae66683764c0b66204d287a0))
    - Use assert!(..) instead of assert_eq!(true, ..) ([`28eca6a`](https://github.com//sharkdp/bat/commit/28eca6a2be4fdce2a8863fbbdbfe0a6db1480c3b))
    - Bump assert_cmd from 1.0.5 to 1.0.8 ([`b7fd552`](https://github.com//sharkdp/bat/commit/b7fd55242ee3aeca3f428af8a1ebb4ea1ab13d3e))
    - Bump nix from 0.21.0 to 0.22.0 ([`8161955`](https://github.com//sharkdp/bat/commit/8161955cc71fab12fe8f0944a69b82a259d2c18b))
    - Pass --locked to all cargo commands ([`697d106`](https://github.com//sharkdp/bat/commit/697d106bd47069c75c18774dcac9f601877c0e2c))
    - Warn when building assets from files if some referenced contexts are missing ([`50e1c60`](https://github.com//sharkdp/bat/commit/50e1c6074f88d246c8fbc79755ed50d23f3043a0))
    - assets::tests: Add get_syntax_name() helper ([`a610987`](https://github.com//sharkdp/bat/commit/a610987ef7b5b464c70e78048b2da362f916178e))
    - Bump predicates from 1.0.8 to 2.0.1 ([`a7fd9f4`](https://github.com//sharkdp/bat/commit/a7fd9f4b1b425630b055a788eb065e23fc978317))
    - Bump semver from 0.11.0 to 1.0.4 ([`5f5b77c`](https://github.com//sharkdp/bat/commit/5f5b77cddadc6f96a7dd3b3893385e3a3fe3b031))
    - Bump serde from 1.0.126 to 1.0.127 ([`83808a6`](https://github.com//sharkdp/bat/commit/83808a63becf7685413e37a0755f3a07d7992eef))
    - Add HTTP Request/Response syntax as a git submodule ([`6d5ff67`](https://github.com//sharkdp/bat/commit/6d5ff671e7ae97c664a406217431cfac22e8d7a5))
    - Encapsulate theme_set behind a getter ([`ffdf349`](https://github.com//sharkdp/bat/commit/ffdf349a9660d42263cbbb31ce3e934783d50b17))
    - Update git2 dependency to fix incompatibility with Rust 1.54 ([`f3d53b7`](https://github.com//sharkdp/bat/commit/f3d53b79a2d7a51f470ac8a06b6bdd9a4f225e8f))
    - Reduce startup time in loop-through mode with 80%-90% ([`6acec2c`](https://github.com//sharkdp/bat/commit/6acec2c074e0a341edb08f1014d97a84096a68c0))
    - Move out fn get_integrated_*set() to module scope ([`1bac375`](https://github.com//sharkdp/bat/commit/1bac3750dfe5dbd4d54894717478e67d0dba4d83))
    - Support a hidden arg --no-custom-assets that skips loading assets from the cache ([`b040eff`](https://github.com//sharkdp/bat/commit/b040efff79b641ba23704e7f34655d11fb91d6e9))
    - Make .syntaxes() and syntax_for_file_name() failable ([`a810096`](https://github.com//sharkdp/bat/commit/a81009607a304f5eadc96d5d1abf02fe9e3060a7))
    - HighlightingAssets::get_extension_syntax(): Split up into smaller methods ([`c0e0966`](https://github.com//sharkdp/bat/commit/c0e09662b45cd7194f09c3d035db825e5c172097))
    - Make loading of cached assets closer in performance to integrated assets ([`ccf4563`](https://github.com//sharkdp/bat/commit/ccf456357366f488d902232faf495d09b246a130))
    - Add Enselic in FUNDING.yml ([`fb1ab09`](https://github.com//sharkdp/bat/commit/fb1ab09e3eea76fa75f188b34e9217d69ca46085))
    - Update battest.py ([`f464b1b`](https://github.com//sharkdp/bat/commit/f464b1ba398b05301db39f7b91157da0afe64deb))
    - Add `rs` identifier for Rust code blocks in Markdown ([`2ea6348`](https://github.com//sharkdp/bat/commit/2ea6348b85f96c2b77a06e45ad258f9215fccde2))
    - Update CHANGELOG ([`6e536ab`](https://github.com//sharkdp/bat/commit/6e536ab06d914b57787a0f01d4803eb2ee8ef1c4))
    - Add groff syntax ([`7537e30`](https://github.com//sharkdp/bat/commit/7537e309d84ec5a2bf50a4a08a742174c096eabf))
    - Add custom FUNDING.yml ([`84e2a2e`](https://github.com//sharkdp/bat/commit/84e2a2e5d1dd89916c08cb6c8870c35d9cb93101))
    - Introduce private fn new() helper ([`f6fc826`](https://github.com//sharkdp/bat/commit/f6fc826dc64b5d06025b131bd969ed386fe57006))
    - Encapsulate syntax_set behind a getter ([`375d55a`](https://github.com//sharkdp/bat/commit/375d55aa5d7f3390e33febcc40a8d629b22926ae))
    - De-duplicate some themes.bin and syntaxes.bin related code ([`6ef2bb3`](https://github.com//sharkdp/bat/commit/6ef2bb3283e1ba5f41316ed819bebe63f758054f))
    - Fix typo in README ([`fc0794a`](https://github.com//sharkdp/bat/commit/fc0794a83dc9afc36cf5ee9402cdd0c3dd93d6dd))
</details>

# v0.18.2 (2021-07-13)

## Features

- Ignore known backup/template filename suffixes when selecting the syntax, see #1687 (@scop)

## Bugfixes

- Fix for a security vulnerability on Windows. Prior to this release, `bat` would execute programs called `less`/`less.exe` from the current working directory (instead of the one from `PATH`) with priority. An attacker might be able to use this by placing a malicious program in a shared directory where the user would execute `bat`. `bat` users on Windows are advised to upgrade to this version. See #1724 and #1472 (@Ry0taK).

## Other

- Add bash completion, see #1678 (@scop)
- Fix Clippy lints, see #1661 (@mohamed-abdelnour)
- Add syntax highlighting test files, see #1213 and #1668 (@mohamed-abdelnour)

## Syntaxes

- Upgraded Julia syntax to fix a highlighting bug, see #1692
- Added support for `dash` syntax, see #1654 (@mohamed-abdelnour)
- Added support for `XAML` syntax, see #1590 and #1655 (@mohamed-abdelnour)
- Apply `DotENV` syntax also for `.env.default` and `.env.defaults` files, see #1669


## Commit Statistics

<csr-read-only-do-not-edit/>

 - 81 commits contributed to the release over the course of 59 calendar days.
 - 3 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' where seen in commit messages

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Add note on vulnerability reporting, closes #1473 ([`589df67`](https://github.com//sharkdp/bat/commit/589df6792fe406f91b00db5e24d35393c6527e6b))
    - Upgrade CHANGELOG with security vulnerability notice ([`945bba7`](https://github.com//sharkdp/bat/commit/945bba777bb65c6d9820e49ffec9c2b28ca01a62))
    - Update Julia syntax test ([`64763ea`](https://github.com//sharkdp/bat/commit/64763eafbeac7a45208e3d22791057e7cbe2dac1))
    - Update Julia syntax, closes #1692 ([`3da4651`](https://github.com//sharkdp/bat/commit/3da46515692c3c3ebfd979c107e83393df85478b))
    - Update assets ([`311ed2f`](https://github.com//sharkdp/bat/commit/311ed2fecac73a6546a05334230894bc25a359f3))
    - Run cargo fmt ([`33c11d6`](https://github.com//sharkdp/bat/commit/33c11d64f016cd6890906a169d64baf7c8bf6040))
    - Bump version to v0.18.2 ([`a4b6749`](https://github.com//sharkdp/bat/commit/a4b674902f26a606531ec3301fc7b240ad750204))
    - Use resolved path for --diagnostic as well ([`3fa09db`](https://github.com//sharkdp/bat/commit/3fa09dbe2e1755740eda1de0900853c4a6ee5f3a))
    - Fix for Windows: do not run binaries from CWD ([`bf2b2df`](https://github.com//sharkdp/bat/commit/bf2b2df9c9e218e35e5a38ce3d03cffb7c363956))
    - Bump assets/syntaxes/02_Extra/CMake from `7d6231c` to `ab6ef4e` ([`3617c98`](https://github.com//sharkdp/bat/commit/3617c98cf5de9996c29fe73bc7fc4126768497f3))
    - Bump ansi_colours from 1.0.2 to 1.0.4 ([`774d36c`](https://github.com//sharkdp/bat/commit/774d36c9894a0c283e2cd2bd57c5bb4fb7638844))
    - Bump globset from 0.4.6 to 0.4.8 ([`6abd618`](https://github.com//sharkdp/bat/commit/6abd61865fea61f46d35ddbbe6f83124dde8a0e3))
    - Add CHANGELOG entry for #1687 ([`3c59b98`](https://github.com//sharkdp/bat/commit/3c59b98dc8503b0ef2876ae30e2997ffa0d59269))
    - Abort ignored filename suffix stripping early on unworkable filenames ([`21338ed`](https://github.com//sharkdp/bat/commit/21338ed7897a3c7dd3319861d2c7ed35c4fbeb58))
    - Simplify ignored filename suffix stripping ([`dc8ab0b`](https://github.com//sharkdp/bat/commit/dc8ab0b5ce6d9cf0f09a3c86c43312a8ebf2fbfc))
    - Add syntax tests for ignored filename suffixes ([`ddb39ef`](https://github.com//sharkdp/bat/commit/ddb39ef2f603ff00301ae3b6241bacfa9d594e15))
    - Find syntax ignoring known backup/template filename suffixes ([`355e62e`](https://github.com//sharkdp/bat/commit/355e62efe9321b4e64520230fe792dc0f80a6041))
    - sync with pastel, minor changes ([`fddd11a`](https://github.com//sharkdp/bat/commit/fddd11a20548883707b31dc20ec727583d49a5e5))
    - Add some more options to bash completion ([`8b37e62`](https://github.com//sharkdp/bat/commit/8b37e62cf3fe0e8fb04a30aba1cdfa8de61a0e6b))
    - Bump assert_cmd from 1.0.4 to 1.0.5 ([`4296d47`](https://github.com//sharkdp/bat/commit/4296d47dcbb565c2bf7785e7d24a576d945e102a))
    - Fix missing config/cache arguments in Zsh completion ([`42f1ef0`](https://github.com//sharkdp/bat/commit/42f1ef019a83dc042e7235283e0bde208f821055))
    - Document --generate-config-file in manual ([`fe8e526`](https://github.com//sharkdp/bat/commit/fe8e526292758a26112a6c3d4d2b186fe689bdc4))
    - Add change log entry for bash completion ([`0e9d612`](https://github.com//sharkdp/bat/commit/0e9d6121739ccfb67c2df796de1760f485ad768c))
    - Depend on scop/bash-completion ([`06c601b`](https://github.com//sharkdp/bat/commit/06c601bc7cdf5d36ca77347a73abb4afac7bd9c4))
    - Add missing comment about --theme arg escaping ([`229fbc1`](https://github.com//sharkdp/bat/commit/229fbc1a18898ed96143dac58857e1df09c3d942))
    - Add bash completion ([`e05f501`](https://github.com//sharkdp/bat/commit/e05f5010dafda6923561d83d3c9129a82b75447b))
    - pretty_printer.rs: Don't use a URL as an example Input title ([`073b996`](https://github.com//sharkdp/bat/commit/073b9968c078788a2c95f87c1c888ea824dbbc6e))
    - Update README.md ([`d3aa17a`](https://github.com//sharkdp/bat/commit/d3aa17ae7704060bb3dbcf2765799e2f41d24ef6))
    - Update README.md ([`4187eed`](https://github.com//sharkdp/bat/commit/4187eed12be703c37f3ecd4eb5dd448795c7da6a))
    - Modify Linguist overrides ([`12ecb32`](https://github.com//sharkdp/bat/commit/12ecb325c911d2f059962ca71f5d90b87f92dc85))
    - Spelling fixes ([`90e48e9`](https://github.com//sharkdp/bat/commit/90e48e9b61f68333c4d846cf273beb7e42555656))
    - Highlight mtab files with fstab highlighting ([`35f3127`](https://github.com//sharkdp/bat/commit/35f31270f3b879469d7214a4cc9d7fe1e0cf3f8b))
    - README.md: List compatibility as another advantage to 3-bit themes ([`2a71852`](https://github.com//sharkdp/bat/commit/2a7185207010ed2d0b43c179b0d5d56d558b2631))
    - README.md: Mention that PAGER is ignored if set to more or most ([`c8dd328`](https://github.com//sharkdp/bat/commit/c8dd32802de64ecb889bee34fe3e2211c092aac3))
    - Bump nix from 0.20.0 to 0.21.0 ([`71f04dc`](https://github.com//sharkdp/bat/commit/71f04dc0e835b097f606e1b12c56e78ac1efe516))
    - Bump assert_cmd from 1.0.3 to 1.0.4 ([`49f8bbf`](https://github.com//sharkdp/bat/commit/49f8bbfee4f78f8b449a515b8f6cb72dbdaa9823))
    - Merge pull request #1669 from sharkdp/dependabot/submodules/assets/syntaxes/02_Extra/DotENV-58201ba ([`f4217eb`](https://github.com//sharkdp/bat/commit/f4217eba73da087945bd04234cdcaf15124ba6d0))
    - Update `CHANGELOG.md` for #1668 ([`14900f6`](https://github.com//sharkdp/bat/commit/14900f6ed8b852b60ab45b28957aa0bd74ab134d))
    - Add Verilog syntax test file ([`c18afcb`](https://github.com//sharkdp/bat/commit/c18afcb01a2bd6561381d4bd301a3cb1fcd5d5f8))
    - Add varlink syntax test file ([`63043d4`](https://github.com//sharkdp/bat/commit/63043d4a60d21c6e0c4a2520827a2ba07077b3a1))
    - Add TypeScriptReact syntax test file ([`7603488`](https://github.com//sharkdp/bat/commit/76034880ae28dafce0d0192f55e1ef6815cf0c25))
    - Add Stylus syntax test file ([`7681898`](https://github.com//sharkdp/bat/commit/768189859ab7737dc5a071a4126f90ad944f99dd))
    - Add Strace syntax test file ([`6e5a2a5`](https://github.com//sharkdp/bat/commit/6e5a2a5c519cd39eb0fcb673ecb6e19b1c955949))
    - Add Robot Framework syntax test file ([`9935c49`](https://github.com//sharkdp/bat/commit/9935c4984c754201fa6192da3d00ae6a95a8ed82))
    - Add Rego syntax test file ([`a0a5e30`](https://github.com//sharkdp/bat/commit/a0a5e30c3961273b16609c566bddb5f802d8279b))
    - Add Puppet syntax test file ([`4126bbe`](https://github.com//sharkdp/bat/commit/4126bbeeadef445f70f7292a81bf396928c050e1))
    - Add NAnt Build File syntax test file ([`a5b7929`](https://github.com//sharkdp/bat/commit/a5b79295d7e48c81fe6266eb1fdf7b938f45811b))
    - Add Literate Haskell syntax test file ([`e917784`](https://github.com//sharkdp/bat/commit/e91778493204c29d875975b81a52074419bbbfa9))
    - Add jsonnet syntax test file ([`f46b90d`](https://github.com//sharkdp/bat/commit/f46b90d28df54d67635c009fc4d7ebe4a08b4bec))
    - Add Java Server Page (JSP) syntax test file ([`64cbfbe`](https://github.com//sharkdp/bat/commit/64cbfbed470926c7ed18521da3081797d1b7633d))
    - Add 'NOTICE' to list of file names to skip ([`1ced35e`](https://github.com//sharkdp/bat/commit/1ced35ec760f4635156c56ba7d8a3a47bc7158a2))
    - Add Fortran Namelist syntax test file ([`16d3467`](https://github.com//sharkdp/bat/commit/16d346773b284558e7ee1b0da396f29b9a403299))
    - Add Fortran (Fixed Form) syntax test file ([`b02120c`](https://github.com//sharkdp/bat/commit/b02120cf6679b50bb4e4b2dd9c8e72be5ed4372b))
    - Add Fortran (Modern) syntax test file ([`702b5ca`](https://github.com//sharkdp/bat/commit/702b5caf2d27955da0caf6e4432823b1e7d7fb99))
    - Add F# syntax test file ([`d395f64`](https://github.com//sharkdp/bat/commit/d395f64f580c4c8f48d5c3b2e94d78ee9001c3ef))
    - Add CoffeeScript syntax test file ([`c962704`](https://github.com//sharkdp/bat/commit/c9627040ccc74919af59a7602decf7797ef01085))
    - Add Cabal syntax test file ([`b1f6943`](https://github.com//sharkdp/bat/commit/b1f69434f9bbf02641ca68cba28f10a1c126dd72))
    - CHANGELOG.md: Apply DotENV syntax also for .env.default and .env.defaults ([`bcca56e`](https://github.com//sharkdp/bat/commit/bcca56e3b1d10630f4e8f9034dcc02fa71fdfd56))
    - DotENV.sublime-syntax: Re-exported after .tmLanguage update ([`2f98610`](https://github.com//sharkdp/bat/commit/2f986109296834622df1faea4394d0a2d0c3eef5))
    - Bump assets/syntaxes/02_Extra/DotENV from `a1c9176` to `58201ba` ([`89539ff`](https://github.com//sharkdp/bat/commit/89539ff247019a000f4c29ed8eed66da2bb6f0a6))
    - Refactor "Use `matches` macro" ([`aa74d19`](https://github.com//sharkdp/bat/commit/aa74d199400b5f711545a96c9b3e229dd3993176))
    - Update `CHANGELOG.md` for #1661 ([`cf7d9ef`](https://github.com//sharkdp/bat/commit/cf7d9ef962de909f3215a3b942e69b70b3293884))
    - Use the functional update syntax ([`425a0f9`](https://github.com//sharkdp/bat/commit/425a0f90e9ee2734693dae523369873e53dc553c))
    - Implement `From<..>` instead of `Into<..>` ([`a27814d`](https://github.com//sharkdp/bat/commit/a27814db8e651100bd718efb34b2dc6a034973ea))
    - Use the functional update syntax ([`9702f52`](https://github.com//sharkdp/bat/commit/9702f5256c5f7feb6d207ddfb8d7c41676e1756d))
    - Use `matches` macro ([`23fd11e`](https://github.com//sharkdp/bat/commit/23fd11e806b52dbea8c4e1d635a80ceb1f98aafa))
    - Use `!theme.is_empty()` ([`304ee14`](https://github.com//sharkdp/bat/commit/304ee1489c360f6d852fbf4954a401ab887921c6))
    - add section about integration with fzf ([`07d4179`](https://github.com//sharkdp/bat/commit/07d41792743d45c290945e87952ad6c39f950b79))
    - fix typo ([`1439dde`](https://github.com//sharkdp/bat/commit/1439dde265ac7ac3ffd15244f9624270b1feac12))
    - Merge pull request #1654 from mohamed-abdelnour/support-dash-syntax ([`bef0bf1`](https://github.com//sharkdp/bat/commit/bef0bf16542c9e4ac20dae9153e75eeb402b3bc0))
    - Merge pull request #1655 from mohamed-abdelnour/support-xaml-files ([`52c11fe`](https://github.com//sharkdp/bat/commit/52c11fe23d2d9ed027fae81d2f84de48710ac9ff))
    - Merge master ([`8435cad`](https://github.com//sharkdp/bat/commit/8435cad602a92a3c359fb1fb8ce9740d8065a290))
    - Update CHANGELOG for #1655 ([`6fc9641`](https://github.com//sharkdp/bat/commit/6fc9641f6adfa9c3fbc3ab3c6e2ffcaed6efc502))
    - Add XAML test file ([`8b63414`](https://github.com//sharkdp/bat/commit/8b6341458b742736bf42da0589c10a2bff864a4c))
    - Add dash test file ([`8b787b4`](https://github.com//sharkdp/bat/commit/8b787b4f704a2f9862958a1b02695d980a1a9e66))
    - Update CHANGELOG for #1654 ([`ef5154d`](https://github.com//sharkdp/bat/commit/ef5154d5b34d3f2853e03da2ee55d90af35b4b65))
    - Add support for XAML files ([`7c49919`](https://github.com//sharkdp/bat/commit/7c49919297717eeca616b382ab398f12958ed56f))
    - Add support for dash shebang ([`395a169`](https://github.com//sharkdp/bat/commit/395a169104446a5ca240329530de91695dbdc72e))
    - Merge pull request #1652 from Zeta611/master ([`93f710b`](https://github.com//sharkdp/bat/commit/93f710bdffcd71fb1533609339778ea699a0c8b8))
    - Fix broken img link ([`170badf`](https://github.com//sharkdp/bat/commit/170badfdd1c00af9073a0199dcfea4e901ebca73))
    - Update and polish Korean translation ([`978b9ad`](https://github.com//sharkdp/bat/commit/978b9adc56b3e544687ab7c97809d1393a5e79d9))
</details>

# v0.18.1 (2021-05-13)

## Bugfixes

- Mouse support and screen clearing broken for `less` versions with minor version number (581.2), see #1629 and #1639 (@aswild)

## Other

- `Input::ordinary_file` and `Input::with_name` now accept `Path` rather than `OsStr` see #1571 (@matklad)
- The `LESS` environment variable is now included in `bat --diagnostic`, see #1589 (@Enselic)
- Increased min. required Rust version to 1.45

## Syntaxes

- Improved the Syslog syntax highlighting, see #1606 (@keith-hall)
- Replaced "Advanced CSV" with a custom CSV syntax definition written especially for `bat`; see #1574 (@keith-hall)
- Added SystemVerilog file syntax, see #1580 (@SeanMcLoughlin)
- Added Solidity and Vyper syntax, see #1602 (@Ersikan)

## New themes

- Dark+ VS Code theme, see #1588 and #1598 (@PatriotRossii)



## Commit Statistics

<csr-read-only-do-not-edit/>

 - 60 commits contributed to the release over the course of 73 calendar days.
 - 2 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 1 unique issue was worked on: [#1612](https://github.com//sharkdp/bat/issues/1612)

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **[#1612](https://github.com//sharkdp/bat/issues/1612)**
    - check docs ([`8321cc6`](https://github.com//sharkdp/bat/commit/8321cc661c94a2796c5033b6c368314f6853d04b))
 * **Uncategorized**
    - Merge pull request #1649 from sharkdp/v0.18.1 ([`3aea514`](https://github.com//sharkdp/bat/commit/3aea51455b8acf553f4481996f93d7e219130c2b))
    - Update binary assets ([`f827ed3`](https://github.com//sharkdp/bat/commit/f827ed338bf5702d7f755e57648e7336492e886a))
    - Bump version, update dependencies ([`73dab51`](https://github.com//sharkdp/bat/commit/73dab51ad1b85ae43efc6512a0599ea46f96f0f3))
    - Updated CHANGELOG ([`9328007`](https://github.com//sharkdp/bat/commit/9328007d056044b0f04821e484e4b734650dc4b3))
    - Merge pull request #1598 from PatriotRossii/feature/dark_plus_theme ([`8d173cd`](https://github.com//sharkdp/bat/commit/8d173cd960e7a3b22b852313966336a93657d317))
    - Merge pull request #1643 from sharkdp/dependabot/submodules/assets/syntaxes/02_Extra/Lean-29a03a8 ([`fb0f12a`](https://github.com//sharkdp/bat/commit/fb0f12a22175b15745aa24fbdd07c73609cadb46))
    - Merge pull request #1647 from sharkdp/dependabot/cargo/serde-1.0.125 ([`9866408`](https://github.com//sharkdp/bat/commit/9866408b7234503b1f1624522e0c94dd8ca32fb4))
    - Merge pull request #1648 from sharkdp/dependabot/cargo/console-0.14.1 ([`aaac56d`](https://github.com//sharkdp/bat/commit/aaac56d89560b08ca92bb71be52a3953896fc55a))
    - Merge pull request #1646 from sharkdp/dependabot/cargo/bugreport-0.4.0 ([`516da77`](https://github.com//sharkdp/bat/commit/516da7719f3e298e185d83ff09fe9dd213a60b2d))
    - Merge pull request #1645 from sharkdp/dependabot/submodules/assets/syntaxes/02_Extra/FSharp-9e4645c ([`d4df0b4`](https://github.com//sharkdp/bat/commit/d4df0b4405ae3853463daa0a60fce82a0342fc84))
    - Merge branch 'master' into feature/dark_plus_theme ([`bfa9c3b`](https://github.com//sharkdp/bat/commit/bfa9c3bef60d97238b6d9e1495b58b8a8c8151d8))
    - Merge pull request #1606 from sharkdp/syslog ([`2904f24`](https://github.com//sharkdp/bat/commit/2904f24ea803ce3fd51cb4080949c8d53e14d06e))
    - Fix less version parsing for minor versions of less ([`91a347b`](https://github.com//sharkdp/bat/commit/91a347bf6d549ba543f1b4bfdc2a05f269c975c8))
    - Update SublimeEthereum version ([`e219c8f`](https://github.com//sharkdp/bat/commit/e219c8fc03b632272d7a75f30f364fcdcee0a9b0))
    - Add syntax test for Vyper language ([`9eb26b7`](https://github.com//sharkdp/bat/commit/9eb26b702c0a7a8e501ebcec1cb38fbf1d7e196f))
    - Add syntax tests for Solidity language. ([`c933be9`](https://github.com//sharkdp/bat/commit/c933be926dfed57f9601c31cef918a3eff86a843))
    - Add Solidity and Vyper syntax ([`fc88040`](https://github.com//sharkdp/bat/commit/fc88040b31635631327432da9babcf73b0c1278f))
    - Merge branch 'master' into syslog ([`5db3c93`](https://github.com//sharkdp/bat/commit/5db3c930576a8aa4705843dd512303059fc98771))
    - Merge branch 'master' into feature/dark_plus_theme ([`aa442b5`](https://github.com//sharkdp/bat/commit/aa442b57954144dda697d063e4c43b9ccbc93101))
    - Bump assets/syntaxes/02_Extra/SCSS_Sass from `4868322` to `63819a1` ([`b16cacc`](https://github.com//sharkdp/bat/commit/b16cacc758fb60777072ff8f9b9ae8ba97e4f5ae))
    - Bump assets/syntaxes/02_Extra/Julia from `45a1b6f` to `cc13e7d` ([`613b41e`](https://github.com//sharkdp/bat/commit/613b41e3572a2932ca22800c66c3230a54cf3e5b))
    - Bump console from 0.14.0 to 0.14.1 ([`d405aa9`](https://github.com//sharkdp/bat/commit/d405aa98d571ff72431595435e448b6dce2c3557))
    - Upgrade to GitHub-native Dependabot ([`285b155`](https://github.com//sharkdp/bat/commit/285b1556a97e0499a2b50679ac6c359942c3e6e7))
    - Bump assets/syntaxes/02_Extra/Lean from `824213d` to `29a03a8` ([`09711cd`](https://github.com//sharkdp/bat/commit/09711cd6f9d1d163bddd068cb29b0a0ef49ff8c7))
    - Bump console from 0.14.0 to 0.14.1 ([`51451a9`](https://github.com//sharkdp/bat/commit/51451a9636f46ef647ccb885b8d740acb0e630de))
    - Bump serde from 1.0.124 to 1.0.125 ([`fbc03da`](https://github.com//sharkdp/bat/commit/fbc03da9972ef9010bb23c26906916554805ffeb))
    - Bump bugreport from 0.3.0 to 0.4.0 ([`1a610db`](https://github.com//sharkdp/bat/commit/1a610dbdd252f23c28d0df6b35e89bd71f0979c4))
    - Bump assets/syntaxes/02_Extra/FSharp from `c18616d` to `9e4645c` ([`a30e3c9`](https://github.com//sharkdp/bat/commit/a30e3c9066bb59af023f5a989f46b5b1bad2881c))
    - Remove variable.other from CSV highlighting ([`848ceb6`](https://github.com//sharkdp/bat/commit/848ceb6f10b6d5f9fcfca5b3530220a5b2403f84))
    - Improvements to CSV highlighting ([`3559079`](https://github.com//sharkdp/bat/commit/3559079de0081987741a1d6fb2ea9ee4b1a42e56))
    - add warning highlighting to log file syntax definition ([`b3ab843`](https://github.com//sharkdp/bat/commit/b3ab8439547b6204167cb05ac7360160b54a1568))
    - add word boundaries to log syntax highlighting ([`867cf63`](https://github.com//sharkdp/bat/commit/867cf63dd9db70adad65f4e8a9ac38a4fce352e9))
    - Add independent log syntax ([`7c4edac`](https://github.com//sharkdp/bat/commit/7c4edacb2be13eef3afe0e2b23fb83c7ca813dd7))
    - Merge pull request #1589 from Enselic/include-less-in-diagnostic ([`0b44aa6`](https://github.com//sharkdp/bat/commit/0b44aa6f68ab967dd5d74b7e02d306f2b8388928))
    - Update changelog for #1589 ([`aa09a9d`](https://github.com//sharkdp/bat/commit/aa09a9dc04b2aec5572b2d389b1800f48b30bd8b))
    - improve syslog syntax ([`782ede5`](https://github.com//sharkdp/bat/commit/782ede5db5ad601e44468c9ca32bd201e5c75aaa))
    - replace Syslog dependency with a custom syntax ([`1537733`](https://github.com//sharkdp/bat/commit/1537733e6b0d764e092b701565de94c51ef3ecf1))
    - add example syslog file for highlighting tests ([`ba0d436`](https://github.com//sharkdp/bat/commit/ba0d43672b783d4a7195b21bfb378e7ba0c4d6ab))
    - Change TheClams/SystemVerilog submodule to use HTTPS ([`4384d25`](https://github.com//sharkdp/bat/commit/4384d25c79d1a2d089df0a1de60b264469415eea))
    - Revert "Update of themes.bin that adds Dark+" ([`b6e3786`](https://github.com//sharkdp/bat/commit/b6e37865298fe360d3683f5cd3d6df7a12c6f2dd))
    - Update of themes.bin that adds Dark+ ([`6fff013`](https://github.com//sharkdp/bat/commit/6fff01397d1e3433653c4818f16548c1d2f5b780))
    - Add "Visual Studio Dark+" theme to vec of themes ([`0cd52e5`](https://github.com//sharkdp/bat/commit/0cd52e5be1661825944d17f101f727b87563320a))
    - Add Dark+ theme ([`c14dd34`](https://github.com//sharkdp/bat/commit/c14dd34dca3cba39b98c8110761bb0cad782dd68))
    - Include LESS in --diagnostic ([`e04fbd1`](https://github.com//sharkdp/bat/commit/e04fbd199222645388c5a978938f3c23ef7f7a22))
    - Add SystemVerilog support ([`d89fa3e`](https://github.com//sharkdp/bat/commit/d89fa3ebc2639375b0184437607003a639d8ecef))
    - Update copyright year ([`db57454`](https://github.com//sharkdp/bat/commit/db57454f3fff5720e80b2783cd827a31dd2c90ae))
    - Remove explicit allow of clippy::match_bool since MSRV 1.45 bump ([`8f93844`](https://github.com//sharkdp/bat/commit/8f938444270e4231936c4f7b13641ae707d97c01))
    - Update dependencies, MSRV: 1.45 ([`52f84b0`](https://github.com//sharkdp/bat/commit/52f84b063c9e4081b53fde8ebc2ce31a36553d99))
    - 'mut self' => 'self', remove pub ([`2e7f2b6`](https://github.com//sharkdp/bat/commit/2e7f2b6c0709d758e4e92e5b9cab134d8d04b931))
    - Improve readability ([`35347c2`](https://github.com//sharkdp/bat/commit/35347c23109400e9694f073bf8fe901b3ca1d59f))
    - Bump assets/syntaxes/02_Extra/Crystal from `5e032ff` to `eb63666` ([`b489fc7`](https://github.com//sharkdp/bat/commit/b489fc75c9e6f54ab0dd855351df4a33a109a493))
    - Bump assets/syntaxes/02_Extra/LESS from `44632e1` to `a2eae04` ([`fc24cb2`](https://github.com//sharkdp/bat/commit/fc24cb2a77fb22512eaca3a73c127a9c6c9e9d0a))
    - Update bug report issue template (--diagnostic option) ([`2540311`](https://github.com//sharkdp/bat/commit/2540311cdfe1e87e386825cbdb2805bb29402cce))
    - Bump nix from 0.19.1 to 0.20.0 ([`3880888`](https://github.com//sharkdp/bat/commit/38808882860e68f9f73359159ca71c7190cea3dc))
    - Bump path_abs from 0.5.0 to 0.5.1 ([`5a806c2`](https://github.com//sharkdp/bat/commit/5a806c2149ad89ed2547020167ac0a5e054b3da3))
    - Bump assets/syntaxes/02_Extra/Julia from `e2b1cb5` to `45a1b6f` ([`8a926d5`](https://github.com//sharkdp/bat/commit/8a926d511ae0afbcec302011c1b69f14e81a02b6))
    - Bump assets/syntaxes/02_Extra/Lean from `49d1853` to `824213d` ([`8885285`](https://github.com//sharkdp/bat/commit/8885285b101b98ce811195982a6e40a16b343f5b))
    - Bump assets/themes/zenburn from `7f6fb86` to `702023d` ([`d1f0181`](https://github.com//sharkdp/bat/commit/d1f0181947beca3cf1060a8150fa8369ea196bcb))
    - Add new unreleased section ([`b147443`](https://github.com//sharkdp/bat/commit/b147443c3204c24f3aca9969b1d13de69e547365))
</details>

# v0.18.0 (2021-02-28)

## Features

- Use a pager when `bat --list-languages` is called, see #1394 (@stku1985)

## Bug Fixes

 - <csr-id-973ea984c8708f477573d9f3ffcf7387dd92f4ef/> warnings of ShellCheck
   SC2155: Declare and assign separately to avoid masking return values.
   SC2164: Use cd ... || exit in case cd fails.
   SC2230: which is non-standard. Use builtin 'command -v' instead.

## Bugfixes

- If the last line doesn't end with a newline character, don't add it if `--style=plain`, see #1438 (@Enselic)
- Only print themes hint in interactive mode (`bat --list-themes`), see #1439 (@rsteube)
- Make ./tests/syntax-tests/regression_test.sh work on recent versions of macOS, see #1443 (@Enselic)
- VimL syntax highlighting fix, see #1450 (@esensar)
- Print an 'Invalid syntax theme settings' error message if a custom theme is broken, see #614 (@Enselic)
- If plain mode is set and wrap is not explicitly opted in, long lines will no be truncated, see #1426
- If `PAGER` (but not `BAT_PAGER` or `--pager`) is `more` or `most`, silently use `less` instead to ensure support for colors, see #1063 (@Enselic)
- If `PAGER` is `bat`, silently use `less` to prevent recursion. For `BAT_PAGER` or `--pager`, exit with error, see #1413 (@Enselic)
- Manpage highlighting fix, see #1511 (@keith-hall)
- `BAT_CONFIG_PATH` ignored by `bat` if non-existent, see #1550 (@sharkdp)

## Other

- Performance improvements, see #1421 (@LovecraftianHorror)
- Added a new `--diagnostic` option to collect information for bug reports, see #1459 (@sharkdp)
- Modified default theme colors to differentiate between a JSON key and a string value, see #1400 (@keith-hall)
- Upped min required Rust version to 1.42

## Syntaxes

- Added Zig syntax, see #1470 (@paulsmith)
- Added Lean syntax, see #1446 (@Julian)
- Added `.resource` extension for Robot Framework files, see #1386
- Added `gnuplot` syntax, see #1431 (@sharkdp)
- Highlight *.pac (Proxy auto-config) files as JavaScript, see #1515 (@sharkdp)

## New themes

- `ansi` replaces `ansi-dark` and `ansi-light`, see #1104 and #1412 (@mk12). **Breaking change:** users that were previously using one of the `ansi-*` themes should switch to `ansi`.
- The Gruvbox theme has been updated, see #1291 (@j0hnmeow). **Breaking change:** users that were previously using `gruvbox` or `gruvbox-white` should update and use `gruvbox-dark`/`gruvbox-light` instead.

## `bat` as a library

- The following `PrettyPrinter` methods have been removed (they were previously deprecated):
   - `input_stdin_with_name`
   - `input_from_bytes_with_name`
   - `input_from_reader_with_name`
   - `vcs_modification_markers` (if the `git` feature is not enabled)


## Commit Statistics

<csr-read-only-do-not-edit/>

 - 219 commits contributed to the release over the course of 93 calendar days.
 - 42 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 4 unique issues were worked on: [#1444](https://github.com//sharkdp/bat/issues/1444), [#1479](https://github.com//sharkdp/bat/issues/1479), [#1483](https://github.com//sharkdp/bat/issues/1483), [#1522](https://github.com//sharkdp/bat/issues/1522)

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **[#1444](https://github.com//sharkdp/bat/issues/1444)**
    - Fix #1443 macOS: regression_test.sh: mktemp: illegal option ([`60e00d4`](https://github.com//sharkdp/bat/commit/60e00d49a99f33eb90397c6932c770d82fd481ec))
 * **[#1479](https://github.com//sharkdp/bat/issues/1479)**
    - Remove duplicate set-output calls ([`939a6a5`](https://github.com//sharkdp/bat/commit/939a6a5f4dc4e0874d4a1f112fdb6c8e330d85f8))
 * **[#1483](https://github.com//sharkdp/bat/issues/1483)**
    - Target VimL upstream .sublime-syntax file ([`b458292`](https://github.com//sharkdp/bat/commit/b458292a69bf21b90d7646bf22fb40cdac9a28f8))
 * **[#1522](https://github.com//sharkdp/bat/issues/1522)**
    - Update dependencies; replace unmaintained ([`573f34d`](https://github.com//sharkdp/bat/commit/573f34d757434a5580abd45f5a059c908552d0b3))
 * **Uncategorized**
    - Use tempfiles for clircle tests ([`b8a18d3`](https://github.com//sharkdp/bat/commit/b8a18d3ebb5b29162c346b74174239ea077f0796))
    - Change circle detection to use new more conservative method and run in main loop instead of before the loop ([`694b319`](https://github.com//sharkdp/bat/commit/694b31909a38989fbb153164f0218f1f0377d2c9))
    - Update syntaxes and themes ([`a98811b`](https://github.com//sharkdp/bat/commit/a98811b6d7a648ac92f5bbe6fe76e7389d756866))
    - Bump version to v0.18.0 ([`cfc505e`](https://github.com//sharkdp/bat/commit/cfc505e1e522df8725dc128bedba58f1c5fa292e))
    - integration_tests: Use tempdir() in config_location_when_generating ([`3af3549`](https://github.com//sharkdp/bat/commit/3af35492320077b2abf7cc70117ea02aa24389a3))
    - integration_tests.rs: Add test config_location_when_generating ([`643f0bc`](https://github.com//sharkdp/bat/commit/643f0bcbe387f5e15a3c97bff5b12f9dc10285a6))
    - Add workflow_dispatch to enable manual builds ([`9db9a38`](https://github.com//sharkdp/bat/commit/9db9a38565ca8ed78a5ec2f3d0363d548d9249fb))
    - Do not ignore non-existent BAT_CONFIG_PATH ([`ca60937`](https://github.com//sharkdp/bat/commit/ca60937c2e0b6b3de3d3e379fd419cdac9daecd6))
    - Update CHANGELOG ([`2aa3ed9`](https://github.com//sharkdp/bat/commit/2aa3ed9da889a0b32b478ccbbf2738cedec08947))
    - Improve Monokai Extended JSON key color with a patch ([`7768433`](https://github.com//sharkdp/bat/commit/7768433d091c77efb6752b6407b25fb8e8a259b9))
    - Add section about integration with fzf ([`c569774`](https://github.com//sharkdp/bat/commit/c569774e1a8528fab91225dabdadf53fde3916ea))
    - Don't run jobs twice in PRs ([`0371f55`](https://github.com//sharkdp/bat/commit/0371f55541292ec6f27f95729aa1d319973800f0))
    - Build step naming ([`bc35592`](https://github.com//sharkdp/bat/commit/bc35592fd95cab1f622bb3d59303ad46b3ad4fbf))
    - Improved job names ([`f5d8344`](https://github.com//sharkdp/bat/commit/f5d834407ed67f54bcc5a7eb11ded7d9aa9f041d))
    - Remove code coverage computation ([`557a748`](https://github.com//sharkdp/bat/commit/557a748ac76bb667688cdae44d949cae0d7fedb2))
    - Build: Rename 'Check is release' step to 'Check for release' ([`3f10f71`](https://github.com//sharkdp/bat/commit/3f10f71ad2e2ff90f9a04a9e3fe7f6491128e2cf))
    - Build: Add separate 'Calculate test options' step ([`7b6388b`](https://github.com//sharkdp/bat/commit/7b6388b19f4769a728f3299c6d851a6d1ab8062f))
    - Build: Check IS_RELEASE in separate step ([`b98ec4b`](https://github.com//sharkdp/bat/commit/b98ec4bbc56ad1b83900369ec94f08038468b316))
    - Build: Remove unused ${{ matrix.job.cargo-options }} expansions ([`94fd481`](https://github.com//sharkdp/bat/commit/94fd481f36166eb2c54edea7c78aa5396f023972))
    - Fix broken resolv.conf highlighting ([`94496df`](https://github.com//sharkdp/bat/commit/94496df3b01dc874071b7e23fe52bd0aec332b74))
    - Build: Use package-specific staging dir ([`0e5ea9c`](https://github.com//sharkdp/bat/commit/0e5ea9c354509ecc408cf0f40bf9d5ef6b62f9f0))
    - Highlight *.pac files as JavaScript ([`7eabb5e`](https://github.com//sharkdp/bat/commit/7eabb5e05a975e7e6b54d8a57ce683b5f6c61eba))
    - Fix clippy suggestion ([`9ad401b`](https://github.com//sharkdp/bat/commit/9ad401be872faf491eaaff40eff40103f9cbb855))
    - Use less binary specified in bat config for --diagnostic ([`f874c8e`](https://github.com//sharkdp/bat/commit/f874c8e4dbb13defbb842398853cdea8e23e39d9))
    - Make less version check accept a path to the less binary ([`025c5c0`](https://github.com//sharkdp/bat/commit/025c5c061b8b79609e87e58d74858e9b90fc1458))
    - Build: Introduce and use new 'Strip release bin' step ([`eac36dd`](https://github.com//sharkdp/bat/commit/eac36dd3b51b28ef73b46f056aec905131c0069c))
    - Wording ([`70cf8a4`](https://github.com//sharkdp/bat/commit/70cf8a4ec57c517d9cd31a8ca04cbcc3338bf63b))
    - README.md: Warn about buggy snap packages ([`37d0d89`](https://github.com//sharkdp/bat/commit/37d0d8984c35ab20489b0d820dc901c62de05337))
    - Add support for env vars and manpage refs with dots to Manpage syntax ([`2e8c0a3`](https://github.com//sharkdp/bat/commit/2e8c0a39dbaa619e833e079b51ea0749cc99c727))
    - Bump assets/syntaxes/02_Extra/Svelte from `aee0676` to `c71f129` ([`deec290`](https://github.com//sharkdp/bat/commit/deec2902e4da2f9b826e6fa92c76b73685edd8e3))
    - Bump assets/themes/onehalf from `8992311` to `141c775` ([`e334375`](https://github.com//sharkdp/bat/commit/e3343759b7b096c1c8b80eea24aecae33f296fae))
    - Bump assets/syntaxes/02_Extra/CMake from `21e9698` to `7d6231c` ([`fa72f86`](https://github.com//sharkdp/bat/commit/fa72f86e17ef35552e163e343456f3223ae65014))
    - Bump assets/syntaxes/02_Extra/Lean from `7e99440` to `49d1853` ([`6527126`](https://github.com//sharkdp/bat/commit/6527126e507fdb4770ec839efab8982d92cdb133))
    - Bump serde from 1.0.118 to 1.0.123 ([`4aec022`](https://github.com//sharkdp/bat/commit/4aec022065e5d8a6bb3a28a4fde57e7bd47b4bdc))
    - Bump predicates from 1.0.6 to 1.0.7 ([`a26c5b8`](https://github.com//sharkdp/bat/commit/a26c5b82190747c77fcae61e14b92c0b14f73019))
    - Build: Move PKG_* vars to 'Package' step ([`d36b091`](https://github.com//sharkdp/bat/commit/d36b091fd7c9050c80078c0fe286d09dbab0f45a))
    - Build: Move DPKG_* vars to 'Debian package' step ([`3dcf025`](https://github.com//sharkdp/bat/commit/3dcf02549e373adcb12a0b031e8d17bddd9e204b))
    - Build: Split out dir creation into the individual package steps ([`e402011`](https://github.com//sharkdp/bat/commit/e402011a735561157b304180810c38bce68f0326))
    - get_pager(): Simplify use_less_instead expression ([`dc1620d`](https://github.com//sharkdp/bat/commit/dc1620d1f0e44ddaefda6bb74899a60160df6de6))
    - CHANGELOG.md: Add entry for #1413 ([`c48e779`](https://github.com//sharkdp/bat/commit/c48e779e8a01d50ef6967c226322efc15d8997c7))
    - Fix #1413 Can't use `bat` at all! (Error: Use of bat as a pager is disallowed...) ([`dd0d44b`](https://github.com//sharkdp/bat/commit/dd0d44bbb3608618427153515a71cdc11c041988))
    - Remove deprecated PrettyPrinter methods ([`060b998`](https://github.com//sharkdp/bat/commit/060b9981b545cb7c813c3261442c9fa718e4d61a))
    - Fix typo in bug report template ([`b257139`](https://github.com//sharkdp/bat/commit/b25713938d23b0ff59e82e3e969b56423028ce53))
    - Update map-syntax text in manpage to match --help ([`5b09561`](https://github.com//sharkdp/bat/commit/5b09561114df82931b0e462b60ee369ca501a982))
    - Add missing command line options to the manpage ([`fcde482`](https://github.com//sharkdp/bat/commit/fcde4824d571055e802c8a589abb97861dd0f0a6))
    - Add comment regarding use of bat as a library, closes #953 ([`b0e5828`](https://github.com//sharkdp/bat/commit/b0e5828d3f6d9daa667055afd12801a182fe4a95))
    - Add 'batcat' comment in MANPAGER suggestion, closes #1434 ([`b7d499d`](https://github.com//sharkdp/bat/commit/b7d499d1be0dfa2e18a409596a89c45fbb15c1dd))
    - Improve --map-syntax documentation, see #1386 ([`1ae16fc`](https://github.com//sharkdp/bat/commit/1ae16fca90e326e11a284de232362d43059a07fb))
    - Fix minor mistakes in --map-syntax documentation ([`0e2bef3`](https://github.com//sharkdp/bat/commit/0e2bef3b79bdb6a6ac537ea401cd2dfe813d9735))
    - Build: Split up into separate 'Debian package' step ([`221c981`](https://github.com//sharkdp/bat/commit/221c9815a51addc1a37618392e9c2cc6ca10c047))
    - Explicitly allow clippy::match_bool until we bump MSRV ([`c5c683f`](https://github.com//sharkdp/bat/commit/c5c683f67cecdfa6cc9f991594966810a3cf1e29))
    - Update number of syntaxes ([`149a017`](https://github.com//sharkdp/bat/commit/149a0177cd6804a9261a9e86794e64df742aca48))
    - Merge pull request #1402 from Enselic/fix-1063 ([`252e5a6`](https://github.com//sharkdp/bat/commit/252e5a6b1360bab10b90f59926e7fa784c1b7f9d))
    - simply use 'sed' to extract crate information ([`faa27ed`](https://github.com//sharkdp/bat/commit/faa27ed6e32af215e56775416f918168b0d5971c))
    - Use cache for faster install ([`f3227c2`](https://github.com//sharkdp/bat/commit/f3227c259e76a537d7abc0f3a5e97dc5a4640618))
    - Use 'cargo get' to extract crate metadata ([`2cfeeba`](https://github.com//sharkdp/bat/commit/2cfeebab90eb88c8229967c5e6d43ec1576651d3))
    - Revert accidental change to assets/syntaxes/02_Extra/VimL ([`8dd67cc`](https://github.com//sharkdp/bat/commit/8dd67cca0c808e44774fb5d4b68999f63aa3cccd))
    - Move 'mocked pagers' utils to separate file ([`eda72c3`](https://github.com//sharkdp/bat/commit/eda72c31b27584d61166c48b36c642cd3a51c3a3))
    - PagerKind::from(): Simplify ([`7809008`](https://github.com//sharkdp/bat/commit/7809008016aef9af72a285d1f7de9c978acba737))
    - fn mocked_pager: Simplify with format! ([`c2c2b02`](https://github.com//sharkdp/bat/commit/c2c2b0211a7efa2058c0cf02467db1e47413d62a))
    - pager.rs: Some comment fixups ([`dd6f57e`](https://github.com//sharkdp/bat/commit/dd6f57e1073100ab0f85f272177efe9568550940))
    - [Bat]PagerEnvVar -> EnvVar[Bat]Pager ([`dfe7a60`](https://github.com//sharkdp/bat/commit/dfe7a601402b974b57f8f103fe5dfec69b69f14a))
    - pager.rs: Limit visibilities to pub(crate) ([`fc30277`](https://github.com//sharkdp/bat/commit/fc30277cfa7c5d0df186460d572af074d0a5aa60))
    - Merge remote-tracking branch 'origin/master' into fix-1063 ([`02e6ff4`](https://github.com//sharkdp/bat/commit/02e6ff4183f6b24ff4fe297195287b65bc66eaee))
    - Build: DEPLOY -> IS_RELEASE and inline it ([`e3b1142`](https://github.com//sharkdp/bat/commit/e3b114236452dc5a9084f623b3bd4b39100edd15))
    - Build: Adapt release version regex to bat ([`8832ff3`](https://github.com//sharkdp/bat/commit/8832ff3c6a0eafae6eded3389c83739b0719a355))
    - Build: Always build and upload Debian packages ([`505ff10`](https://github.com//sharkdp/bat/commit/505ff10dc629b65d81630180d59960c369c39539))
    - Build: Use Cargo.toml version instead of tag ([`9a3a554`](https://github.com//sharkdp/bat/commit/9a3a5545e70d10f62d645e5f553b75ae13feb755))
    - Use fixed OS versions instead of 'latest' ones ([`4391906`](https://github.com//sharkdp/bat/commit/43919066ad3cb12acb62d78758d50624ae297f5d))
    - Add ChangeLog entry ([`7ada963`](https://github.com//sharkdp/bat/commit/7ada963ec2f66470064c7150eb646c609a4e16ad))
    - Add gnuplot syntax ([`4c523af`](https://github.com//sharkdp/bat/commit/4c523af1ab4b0fdbb403ae31423bb6cf1dc7f65f))
    - --diagnostic: add MANPAGER environment variable ([`c5c28eb`](https://github.com//sharkdp/bat/commit/c5c28eb05b89e8dc095da1593b63ea53862800c8))
    - Fix link in doc/assets.md ([`2292453`](https://github.com//sharkdp/bat/commit/22924532d0147459646315aead0ba63ba7e2676d))
    - Add note about breaking change ([`7c22716`](https://github.com//sharkdp/bat/commit/7c227169a4e174d73605a8ff3180a70d750970ca))
    - Update dependencies ([`8e4b0b4`](https://github.com//sharkdp/bat/commit/8e4b0b4377d5cb33e635d6e2c9ddee061e327597))
    - Use unstable sort for theme test ([`6d27df3`](https://github.com//sharkdp/bat/commit/6d27df3b7721f0c9e8b00a50d454c2082664da12))
    - Use BatTester::default instead of BatTester::new ([`73d14f4`](https://github.com//sharkdp/bat/commit/73d14f4655780a4eba0f0664fb7d1e094d885a90))
    - Enable clippy::style checks ([`19b8c53`](https://github.com//sharkdp/bat/commit/19b8c53c46922b9c6bbd620bb01fd688778c728d))
    - Fix clippy suggestion: .or_else(|| Some(…)) => .or(Some(…)) ([`cd7be01`](https://github.com//sharkdp/bat/commit/cd7be018fea57e76399290876fe31764b990bafb))
    - Use new matches!(…) macro to simplify code ([`03a2710`](https://github.com//sharkdp/bat/commit/03a2710a0886706621be6d68ce8b0f9bf1ab2d7d))
    - Add a Default implementation for PrettyPrinter ([`09fbabb`](https://github.com//sharkdp/bat/commit/09fbabb0b8b278d098765684cdfbfcd928bf209a))
    - Addressed PR feedback. Upped min version and used matches! macro ([`de6cb75`](https://github.com//sharkdp/bat/commit/de6cb75f4bb5d6e65301b5e9a9890228760da4c4))
    - fix bug where long lines were truncated in plain mode without wrap=never not being set ([`83c9cb7`](https://github.com//sharkdp/bat/commit/83c9cb7907a7a6ea48c7c76d51e5a7b0332bf398))
    - Build: Make 'Upload build artifacts' more like deploy ([`7ffb04a`](https://github.com//sharkdp/bat/commit/7ffb04a17aeed4815d97f184ead450ceb58d6a6f))
    - Add Enselic as a maintainer ([`c38c186`](https://github.com//sharkdp/bat/commit/c38c186d264ce823463104f300e3a07aafcf5eea))
    - Build: Stop building on Ubuntu 16.04 ([`e22a9a6`](https://github.com//sharkdp/bat/commit/e22a9a69b13d4c402ef6972050d04889f21c529d))
    - Build: Add and use disable-deploy matrix var ([`59f9adc`](https://github.com//sharkdp/bat/commit/59f9adc70610a7a30f5c267f2f8f7816e21b1f09))
    - Fix theme dir hint in --list-themes output ([`fed30b1`](https://github.com//sharkdp/bat/commit/fed30b1b36e4197beafceb09da1d6273fb0c30a6))
    - Build: Remove unused JOB_DO_TESTING var and output ([`411d68e`](https://github.com//sharkdp/bat/commit/411d68e8396e92160546769fe5ae47b9964c9882))
    - Build: Remove unused PKG_suffix output var ([`77d42a1`](https://github.com//sharkdp/bat/commit/77d42a17c6d0744c6b6984f01ea95d3213fc9758))
    - Build: Remove unused TARGET_* vars and outputs ([`e7c55bf`](https://github.com//sharkdp/bat/commit/e7c55bffe9349b67823f195482fa3efc6f5b1321))
    - Merge remote-tracking branch 'origin/master' into fix-1063 ([`da10166`](https://github.com//sharkdp/bat/commit/da1016662588cae98aeb96b2ec570b14f02f9c62))
    - Build: Use matrix.job.use-cross directly ([`b6b7262`](https://github.com//sharkdp/bat/commit/b6b7262962ab67d34ad98788aff6ed78f1e763e3))
    - Build: Remove unused REF_* outputs ([`3ed8391`](https://github.com//sharkdp/bat/commit/3ed83913b2163b43d58991992c064302d7bfd230))
    - Build: Remove conditional TOOLCHAIN logic ([`63460f4`](https://github.com//sharkdp/bat/commit/63460f4bf94ea5a8706272faba507fd645d0d07d))
    - Build: Enable x86_64-pc-windows-gnu again ([`c67b439`](https://github.com//sharkdp/bat/commit/c67b43975248c0ad3e4e699623f05f472fc48a9a))
    - Update CHANGELOG ([`2b44940`](https://github.com//sharkdp/bat/commit/2b44940f81cb2bfcf9747a0ad23d1f1a5545aea7))
    - Add --diagnostics alias ([`26136be`](https://github.com//sharkdp/bat/commit/26136be9037e246b1d2cc5503c760e6e2ebb1491))
    - Update to bugreport 0.3.0 ([`6a52f69`](https://github.com//sharkdp/bat/commit/6a52f69b5816819ae3d44f231e236e6249c4eff7))
    - Exhaustive list of relevant environment variables ([`60406c7`](https://github.com//sharkdp/bat/commit/60406c7c2d581cb9110e02e7388e405afc831557))
    - Bump bugreport version ([`2465438`](https://github.com//sharkdp/bat/commit/2465438ec32cffa03a0ce3efd04c4565937cc346))
    - Add --diagnostic run to CI ([`99a6158`](https://github.com//sharkdp/bat/commit/99a61580e1c5c290e1e080b059cf29c55c381a6f))
    - Add config file, compile time info and less version ([`dec94b4`](https://github.com//sharkdp/bat/commit/dec94b4111ea7e484df6ce52041c0a86c3f5e713))
    - Add --diagnostic option to bat ([`ebb97e9`](https://github.com//sharkdp/bat/commit/ebb97e94a9f3936e7db23c211f9dba779b0daa89))
    - Code Coverage: Explain why disabled ([`5e1f9fa`](https://github.com//sharkdp/bat/commit/5e1f9fadf4d9a45c39e41ebc0deb567fbc0427fd))
    - Code Coverage: Disable for now ([`fe08de8`](https://github.com//sharkdp/bat/commit/fe08de846de71534713fee43902b6fa044cb8dfc))
    - Fix unused import warning ([`35e3c51`](https://github.com//sharkdp/bat/commit/35e3c51b75c659690276b74cb97fe684a5d34699))
    - Fix clippy warnings in tests on Windows ([`59c9105`](https://github.com//sharkdp/bat/commit/59c9105c2542f19633eaa8294c2549912d2becca))
    - Remove use-cross when host == target ([`78aed2c`](https://github.com//sharkdp/bat/commit/78aed2cb69ab0bddeb22dcc5fbf3f44b98691e4d))
    - Merge remote-tracking branch 'origin/master' into fix-1063 ([`46487b2`](https://github.com//sharkdp/bat/commit/46487b201ff0f4cdcbfdea9d328cbac6d29a22d1))
    - Code Coverage: Use matrix.job.toolchain directly ([`caf0743`](https://github.com//sharkdp/bat/commit/caf0743811b3b38eab353ddf8ba7c8836f049b12))
    - Add integration test for unparsable pager ([`9ccb667`](https://github.com//sharkdp/bat/commit/9ccb667653ca2e8e6243becc9312ca30dc2ce989))
    - Add unix specific integration test about running bat without any arguments ([`b600f62`](https://github.com//sharkdp/bat/commit/b600f62ab68e0a11a9bb69dcf45648632d3f52ee))
    - Add integration tests for clircle cycle detection ([`ace655e`](https://github.com//sharkdp/bat/commit/ace655e164a4ce383cc5b6664e8cf5e360ddae35))
    - Add cycle detection with clircle, now v0.2 ([`21ae26c`](https://github.com//sharkdp/bat/commit/21ae26cb17ad0dfe815ce001be84f40078c5a4a7))
    - Merge remote-tracking branch 'origin/master' into fix-1063 ([`478233f`](https://github.com//sharkdp/bat/commit/478233f7952d1c634258a76589d2f8151d7c51c0))
    - Make mocked pagers work on Windows ([`e87c554`](https://github.com//sharkdp/bat/commit/e87c554ccd0d4eb22794e1d193b8911b1d11edb1))
    - Run PATH-dependent tests serially ([`df33ed0`](https://github.com//sharkdp/bat/commit/df33ed05dd7ebd153f532bd38be2043f7b14d3be))
    - Add integration tests for 'more' and 'most' used as pagers ([`c9efdd6`](https://github.com//sharkdp/bat/commit/c9efdd68edd6fede17ff21cac1d8ce0f826194f2))
    - Update formatting ([`8381945`](https://github.com//sharkdp/bat/commit/8381945cb59aac45e4a60c597f114dbf0102cfcf))
    - Move "diagnostics" part to the bottom of the bug report template ([`2046b47`](https://github.com//sharkdp/bat/commit/2046b47739b661efaa10f1a7a2c674990fb3bb55))
    - Update bug_report.md ([`0c302f0`](https://github.com//sharkdp/bat/commit/0c302f088a35179dd488fc39e28e23f49aa01051))
    - Update bug_report.md ([`a41db63`](https://github.com//sharkdp/bat/commit/a41db63907401a7ab8b4310e4e5a33a5e1cc7b8c))
    - bug_report.md: improve wording ([`3573c48`](https://github.com//sharkdp/bat/commit/3573c48e98e1698005cbf8ad5eeb58b3dcdaf994))
    - bug_report.md: move environment prompt back up ([`e94980b`](https://github.com//sharkdp/bat/commit/e94980bfd0d6eaff577b8e0a0cae42354e846c99))
    - Update bug_report.md ([`5422982`](https://github.com//sharkdp/bat/commit/54229822079ca681a382dd8dc577ba88d9989258))
    - Merge pull request #1268 from eth-p/improved-benchmark ([`4fe5497`](https://github.com//sharkdp/bat/commit/4fe5497d19914c6d912b3cc4586134981b186b3b))
    - Svelte syntax test: rename license file ([`b5bdba8`](https://github.com//sharkdp/bat/commit/b5bdba8b16666939f16d3e33372db88193bcd269))
    - DotEnv syntax test: add missing newline ([`522c97f`](https://github.com//sharkdp/bat/commit/522c97f5ad68349316f4380a43d37b229eba07a0))
    - Add Zig entry in ChangeLog, see #1470 ([`7fbb3a5`](https://github.com//sharkdp/bat/commit/7fbb3a53523dc92d71040fa836a2a4948f392573))
    - Merge branch 'master' into improved-benchmark ([`1b549ec`](https://github.com//sharkdp/bat/commit/1b549ecc66b4799a8d15a2b69177a866eb794af5))
    - Update Zig syntax test output ([`b3f3452`](https://github.com//sharkdp/bat/commit/b3f34529b5122743f13642c8c4a63008719c2b6f))
    - Delete erroneously added Sublime file ([`d388d07`](https://github.com//sharkdp/bat/commit/d388d07e9fa4b919905b706f5b56b9cacbdcc675))
    - Add support for Zig ([`c76e278`](https://github.com//sharkdp/bat/commit/c76e27851cfea1563ed8ff4b55b989706f433f35))
    - Update syntax test instructions ([`8c0dcf3`](https://github.com//sharkdp/bat/commit/8c0dcf3b57b2c656e869dfe88a9b4137fe67f9e5))
    - Add step-by-step guide to add syntax tests, see #1211 ([`962b3a7`](https://github.com//sharkdp/bat/commit/962b3a78c0891ede2cfb93c9e026d57684668a24))
    - Add ChangeLog entry ([`aa5b941`](https://github.com//sharkdp/bat/commit/aa5b941ed5e7f34743e13dcc5e38082dde687d9f))
    - Add a Lean highlighting test file. ([`af8a803`](https://github.com//sharkdp/bat/commit/af8a8035e82dfd1e0c6a986d3d2bf5bfd7392e22))
    - Add the Lean submodule. ([`037a66c`](https://github.com//sharkdp/bat/commit/037a66c57b70174cf9f98fe6f6a99bb4df817261))
    - Add Lean.sublime-syntax. ([`1a04dcf`](https://github.com//sharkdp/bat/commit/1a04dcf10f502798c7907cb1e1da37d5b8955eb8))
    - Fix repology badge ([`2eae8b5`](https://github.com//sharkdp/bat/commit/2eae8b578edb21918b0417a8d4cc334dfd13fb48))
    - Bump assets/themes/zenburn from `cb746f6` to `7f6fb86` ([`17189fc`](https://github.com//sharkdp/bat/commit/17189fce9b384bfc971dc61bd335d197c9e65b6b))
    - Bump assets/syntaxes/02_Extra/TypeScript from `603ebb4` to `a607ddf` ([`a022501`](https://github.com//sharkdp/bat/commit/a0225018e645d04823363c33aa5cf48fe6d839a9))
    - Bump console from 0.13.0 to 0.14.0 ([`361b7aa`](https://github.com//sharkdp/bat/commit/361b7aa0daca527ec56062fc061ecdd48494efc4))
    - Bump syntect from 4.4.0 to 4.5.0 ([`3345909`](https://github.com//sharkdp/bat/commit/334590932af301f5319fb8f27544bdef88dc8aca))
    - Bump git2 from 0.13.12 to 0.13.15 ([`3f46382`](https://github.com//sharkdp/bat/commit/3f4638204f06069b805fc078df19c110dcec1236))
    - Bump predicates from 1.0.5 to 1.0.6 ([`5e7061b`](https://github.com//sharkdp/bat/commit/5e7061b9f6eb3dbdcdd6a5f596856423a569ee82))
    - Bump serde from 1.0.117 to 1.0.118 ([`7a1cd52`](https://github.com//sharkdp/bat/commit/7a1cd5226fd6311a63f040fcf1b7889e928cf304))
    - Bump assets/syntaxes/02_Extra/Julia from `6c0d770` to `e2b1cb5` ([`285ac75`](https://github.com//sharkdp/bat/commit/285ac7573857d54328c2af6232db68e17b4389e0))
    - Bump assets/syntaxes/02_Extra/SCSS_Sass from `bc6332c` to `4868322` ([`00ff54b`](https://github.com//sharkdp/bat/commit/00ff54be4e0c853ef9591bc26da605e25f4ff951))
    - Bump assets/themes/dracula-sublime from `26c57ec` to `c2de0ac` ([`aab35e3`](https://github.com//sharkdp/bat/commit/aab35e3faac53cc52a3783c11cec08a09fc92a35))
    - Also replace 'more' from PAGER with 'less' ([`bfa5342`](https://github.com//sharkdp/bat/commit/bfa5342331cc17508ec2c528f2369514221ad3e4))
    - Allow clippy::style lints ([`c0d945c`](https://github.com//sharkdp/bat/commit/c0d945c0acbd6dd75971f6ee04a8b294471ead7f))
    - When PAGER=most, don't print a warning to stderr, silently use less instead ([`22bdc7c`](https://github.com//sharkdp/bat/commit/22bdc7c20f21108ddc7be18775a8050059459ebf))
    - Improve benchmark script to support cargo/config target-dir ([`3ed0081`](https://github.com//sharkdp/bat/commit/3ed0081f1fb4b707af026fcf1789909114487975))
    - Add .resource extension for Robot Framework ([`6d0e765`](https://github.com//sharkdp/bat/commit/6d0e7650c3192b60a5f5bc90b67d652b8cd89842))
    - Merge remote-tracking branch 'origin/master' into fix-1063 ([`552545f`](https://github.com//sharkdp/bat/commit/552545fe5f807f7fca51ab5a52cebcd15dd0021a))
    - Simplify and polish pager.rs and related code ([`dcfe883`](https://github.com//sharkdp/bat/commit/dcfe883f4be477dfe2ef3cef254d9446048a3bfe))
    - CHANGELOG.md: Add bugfix entry for #614 ([`f45fa5e`](https://github.com//sharkdp/bat/commit/f45fa5e18718140a0892e4e662c8715e1f1a073c))
    - bat cache --build: Print syntect error message for themes if any ([`05e9da3`](https://github.com//sharkdp/bat/commit/05e9da390fbb9a595b15df4860f61efde81ea9d2))
    - Merge remote-tracking branch 'origin/master' into fix-1063 ([`cc0f8ca`](https://github.com//sharkdp/bat/commit/cc0f8ca813b2d820e85a9fa3e42b0aded244fef8))
    - bat --list-languages: remove unnecessary format!() call ([`9c16571`](https://github.com//sharkdp/bat/commit/9c1657134736b9deb1563d5d6cabe8f06904a8aa))
    - Only enforce 'correctness' lints, just print the rest ([`2765c6b`](https://github.com//sharkdp/bat/commit/2765c6ba3b7c7bf3b2cbb42516ebc2798cd36722))
    - Add Clippy linter step to CICD ([`28f3f3c`](https://github.com//sharkdp/bat/commit/28f3f3c9c9e5a39bec69a0cd92e21eaf7779ebb7))
    - Update CHANGELOG ([`73d9a95`](https://github.com//sharkdp/bat/commit/73d9a958620797724b645cf389f0696269ff23ba))
    - Update VimL syntax upstream ([`9c9a9ac`](https://github.com//sharkdp/bat/commit/9c9a9ac4527c6a50119c6ba216c7403025cfcc0f))
    - Improve VimL regex highlighting ([`29ea396`](https://github.com//sharkdp/bat/commit/29ea396c311d9f1992c5e876752f2c50f05316c2))
    - Add case from #1604 to VimL syntax-test source ([`171d215`](https://github.com//sharkdp/bat/commit/171d215f910cd21e6ec07eac20a05b0403444b48))
    - Add syntax highlighting tests for VimL ([`cc6109a`](https://github.com//sharkdp/bat/commit/cc6109a751d9a41707991cc80827534a0459ba26))
    - Introduce bat_warning! helper macro ([`47bb4a9`](https://github.com//sharkdp/bat/commit/47bb4a9c0f3e8cf5af92ce5741f9f0c3d810cf45))
    - Cargo.toml: remove "readme" field ([`b149ea9`](https://github.com//sharkdp/bat/commit/b149ea91ddfd741a9c8012fded1770a69ca92995))
    - Fix clippy::if_same_then_else warning in --paging=auto logic ([`6d1c7d5`](https://github.com//sharkdp/bat/commit/6d1c7d5f57fbccd54499b003c94528a5188d281c))
    - Update CHANGELOG.md ([`07bd750`](https://github.com//sharkdp/bat/commit/07bd7503c530a992f28400f95ea7b3bcc42ab29e))
    - Move changelog entry to unreleased ([`1b5af89`](https://github.com//sharkdp/bat/commit/1b5af89ddd1683cde6b1936db020036f0a532954))
    - Use Briles/gruvbox .tmTheme files instead of peaceant's port ([`3d07dec`](https://github.com//sharkdp/bat/commit/3d07dec8fdd42e00f659f7c606562e3a51a58a55))
    - Add test 'grid_for_file_without_newline' (for issue #299 fix) ([`a63bb08`](https://github.com//sharkdp/bat/commit/a63bb08edae661f68fc330549c27d0d43355d606))
    - Fix test ([`e0207f8`](https://github.com//sharkdp/bat/commit/e0207f8167eeeda3ef8dc7454578cdcd4dce8aea))
    - Add ansi theme to replace ansi-light and ansi-dark ([`3099f51`](https://github.com//sharkdp/bat/commit/3099f51ba7a19596f084699eeb1b69305a88b9e4))
    - Add changelog entry ([`19e7763`](https://github.com//sharkdp/bat/commit/19e7763f3577645bfe34d09e1c832d2bc713d5a9))
    - Remove commented-out line from Cargo.toml ([`73cff42`](https://github.com//sharkdp/bat/commit/73cff42ec9529a12376a3e5bdcf94e3533bb611a))
    - Remove repeated `contains` calls ([`b349155`](https://github.com//sharkdp/bat/commit/b349155f2f959eeeb339824830ec205ff8642295))
    - Highlight 'batcat' note in README, see #1420 ([`ab4c120`](https://github.com//sharkdp/bat/commit/ab4c120ea534145202270395824f50f0fba4655b))
    - Update CHANGELOG.md ([`947133d`](https://github.com//sharkdp/bat/commit/947133d2950439e03d3fa40efcb92766c4ef3b01))
    - make bat -L use plain style ([`bf96e6e`](https://github.com//sharkdp/bat/commit/bf96e6e642ce3d94b0eacf8838e75a765525691c))
    - make bat -L use pager ([`cffacad`](https://github.com//sharkdp/bat/commit/cffacad306c11f8d64208cf96e45be5df7f83759))
    - Merge pull request #1440 from Enselic/fix-1438-newline-can-be-added-even-if-style-plain ([`cc7b89f`](https://github.com//sharkdp/bat/commit/cc7b89faf822d773a5ec74f3c80320ff06863af8))
    - change docs dir name to match package name ([`c1e4746`](https://github.com//sharkdp/bat/commit/c1e4746d509b5d5b3a08d88cc69833b77116e72b))
    - include changelog in package ([`8331eec`](https://github.com//sharkdp/bat/commit/8331eec7fcf6d05a52c3318766f0cbde5e05df5e))
    - include years in copyright notice ([`0547068`](https://github.com//sharkdp/bat/commit/0547068ed0bee466bd1e1f8931c0f6d7ebf8ddb2))
    - ensure copyright is mode 644 ([`18d8389`](https://github.com//sharkdp/bat/commit/18d8389785177d6fc0581f7c79117db0bedf9d61))
    - remove leading article from description ([`165d25d`](https://github.com//sharkdp/bat/commit/165d25d941b791876f27c3fcc565e9d89e8df947))
    - invoke gzip with -n ([`c4fb77b`](https://github.com//sharkdp/bat/commit/c4fb77b04294b3145d01c801b07b7962e7f2727d))
    - warnings of ShellCheck ([`973ea98`](https://github.com//sharkdp/bat/commit/973ea984c8708f477573d9f3ffcf7387dd92f4ef))
    - Add integration test for nonexisting newline ([`3e9afe2`](https://github.com//sharkdp/bat/commit/3e9afe297446e449f3b05df32d4731c78ee14ce3))
    - Merge branch 'master' into fix-1438-newline-can-be-added-even-if-style-plain ([`73c1657`](https://github.com//sharkdp/bat/commit/73c16574e6705a7ddbfa0438d938eab485ee3d75))
    - Merge pull request #1439 from rsteube/themes-help-interactive-only ([`2d22c70`](https://github.com//sharkdp/bat/commit/2d22c705baac9ed1161c9050813475904b8232d9))
    - Don't add artificial newline to last line if --style=plain ([`68d525c`](https://github.com//sharkdp/bat/commit/68d525cd8b350b40e2c990c70625eba4e82f746d))
    - replace_nonprintable: Keep \n around ([`c3fc1b8`](https://github.com//sharkdp/bat/commit/c3fc1b88fea5e4f4aee2e02ee7426180031a0081))
    - only print themes hint in interactive mode ([`84b0702`](https://github.com//sharkdp/bat/commit/84b070239951a1fff778812393b670cc5c06244d))
    - add packaging in unreleased section ([`f3489ff`](https://github.com//sharkdp/bat/commit/f3489ffa29e8851a289dd86e7f67d504cb3223a2))
    - update changelog file ([`8e6ba2a`](https://github.com//sharkdp/bat/commit/8e6ba2a3e17e1fdd653f44a8047803644a0f53b0))
    - changing snap name to batcat ([`48e799e`](https://github.com//sharkdp/bat/commit/48e799e7a6da71a9d7ee14ed69bc9277d026ca07))
    - update chagelog.md ([`bc01999`](https://github.com//sharkdp/bat/commit/bc01999f0d6285bc816f72cc9b2b8a39e59a441b))
    - add snap installtion command ([`b0fcb30`](https://github.com//sharkdp/bat/commit/b0fcb3065f927d998b2a567eb962b47f6baf825a))
    - Minor fixes in CONTRIBUTING.md ([`94d8cd5`](https://github.com//sharkdp/bat/commit/94d8cd51002b80442ecfc8c3c29063b7d1dc2751))
    - Bump assets/syntaxes/02_Extra/ssh-config from `1ddcb32` to `201816b` ([`5596c61`](https://github.com//sharkdp/bat/commit/5596c6133b9e9f5471f3178ec9e566431a8645c9))
    - Bump assets/syntaxes/02_Extra/GraphQL from `c9d8458` to `59304d6` ([`304332e`](https://github.com//sharkdp/bat/commit/304332e380500c43f786784204a74fba9655cb7f))
    - Bump assets/syntaxes/02_Extra/Svelte from `bf92f5b` to `aee0676` ([`fb989d8`](https://github.com//sharkdp/bat/commit/fb989d8c94eb84fad1066b38ff523983f3d20b68))
    - Bump assets/syntaxes/02_Extra/AWK from `e593eb6` to `e23926e` ([`026220b`](https://github.com//sharkdp/bat/commit/026220b96039abff5e25aefa07dfb9a912218591))
    - Bump assets/themes/Coldark from `b4a1c74` to `e44750b` ([`543a253`](https://github.com//sharkdp/bat/commit/543a253bc15ff9581d0b3e510d0c4f0f8580a57d))
    - Bump assert_cmd from 1.0.1 to 1.0.2 ([`8f8e44e`](https://github.com//sharkdp/bat/commit/8f8e44e87aef5f1cb5f32fb041c3d6b48174c427))
    - Update with termux installation ([`2da1109`](https://github.com//sharkdp/bat/commit/2da11097f945a156a20a38bed345f2c4765dd78a))
    - only try to publish one set of archives/packages for linux x64 ([`d124ebe`](https://github.com//sharkdp/bat/commit/d124ebecede82a7e27ee86653d78911ebbdaf5c5))
    - Ignore PAGER=most by default with a warning to stderr ([`986d0e9`](https://github.com//sharkdp/bat/commit/986d0e9777a77b309ffc55de24fcb665adfa3bb7))
    - Add Pager helper with info about where the value comes from ([`f420236`](https://github.com//sharkdp/bat/commit/f4202361b4726bfceea429b5840c604622932807))
</details>

# v0.17.1 (2020-11-24)

## Bugfixes

- Running `bat` without arguments fails ("output file is also an input"), see #1396


## Commit Statistics

<csr-read-only-do-not-edit/>

 - 3 commits contributed to the release.
 - 0 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' where seen in commit messages

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Bump version to 0.17.1 ([`6d98149`](https://github.com//sharkdp/bat/commit/6d981498d80aa0127df9547ec0ac69d8314b29e5))
    - Revert "Add cycle detection integration tests" ([`bd2e991`](https://github.com//sharkdp/bat/commit/bd2e9917da5cf5b4f7bae48ef5b97ce5cead2803))
    - Revert "Add io cycle detection with clircle" ([`a6810e3`](https://github.com//sharkdp/bat/commit/a6810e3353db8536015b830b28e45e5e0167fdac))
</details>

# v0.17.0 (2020-11-23)

## Features

- Added a new `--style` value, `rule`, which adds a simple horizontal ruled line between files, see #1276 (@tommilligan)
- Pass `-S` ("chop long lines") to `less` if `--wrap=never` is set in `bat`, see #1255 (@gahag)

## Bug Fixes

 - <csr-id-5d490664ca3613965fdf01038b9b29305c5273b5/> update original file in license

## New Features

 - <csr-id-a3f037773a9ba9dfa21675bc745965864cac9ee8/> include dotfiles (.) in create_highlighted_versions.py search
 - <csr-id-c71c8980cf4a7a917d9fb1712985d3abd48ef54b/> add syntax sample for dotEnv files
 - <csr-id-3c756a65a61219d7a68aa1551dbda45b5464a030/> add syntax tests for sass
 - <csr-id-b83716f0ebc211924dbb11bcabcbba88373c9085/> add Nim syntax test file

## Bugfixes

- Detect infinite loop when input and output are the same, see #1193 and #1197 (@niklasmohrin)
- Throw an error when `bat` is being used as `pager`, see #1343 (@adrian-rivera)
- Bash syntax highlighting not selected for `*.ebuild` and `*.eclass` files, see #1292 (@sharkdp)
- Fix `zsh` completion when using `-p`, see #1320 (@xzfc)

## Other

- Add note to refer to see detailed help with `--help` (and vice versa with `-h`), see #1215 (@henil)
- Add a `Contributors` section to `README`, see #1348 (@adrian-rivera)

## Syntaxes

- Manpage syntax highlighting has been improved, see #1315 (@keith-hall)
- Add Svelte file syntax, see #1285 (@kjmph)

## New themes

- Coldark, see #1329 (@armandphilippot)


## Commit Statistics

<csr-read-only-do-not-edit/>

 - 237 commits contributed to the release over the course of 51 calendar days.
 - 17 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 2 unique issues were worked on: [#1213](https://github.com//sharkdp/bat/issues/1213), [#1293](https://github.com//sharkdp/bat/issues/1293)

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **[#1213](https://github.com//sharkdp/bat/issues/1213)**
    - Add syntax highlighting test file for INI ([`975487f`](https://github.com//sharkdp/bat/commit/975487f6303c59236bd779a534f292ec1c340ff2))
 * **[#1293](https://github.com//sharkdp/bat/issues/1293)**
    - Integrate README.md with new assets.md ([`f80fba4`](https://github.com//sharkdp/bat/commit/f80fba4d24fddbac4f481c6639f7b334dec149a1))
 * **Uncategorized**
    - add new syntax test files ([`591eba6`](https://github.com//sharkdp/bat/commit/591eba66a3af1c211a09eefd17cbeb819b0a131f))
    - Revert "Update dependencies" ([`a5a9ac8`](https://github.com//sharkdp/bat/commit/a5a9ac83e5a506ac2bdc673476aca54c818eabe2))
    - Update syntax cache ([`15b122a`](https://github.com//sharkdp/bat/commit/15b122a448955391e61011344a2cca9f2c89b891))
    - Update dependencies ([`44a905d`](https://github.com//sharkdp/bat/commit/44a905d1358fff573fe59ec43be8b15df19fd768))
    - Bump version to v0.17.0 ([`8884104`](https://github.com//sharkdp/bat/commit/8884104e828a2abebf19ab42837527d38e924f83))
    - Revert "invoke gzip with -n" ([`277cc5f`](https://github.com//sharkdp/bat/commit/277cc5fa2120e3801c4cf2df2d0be08a8c9effd3))
    - Revert "remove leading article from description" ([`0cbd7d5`](https://github.com//sharkdp/bat/commit/0cbd7d583c49d1629bde72fe6afb1abf8065f1f5))
    - Revert "ensure copyright is mode 644" ([`3c3fc92`](https://github.com//sharkdp/bat/commit/3c3fc92863ad93748f703bcbaa141bd9fe4acdb6))
    - Revert "include years in copyright notice" ([`e69d650`](https://github.com//sharkdp/bat/commit/e69d65059838a6c0827798c9e0dff701329c8918))
    - Revert "include changelog in package" ([`9385c81`](https://github.com//sharkdp/bat/commit/9385c81882cefbca714f1b8431631a8ac6784200))
    - Revert "add Depends line" ([`1b84f9f`](https://github.com//sharkdp/bat/commit/1b84f9f1ae3eb83405b9fc5d1920d2ccb67a923c))
    - Revert "change docs dir name to match package name" ([`d879fbd`](https://github.com//sharkdp/bat/commit/d879fbd8a1ae91cd3a0cbd63932bf70815e5bf28))
    - Use unreachable!(…) ([`1fbdef0`](https://github.com//sharkdp/bat/commit/1fbdef06aae65927e27a076b18ca922f0901c59e))
    - remove all matches ([`f5531cc`](https://github.com//sharkdp/bat/commit/f5531cc7faabe75f9b1460446e5249ad1f839911))
    - remove unuseful matches expression ([`2ccff14`](https://github.com//sharkdp/bat/commit/2ccff145ed67044e808dc8b1d27fe0d0329d06af))
    - fix some clippy warnings ([`43b2ee5`](https://github.com//sharkdp/bat/commit/43b2ee5e71f70b8f1f805d2c45331807ef20bafe))
    - Change Robot syntax name to 'Robot Framework' ([`51463a4`](https://github.com//sharkdp/bat/commit/51463a4b418161cc800e082a32974480e640e91b))
    - Add cycle detection integration tests ([`4f0116b`](https://github.com//sharkdp/bat/commit/4f0116bee7873033f813d90d86275696c697eae9))
    - Add io cycle detection with clircle ([`31793cf`](https://github.com//sharkdp/bat/commit/31793cfa6287cb90cc12eb4fbd0c76bd496e91a5))
    - Merge pull request #1389 from nstickney/master ([`6d75540`](https://github.com//sharkdp/bat/commit/6d75540d4e95c57ba89d5810d271bc687761763e))
    - explain how to use bat as cat (no paging) ([`ba5cbe2`](https://github.com//sharkdp/bat/commit/ba5cbe208ff7d9397950be42e9f11f2051d57966))
    - Update bug_report.md ([`2b82203`](https://github.com//sharkdp/bat/commit/2b82203041e0a4242659b95a4fa2ae6454497637))
    - Add zsh completion fix to CHANGELOG ([`35da065`](https://github.com//sharkdp/bat/commit/35da065445c7b36167ae6b64e4281816aa9a8b1d))
    - Bump git2 from 0.13.11 to 0.13.12 ([`12a1fe3`](https://github.com//sharkdp/bat/commit/12a1fe3ad417f7694c4490b94b793387c7a7b536))
    - Bump assets/syntaxes/02_Extra/Terraform from `64208ea` to `54d8350` ([`91057f2`](https://github.com//sharkdp/bat/commit/91057f2a096ad1b222b4d6a20a545ef4cd7d3ad6))
    - Bump assets/syntaxes/02_Extra/Elixir from `4ee539b` to `4fb0189` ([`15060f0`](https://github.com//sharkdp/bat/commit/15060f00a520d5e14708eff259d2bb10ef702e4d))
    - Bump assets/themes/onehalf from `3aa42a3` to `8992311` ([`1a82a03`](https://github.com//sharkdp/bat/commit/1a82a03d5dbf0ca59a88734255939f7ff19a4783))
    - Bump assets/syntaxes/02_Extra/CMake from `aba96a0` to `21e9698` ([`4674105`](https://github.com//sharkdp/bat/commit/4674105698d7ee9a96d4cae4361635287b8e4989))
    - Bump serde from 1.0.116 to 1.0.117 ([`ca3e9a3`](https://github.com//sharkdp/bat/commit/ca3e9a385fb972efe5e7c7286e933bd48f48fce9))
    - Bump serde_yaml from 0.8.13 to 0.8.14 ([`f05c222`](https://github.com//sharkdp/bat/commit/f05c2222217da013dc9f6cad4d9234a013d1ad76))
    - Import syntax regression test for Ruby Haml ([`3dc5fc5`](https://github.com//sharkdp/bat/commit/3dc5fc54dff6c6819d2f02c5d52934673d2b98d7))
    - Add sshd_config syntax highlighting test ([`bac84b4`](https://github.com//sharkdp/bat/commit/bac84b4c8bbb72440423890ffb8221c49c11a986))
    - Bump console from 0.12.0 to 0.13.0 ([`a5b9a80`](https://github.com//sharkdp/bat/commit/a5b9a80216fe9fa2bfc586ab02cb1635966c4581))
    - Bump globset from 0.4.5 to 0.4.6 ([`142d5e6`](https://github.com//sharkdp/bat/commit/142d5e60ec24bddf29b23d22d63f9c8a30f83370))
    - Merge pull request #1276 from tommilligan/style-rule ([`61f947a`](https://github.com//sharkdp/bat/commit/61f947a580f385f66e96d60e2458e89f06940113))
    - Highlight ebuild/eclass files with shell syntax ([`c4bbf28`](https://github.com//sharkdp/bat/commit/c4bbf2886240148979f65d88f1adf548c3e5be4d))
    - Merge pull request #1331 from priner/master ([`9befa81`](https://github.com//sharkdp/bat/commit/9befa81e36de97475e324931061150e3d97da9c1))
    - Fix trailing newline ([`414c7ac`](https://github.com//sharkdp/bat/commit/414c7ac83d759b17fd6fe579c037b0429de0c16a))
    - Merge branch 'master' into master ([`3a612f0`](https://github.com//sharkdp/bat/commit/3a612f063eb157a35b4d09d4dcf2b04ed1604388))
    - Add Ninja highlight test ([`9daea73`](https://github.com//sharkdp/bat/commit/9daea73bbab6e3f4ad6761bcf9702cc7a9811489))
    - Use platform based path concatenation ([`8e81315`](https://github.com//sharkdp/bat/commit/8e8131590ca9922c996c33ad7a5076f57a619e42))
    - Add extra themes help to output ([`3acfe79`](https://github.com//sharkdp/bat/commit/3acfe790b94bac518d19cb75540ba56a83976d36))
    - Add a new 'Feature request' section ([`877d912`](https://github.com//sharkdp/bat/commit/877d9120c90743d9f837e45bf89cf5f2ee50fb11))
    - Add new section on Development and New syntaxes/themes ([`c15b88f`](https://github.com//sharkdp/bat/commit/c15b88f250a1a47f2042fab040970ebdec23b286))
    - More detailed description for CHANGELOG section ([`2889e01`](https://github.com//sharkdp/bat/commit/2889e017cd05c02628ed47f253d210de832942cc))
    - Style changes on CONTRIBUTORS ([`47ea990`](https://github.com//sharkdp/bat/commit/47ea99090f3d817d4a677195b7d2dcb86e7e831e))
    - Adds a CONTRIBUTORS file ([`1c1ba04`](https://github.com//sharkdp/bat/commit/1c1ba04c41376a823167a56f888d04f9f4145c54))
    - Update CHANGELOG.md ([`456216a`](https://github.com//sharkdp/bat/commit/456216ab379c69947240f4f644bc9801614094d8))
    - From a TODO in #1211. Add documentation for contributors. ([`49d7bf8`](https://github.com//sharkdp/bat/commit/49d7bf8c3908262115e15830111bd6ae511b9e84))
    - Add reference to batdiff ([`dade4cc`](https://github.com//sharkdp/bat/commit/dade4cc6f36075e6a9872220cb640be049382e6c))
    - Updates CHANGELOG entry ([`c747ac1`](https://github.com//sharkdp/bat/commit/c747ac181d1843c30891f14bafbbd0e27478e0c2))
    - Throws an error when `bat` is being user as pager. ([`9837948`](https://github.com//sharkdp/bat/commit/9837948c3a7d59b0629ed5f176cd2861eb340af0))
    - Add Svelte as a syntax ([`072fb38`](https://github.com//sharkdp/bat/commit/072fb380d82df11e167d34b6a37dfe9fe1a2fb19))
    - Bring back delta to me to me ([`8559495`](https://github.com//sharkdp/bat/commit/85594956cfd0874849e08bf45320c98acb4adb59))
    - Add git diff example to Integration docs ([`88eba56`](https://github.com//sharkdp/bat/commit/88eba5660746e3caba334402398a66d340321ee6))
    - 1213: org mode highlight testing ([`cc6cf48`](https://github.com//sharkdp/bat/commit/cc6cf482563327957e09c1792e018d4862b08c5e))
    - 1213: nix; added trailing new line ([`e571cb5`](https://github.com//sharkdp/bat/commit/e571cb553b5dcdbb032e84a5d7a404cd35b5b934))
    - 1312: nix; fixed src ([`bf9f6e8`](https://github.com//sharkdp/bat/commit/bf9f6e872d2e93e47d58894b058470f27d5bd981))
    - 1213: nix; fix buildInputs arr ([`8348c1b`](https://github.com//sharkdp/bat/commit/8348c1b7468d03b5ccabc38175059873da393847))
    - add nix highligh ([`0a9744a`](https://github.com//sharkdp/bat/commit/0a9744ac95d37b4c1bd7008e08ff5d269432e4aa))
    - Update CHANGELOG ([`95954b7`](https://github.com//sharkdp/bat/commit/95954b7c57d117a13c49781e3eae9ddf989468a5))
    - Add Coldark in New Themes ([`a707f81`](https://github.com//sharkdp/bat/commit/a707f8172637dcae94667c346e9cdc3bc599312c))
    - Update themes.bin - Coldark themes ([`dae6664`](https://github.com//sharkdp/bat/commit/dae66645555580ad37be6b7d0fd9e12a7cbf2433))
    - Ignore 'all_themes_are_present' unit test by default ([`9dd8073`](https://github.com//sharkdp/bat/commit/9dd807344c55751654174f4685ea1b8dc957b3f2))
    - Add Coldark themes to unit test ([`2ecebf8`](https://github.com//sharkdp/bat/commit/2ecebf8ff2fa167efb0dce4e1ff692acde0d4a66))
    - Add Coldark themes ([`6d3cb5f`](https://github.com//sharkdp/bat/commit/6d3cb5f99f347c3c9cec727f936d961851a88650))
    - Add PowerShell syntax test ([`ec8072d`](https://github.com//sharkdp/bat/commit/ec8072d51fa943569ebdace5c79d9f8452cf2d8f))
    - Add Git Config syntax test file ([`7a7bb80`](https://github.com//sharkdp/bat/commit/7a7bb80e234e346dbee109cae7c443b1666f427f))
    - Add fairly complex nginx config. ([`99ac5c3`](https://github.com//sharkdp/bat/commit/99ac5c30456b01d70c35b7f35cdfe3a3c2213907))
    - Add Git Attributes syntax test ([`efab00a`](https://github.com//sharkdp/bat/commit/efab00a9de08266bd4d47d4d076e6e1c6c405fab))
    - Revert accidental regex literal -> unicode char change in Manpage syntax ([`9ab0801`](https://github.com//sharkdp/bat/commit/9ab0801ee19a59d9ffa96f8c3ab5804d6e3737b2))
    - End highlighting of C code in man pages when no longer relevant ([`cfdb853`](https://github.com//sharkdp/bat/commit/cfdb853ea60e8d6ff3eb56530079447777216d8b))
    - Highlight C code in all man pages sections ([`41e857e`](https://github.com//sharkdp/bat/commit/41e857ea1650e216932fca057b46fed5566e26a2))
    - #1213 expanded D test file ([`5b2da2b`](https://github.com//sharkdp/bat/commit/5b2da2b08d5f34ca8e0fdf133f081f9b97c7bf92))
    - #1213 added very basic test file for D ([`5e0a608`](https://github.com//sharkdp/bat/commit/5e0a608ea60e51d3115ce239042063c929d78a7f))
    - Add QML syntax test ([`3729aef`](https://github.com//sharkdp/bat/commit/3729aefb6fb659d6eda9051016798542f7e3d5ac))
    - Add test file for `/etc/group` ([`8a2122b`](https://github.com//sharkdp/bat/commit/8a2122b4a483a903fb286987e0f23d6e709882db))
    - Add tests for Vue component syntax ([`cc6f6fd`](https://github.com//sharkdp/bat/commit/cc6f6fdb1d524bcb9ccaab45675a2c6581f01d71))
    - Add Graphviz DOT example files ([`5ec4936`](https://github.com//sharkdp/bat/commit/5ec4936a4f60f604436a32995b4d9995cecbbf15))
    - Add MATLAB syntax test file ([`be84682`](https://github.com//sharkdp/bat/commit/be84682bcb1e5ef3aba7ccc355f32f9820aebc86))
    - Add Salt State (SLS) test file ([`b07f3b4`](https://github.com//sharkdp/bat/commit/b07f3b4e74b104b40b4b79e2a7094cd18b9ec1c2))
    - Add Ninja syntax highlighting ([`fc1f37d`](https://github.com//sharkdp/bat/commit/fc1f37d3df86616d8a99d70dc5d05fb637417915))
    - include dotfiles (.) in create_highlighted_versions.py search ([`a3f0377`](https://github.com//sharkdp/bat/commit/a3f037773a9ba9dfa21675bc745965864cac9ee8))
    - add syntax sample for dotEnv files ([`c71c898`](https://github.com//sharkdp/bat/commit/c71c8980cf4a7a917d9fb1712985d3abd48ef54b))
    - Fix zsh completion ([`6bf7b79`](https://github.com//sharkdp/bat/commit/6bf7b79f13b308d6b14d5dfb78ba348922c4cf5b))
    - Include details about Manpage syntax improvements in the changelog ([`5d9adc1`](https://github.com//sharkdp/bat/commit/5d9adc14e7c5ef15f9bf9d7c2405af22f6d7e978))
    - Fix Manpage C highlighting regression ([`575888d`](https://github.com//sharkdp/bat/commit/575888d81b3ecdb49dc31406d12053ab135f648e))
    - Add Manpage syntax regression test for select(2) ([`3539d3e`](https://github.com//sharkdp/bat/commit/3539d3e72da3fb0042e8c05a196d1f50a408c3cd))
    - Improve Manpage syntax ([`bb25111`](https://github.com//sharkdp/bat/commit/bb25111ca9ccb08d9d383b2bd7f816b3c6a105e7))
    - add syntax tests for sass ([`3c756a6`](https://github.com//sharkdp/bat/commit/3c756a65a61219d7a68aa1551dbda45b5464a030))
    - Added reStructuredText reference for syntax tests ([`8d57482`](https://github.com//sharkdp/bat/commit/8d574826c9a7a7b8d250eb95bb8fa0e34137811d))
    - Changed Makefile syntax test source ([`7c7eebb`](https://github.com//sharkdp/bat/commit/7c7eebbe8368e9ac605113af026a9f5eaf74090e))
    - Added Makefile to syntax tests ([`57ad858`](https://github.com//sharkdp/bat/commit/57ad85814b89bdcb3ee8114b7eb45460156d9914))
    - Add Common Lisp syntax highlighting test files ([`4b2b419`](https://github.com//sharkdp/bat/commit/4b2b419400834ba82aae957b73f273e68eb4a669))
    - add Nim syntax test file ([`b83716f`](https://github.com//sharkdp/bat/commit/b83716f0ebc211924dbb11bcabcbba88373c9085))
    - Fix small grammar mistake/typo in the readme ([`6872a4d`](https://github.com//sharkdp/bat/commit/6872a4dd7f972721fe9b9698ca9fc5612b197890))
    - Add PureScript syntax test ([`4d6b2fe`](https://github.com//sharkdp/bat/commit/4d6b2fec130331507f872b3683f8b2c8d3340dd5))
    - Formatting fix ([`57c1a7f`](https://github.com//sharkdp/bat/commit/57c1a7fd694a7fffcfa6d6d2071eb8d5aabcd186))
    - Minor README improvements ([`bf78b74`](https://github.com//sharkdp/bat/commit/bf78b74db4274361d2d6eb4746e1c77667b99255))
    - Make logo compatible with light and dark themes ([`d075d2b`](https://github.com//sharkdp/bat/commit/d075d2b453c51c66cc02c69c22d431ffd056a259))
    - Add ActionScript 3 syntax test ([`5650624`](https://github.com//sharkdp/bat/commit/5650624822cac9dc9ac40990d1c65b4f42182996))
    - Add Perl test file ([`bf07b0b`](https://github.com//sharkdp/bat/commit/bf07b0be58d4ae7d247de04c2f7a57a851664e7a))
    - Add apache httpd.conf example ([`eeea537`](https://github.com//sharkdp/bat/commit/eeea53759bc3dfce932e901ee6eec18e9fae546e))
    - added new syntax highlighting files ([`a39f1e0`](https://github.com//sharkdp/bat/commit/a39f1e0f9b63833b2d34ae96e0aa176338e0a0c7))
    - add integration test for grid overriding rule ([`1614f80`](https://github.com//sharkdp/bat/commit/1614f80366a1fcc344df157d04bb5284ef09ee4c))
    - update man pages ([`e293c58`](https://github.com//sharkdp/bat/commit/e293c58bb0d53c32b69b5fdcd9cc6a617bf75312))
    - disable rule when grid enabled, and print warning ([`3015ebf`](https://github.com//sharkdp/bat/commit/3015ebfba1c1e95f8fd664a7cd9c7cbbd7bc3dcf))
    - update changelog ([`d7da4f5`](https://github.com//sharkdp/bat/commit/d7da4f5f2b0206e6c36b14b54337751cd4c63f79))
    - Add integration test for style rule ([`c1af6ff`](https://github.com//sharkdp/bat/commit/c1af6ff2e6f7c596b381a70da8d37c7a28f764f0))
    - add component 'rule' for horizontal file delimiter ([`aa4000c`](https://github.com//sharkdp/bat/commit/aa4000cb0de7db2ad5fcf642e53627c4f53d46f9))
    - Use list.extend(…) ([`33128d7`](https://github.com//sharkdp/bat/commit/33128d75f22a7c029df17b1ee595865933551469))
    - Better ask forgiveness than permission ([`bf87f37`](https://github.com//sharkdp/bat/commit/bf87f37039c03fdb3be2bfd3e8286d0260023a49))
    - Formatting ([`62e715f`](https://github.com//sharkdp/bat/commit/62e715ff5b9640d8811c82c2198bf8257a397242))
    - Auto-format Rust code ([`b390317`](https://github.com//sharkdp/bat/commit/b3903175c8b5d72c0a9dc428d77bec01bdc349fa))
    - Added newline to multicharacter line of plaintext generation script ([`de47bd2`](https://github.com//sharkdp/bat/commit/de47bd2323d2b4a2e551022430c28e52274856d8))
    - Fixed file location in creation script ([`19b5570`](https://github.com//sharkdp/bat/commit/19b55706ee20cb0325304b6db37570c6614089ad))
    - Updated creation script and README to match generation script ([`6c68198`](https://github.com//sharkdp/bat/commit/6c68198d19f833abf283c812f0f268a31e5b7329))
    - #1244 Use [].copy and path.exists instead of more cumbersome solutions ([`f3c760c`](https://github.com//sharkdp/bat/commit/f3c760c25f5270424ded2fd55765da3d20d167ba))
    - Move syntax mapping section ([`de6680b`](https://github.com//sharkdp/bat/commit/de6680bdf2b01e30acd3d1720551868d5c051baf))
    - Small improvement to the syntax mapping section ([`0a28da0`](https://github.com//sharkdp/bat/commit/0a28da0cd018f2602677dcb04f8abef9aaabf523))
    - made requested changes ([`bce304a`](https://github.com//sharkdp/bat/commit/bce304a800e4f46eec9e202dc40036041193c63e))
    - fix a typo ([`206d3e7`](https://github.com//sharkdp/bat/commit/206d3e7b914a6ee4d3d9c53ccd71e74859ae111e))
    - add new section for sytax of file in README.md ([`4be32cf`](https://github.com//sharkdp/bat/commit/4be32cf746630e91ef291751cbf1590dd75fc97c))
    - minor update to specify pattern matching on --map-syntax supports glob matching ([`4ec6075`](https://github.com//sharkdp/bat/commit/4ec6075f81270a2f9d559aa12fcdd7ea0de03f44))
    - fix section ([`aa00e0a`](https://github.com//sharkdp/bat/commit/aa00e0a6b0d31072a5a6eea54d663d64ec48bb14))
    - add a new syntax section ([`bb7fae7`](https://github.com//sharkdp/bat/commit/bb7fae72c9f1a23ced3581edb447d4099d28f155))
    - add new --map-syntax example ([`7bd1964`](https://github.com//sharkdp/bat/commit/7bd19640a2bcb6e1eefbe8e48a373762099d4268))
    - updated the php commit ([`8bd078c`](https://github.com//sharkdp/bat/commit/8bd078caeb6a6376834009d4570f0b7f4fc1d714))
    - Revert "added php test file" ([`b7271a2`](https://github.com//sharkdp/bat/commit/b7271a2a6d1a4618d64178707f1c00c2f4d8f6e9))
    - Add MemInfo syntax test ([`51036ce`](https://github.com//sharkdp/bat/commit/51036ced9d2dfe8dd63e4d103560ac21e9352aaa))
    - Add CpuInfo syntax test ([`b6c1ec3`](https://github.com//sharkdp/bat/commit/b6c1ec3a2457096e06f404425d146d23a102d175))
    - added php test file ([`ecde418`](https://github.com//sharkdp/bat/commit/ecde418abbf869201ab83523cf9b52f859b456a1))
    - Merge pull request #1308 from okezieuc/add-php-file ([`24fe946`](https://github.com//sharkdp/bat/commit/24fe946c0630b8ec0a6831ffe07eb7dbac5f26bc))
    - Merge pull request #1307 from molcay/master ([`a838ff4`](https://github.com//sharkdp/bat/commit/a838ff47b5f5ec6153f3ff63087ee7ef3df24dd2))
    - Add Clojure syntax test. ([`4600400`](https://github.com//sharkdp/bat/commit/46004001cb366a8a3f06df882eb1ce015148b1c6))
    - Add GraphQl syntax test sample ([`f10c8ce`](https://github.com//sharkdp/bat/commit/f10c8ce25ea6ee40c587a4d0e1527f4846880058))
    - Add basic Dockerfile syntax highlighting test files ([`457ab84`](https://github.com//sharkdp/bat/commit/457ab84065416e3d9c0ff7dc4ea7f8902fb09eb6))
    - Add HTML syntax test sample ([`3bcb545`](https://github.com//sharkdp/bat/commit/3bcb5452aa91e527d04cbfc33f81395618db4959))
    - Added BibTeX syntax tests ([`1f0e53a`](https://github.com//sharkdp/bat/commit/1f0e53a44d1729cf3ce4ff397c7fd56e3d28fb6a))
    - Merge pull request #1305 from grg121/1213-dockerfile-syntax-highlighting-test ([`acc370d`](https://github.com//sharkdp/bat/commit/acc370d345245d6060ea0893e04d8780f29b5b20))
    - Remove gitattributes for .bat/.cmd files to make all testfiles eol=lf ([`2d5cf1d`](https://github.com//sharkdp/bat/commit/2d5cf1d3cff272c3edf82f546a4a54dcc3010a20))
    - Fix line endings ([`a81254e`](https://github.com//sharkdp/bat/commit/a81254ed2a5066483ef4705c4d2a32ade7d5464e))
    - Update test.swift ([`fdef133`](https://github.com//sharkdp/bat/commit/fdef133d3dc78fb5926a6a7d28ac2d3150863ef9))
    - ADDED swift test file for syntax highlighting ([`fd01f0a`](https://github.com//sharkdp/bat/commit/fd01f0a0ee275e39569ce4aed2a7e2377b148d69))
    - Fix line endings ([`5b095ed`](https://github.com//sharkdp/bat/commit/5b095ed6f38bd54aa9d49403232bffa9918f82c1))
    - ADDED test file for R ([`e7a3f34`](https://github.com//sharkdp/bat/commit/e7a3f34959f9c20344c44469581488784d3153b3))
    - add CMake syntax highlighting test ([`06d0c8f`](https://github.com//sharkdp/bat/commit/06d0c8f0565f8ad9bd4d78df1a33500e1178abf4))
    - Added test file for Erlang. ([`4064b8f`](https://github.com//sharkdp/bat/commit/4064b8ff145521c06989f122d9491385ad7dbb6f))
    - Update changelog ([`83ecc3d`](https://github.com//sharkdp/bat/commit/83ecc3d3ace271510e705bf5f58bf186e46a6e9d))
    - Code review: replace `if let` with equals operator ([`6615ece`](https://github.com//sharkdp/bat/commit/6615eceb185abbcb40279a564f17f074eae68762))
    - Code review ([`53f5a37`](https://github.com//sharkdp/bat/commit/53f5a37f01c0c354a552c50aa87a8edf38f9b9f0))
    - Add `-S` flag to less when `--wrap=never` (closes #1255) ([`bbef2f4`](https://github.com//sharkdp/bat/commit/bbef2f41ec0c670dcc54a78ac5b497a084c2557b))
    - Add Diff syntax highlight test ([`b6e729a`](https://github.com//sharkdp/bat/commit/b6e729abeb37306d70d791d79dcb4bcf03c2c299))
    - x86-64 Assembly NASM syntax test ([`5fe9868`](https://github.com//sharkdp/bat/commit/5fe98689d4301f43831f9a104b3be51b91ae068a))
    - add GLSL syntax highlighting test ([`8832a96`](https://github.com//sharkdp/bat/commit/8832a96e82cb7a1135d6188972f9634f21e83a4c))
    - Add Batch syntax test ([`09631ed`](https://github.com//sharkdp/bat/commit/09631ed119d94504f6701326b2321569890160c6))
    - Regenerate using a fresh latest build of bat ([`9257c7c`](https://github.com//sharkdp/bat/commit/9257c7ce208e5362ee07e0b4e1aeed21226bcb29))
    - Add sample Crystal file ([`b30f9a1`](https://github.com//sharkdp/bat/commit/b30f9a1677a4726b498828dfab1de2dcfc67c995))
    - Add Groovy syntax test. ([`e6c3f96`](https://github.com//sharkdp/bat/commit/e6c3f9693131db18a83cb971c7326798d0101c66))
    - Add Lua syntax test. ([`85ff81f`](https://github.com//sharkdp/bat/commit/85ff81f2388cd2ce10a65dfc3d2a00eb137fecae))
    - Add support for comments after section headers (closes #1259) ([`5e0b7f0`](https://github.com//sharkdp/bat/commit/5e0b7f013ac636bb6e37b3bfcb4769c8bdf5b406))
    - Add CSV syntax hightlighting test ([`aa205c6`](https://github.com//sharkdp/bat/commit/aa205c6a9face3a833d1b743562a93de8928cc9a))
    - Add Terraform example file for Syntax Highlighting ([`44bfad2`](https://github.com//sharkdp/bat/commit/44bfad24a1a7cd5ce9093926f85f22cca8386f7f))
    - add syntax highlight file ([`13e0184`](https://github.com//sharkdp/bat/commit/13e01841caa818f875fb6606d4e95cb516dffddc))
    - Add missing newline ([`a999975`](https://github.com//sharkdp/bat/commit/a999975bc9f429ab4bdb16316dfe04e867eddfc5))
    - Added missing highlighted generated test file ([`edf30e1`](https://github.com//sharkdp/bat/commit/edf30e1fa17248a96abf30cd7ec786a4fc621a9d))
    - Added CSS syntax highlight file and folder ([`f41bf20`](https://github.com//sharkdp/bat/commit/f41bf2082a06c06ad5d79ef9b01b12094742499b))
    - Replace SCSS example with unlicensed one ([`894d3f1`](https://github.com//sharkdp/bat/commit/894d3f1722f8d2ab760da0a1ea01e1f263ae16fd))
    - Rename SASS tests to SCSS, since they're using SCSS syntax ([`83d96e2`](https://github.com//sharkdp/bat/commit/83d96e2bbbcb4e2bab96c0b2897c5f35a65d0adc))
    - Add SQL syntax highlighting test ([`7a180e2`](https://github.com//sharkdp/bat/commit/7a180e22ff37d0b078eb4b0135c51a1a71bacf0b))
    - Add keith-hall as a maintainer! ([`3609700`](https://github.com//sharkdp/bat/commit/3609700b5e226482355a00b01174ca816dbb8f3d))
    - Fix mismatch between syntax-test and hightlighted ([`e85064e`](https://github.com//sharkdp/bat/commit/e85064e4cfa85bc0884e89ddb40b5fcbcf2d49b4))
    - Add example file for YAML highlighting ([`dd95f8b`](https://github.com//sharkdp/bat/commit/dd95f8b5cb8a4d045ed8c78dfe525f43a2d5a97a))
    - Add manually converted syntax ([`c97aa55`](https://github.com//sharkdp/bat/commit/c97aa551639373ba048048e35b253108eea78305))
    - Bump assets/syntaxes/02_Extra/TypeScript from `f21aba5` to `603ebb4` ([`2e1e307`](https://github.com//sharkdp/bat/commit/2e1e30705a08eecdc07c599bdeb36baf947f7588))
    - added test.dart ([`c92ac80`](https://github.com//sharkdp/bat/commit/c92ac809a5966d925858faacc22e260fe92bb899))
    - Added a JavaScript test file ([`ec842e8`](https://github.com//sharkdp/bat/commit/ec842e8d51f835a1f1f50ec8a716fc413a0b59a6))
    - add LICENSE ([`6d6e390`](https://github.com//sharkdp/bat/commit/6d6e3900c3b183628c907f0fb02bee0ad85a5d47))
    - add .gitignore test ([`0516a0d`](https://github.com//sharkdp/bat/commit/0516a0d7a66a49237f65f2ec13e0c50d531b35c6))
    - Add option to specify exact binary in run-benchmarks.sh ([`e26ec31`](https://github.com//sharkdp/bat/commit/e26ec314638ffbdfdd66d43403f965a59e3ffc5d))
    - Add option to specify bat target in run-benchmarks.sh ([`e3bc41d`](https://github.com//sharkdp/bat/commit/e3bc41dbe67c964c1d35ab1c127b28991ae19772))
    - Fix spelling and use PROJECT_EXECUTABLE more ([`4ba7546`](https://github.com//sharkdp/bat/commit/4ba7546c65fc86ecd8f5b9bfebfb3e721b6676ff))
    - forgot to update highlighted file after changing ([`adbbe15`](https://github.com//sharkdp/bat/commit/adbbe158db3c042619ecb4243aa5d4723472912f))
    - License attribution not required ([`6d2be4a`](https://github.com//sharkdp/bat/commit/6d2be4a3051b925768dc068e550254470636749b))
    - add julia test file ([`78c9a5d`](https://github.com//sharkdp/bat/commit/78c9a5d45bcf6e1fcf351d7f9f8dda4c383e3a79))
    - Add Java file test ([`5fa1645`](https://github.com//sharkdp/bat/commit/5fa164516560386900e019435892747b6f88c0ed))
    - Merge pull request #1265 from yimmt/minor-manpage-changes ([`389f64d`](https://github.com//sharkdp/bat/commit/389f64df57b1c3beed4d1d1baa4f34dfeb6ae9bb))
    - Try to fix syntax-detection tests on Windows ([`bbf6ec0`](https://github.com//sharkdp/bat/commit/bbf6ec0458114b4d79abdad9ba297c7e1b3ded4d))
    - Add C file test ([`e58b20f`](https://github.com//sharkdp/bat/commit/e58b20f68d8b9ecedceaa03f3e78c560127c3527))
    - #1241 Added newline to end of example.xml ([`0ce697d`](https://github.com//sharkdp/bat/commit/0ce697dabaeb387371087f3d8da8d3035ddc3d59))
    - #1213 Added example xml file ([`a2dfadc`](https://github.com//sharkdp/bat/commit/a2dfadc6c852d89e9900f81dec95bcee97df08b6))
    - Print error message if bat not found on path ([`6df8b3f`](https://github.com//sharkdp/bat/commit/6df8b3fe72b0ccd77657bdda96c7f09dabfc09ce))
    - Add Cpp file test ([`b623adc`](https://github.com//sharkdp/bat/commit/b623adc6df59848c0cfd530332f69fa6bcb16a4e))
    - Add /etc/passwd syntax test file ([`2f77da7`](https://github.com//sharkdp/bat/commit/2f77da781317194350289d4c481f7cf92047e509))
    - Haskell highligth test ([`9897c99`](https://github.com//sharkdp/bat/commit/9897c9919042a9b37ef12d6a950709c2b5e334ef))
    - Add TeX syntax highlighting test files ([`aebc304`](https://github.com//sharkdp/bat/commit/aebc30447f6822ec5dfdc64965b19bc5c1161e4e))
    - Add C-Sharp (C#) syntax highlighting test files ([`d2bae54`](https://github.com//sharkdp/bat/commit/d2bae54ed0249c276118ba85025cd8d3d1d7b6c9))
    - #1213 Added example Markdown file ([`501c369`](https://github.com//sharkdp/bat/commit/501c369f39eb4a8d0eac667bc021b0075e478456))
    - ARM Assembly syntax test file ([`db3468a`](https://github.com//sharkdp/bat/commit/db3468a815ff7886a7ced0f902a0f1fdf2e3dd51))
    - Kotlin syntax test file ([`3aabed4`](https://github.com//sharkdp/bat/commit/3aabed44cec6c0fc3c1e32a26e24e856c0f3a935))
    - Add a protobuf syntax sample. ([`2e98519`](https://github.com//sharkdp/bat/commit/2e985198c6e25456332d1f037c59b3bb3755e3ac))
    - add Jinja2 syntax test file ([`74a2ef8`](https://github.com//sharkdp/bat/commit/74a2ef8138474fce5211ab6fb6cc96241e51e77a))
    - Add an SML syntax test file. ([`88ea21b`](https://github.com//sharkdp/bat/commit/88ea21b2762b3715d3816248953ed7f551cdaa7b))
    - add newline to JSON syntax source file ([`62397db`](https://github.com//sharkdp/bat/commit/62397dbd0b49f4d9866423ddf8fb4f09aebb9d92))
    - add json highlight test ([`317b086`](https://github.com//sharkdp/bat/commit/317b086f6604ae1e1ba5277a0b2e16c1b366cb1e))
    - Add change dir to script dir to allow running from project root dir ([`12340c1`](https://github.com//sharkdp/bat/commit/12340c1d10659df2f1b84a7040fa293b6b853b76))
    - Generate highlighted file on latest version ([`e747199`](https://github.com//sharkdp/bat/commit/e74719971153ad454be08e30adb98d0aba12ece9))
    - Add texts indicating source to LICENSE.md ([`df7ce88`](https://github.com//sharkdp/bat/commit/df7ce883c7042138cbc7abce809d955604e34211))
    - Add basic typescript test file ([`eb3e2dc`](https://github.com//sharkdp/bat/commit/eb3e2dca2479f73d8d9a62070d9c95eb08d6bf6a))
    - add asciidoc highlight test ([`2d9b936`](https://github.com//sharkdp/bat/commit/2d9b936b0a60dd05b2234a3befbaaba34d08c2ad))
    - Add elm syntax regression test ([`66c7aca`](https://github.com//sharkdp/bat/commit/66c7aca2f6e50e13c07b52dd9c169fb2d713510d))
    - Add a ruby source code syntax test with source and output ([`0c3344d`](https://github.com//sharkdp/bat/commit/0c3344d0c4db888ea039480d85121600a9c2222d))
    - Merge pull request #1226 from mdevlamynck/fix-scripts ([`3af7a6b`](https://github.com//sharkdp/bat/commit/3af7a6b082f471207474b34824789816ee2ddbf3))
    - Update README.md using cargo install --path ([`0ac2111`](https://github.com//sharkdp/bat/commit/0ac211140a27455947452c8d4fdcdf6ab80104e6))
    - update original file in license ([`5d49066`](https://github.com//sharkdp/bat/commit/5d490664ca3613965fdf01038b9b29305c5273b5))
    - Add scala code highlight test ([`d170dd3`](https://github.com//sharkdp/bat/commit/d170dd305d865c87d577f6f09a239f0780429433))
    - Replace #!/bin/bash with #!/usr/bin/env bash to improve compatibility with systems where /bin/bash is not available ([`b50bd58`](https://github.com//sharkdp/bat/commit/b50bd587954e8467cf6141b8c86ca416e74fb26c))
    - Add link to original source ([`1da279a`](https://github.com//sharkdp/bat/commit/1da279a290cc6710775f376556d1f71e4528cdcc))
    - add go highlight test ([`100b9a4`](https://github.com//sharkdp/bat/commit/100b9a470b3e04850e701f73a89a882c54dd97d3))
    - Make tests/syntax-tests/update.sh shell script executable. ([`43c2b36`](https://github.com//sharkdp/bat/commit/43c2b36eaa57b865353a9810d69bdeb29308e0bf))
    - Add an OCaml file for syntax tests. ([`203d0bd`](https://github.com//sharkdp/bat/commit/203d0bd8ae54cc369faa92c4dc112214831459cd))
    - Apply requested changes regarding Homebrew 🍺 ([`14092fb`](https://github.com//sharkdp/bat/commit/14092fbadb8f62695094258ee86afa086f691607))
    - Mention Homebrew on Linux 🍺🐧 ([`35ad767`](https://github.com//sharkdp/bat/commit/35ad76771dc71def370d2b1f32815f0ab49d4316))
    - Add a batgrep as Bash script syntax tests ([`96151c4`](https://github.com//sharkdp/bat/commit/96151c44aae21849ea566d881310c48446045468))
    - Add a simple Bash script for syntax tests ([`02dcb01`](https://github.com//sharkdp/bat/commit/02dcb01120b8f42073e8641fc32ad35ec597daa5))
    - Fix syntax highlighting test update.sh to work on Mac OS ([`51d25e5`](https://github.com//sharkdp/bat/commit/51d25e5859171e060eb6edd6a1de758b4bc125ce))
    - update CHANGELOG.md ([`b853568`](https://github.com//sharkdp/bat/commit/b8535689d3fcb5a6df19a357ce895b707aa7749b))
    - add an after help NOTE ([`8ff8906`](https://github.com//sharkdp/bat/commit/8ff890635f2188ea84ff94e4531db01d5c473c73))
    - Added Elixir file example ([`29b6ba0`](https://github.com//sharkdp/bat/commit/29b6ba07585ddf7553ae4468a7eb8014818afb25))
    - Added Basic python test file ([`ca5793d`](https://github.com//sharkdp/bat/commit/ca5793dbea60d7b11643a508b618ca7a4fb11e9e))
    - Remove unnecessary white spaces ([`0fdaa8e`](https://github.com//sharkdp/bat/commit/0fdaa8e7451c192908f50269125b0d8c5e5d8575))
    - Merge the change in fca83bf7 and 6e3e7daf ([`5a71044`](https://github.com//sharkdp/bat/commit/5a71044d5a2522815927be56cd32676dd2f51f2c))
    - Merge the change in 1f177f0a ([`8d09105`](https://github.com//sharkdp/bat/commit/8d091051afddcc201c5cc20dc764d5c72619686f))
    - less is just an example for PAGER ([`34da1ef`](https://github.com//sharkdp/bat/commit/34da1ef9d61bbf6ea5b68fca38ec654252cafbf8))
    - use checkout v2 ([`e31e35c`](https://github.com//sharkdp/bat/commit/e31e35c46b3b5e8237ea0469b60e2c9183e3cee0))
</details>

# v0.16.0 (2020-10-02)

## Features

- Added support for the `NO_COLOR` environment variable, see #1021 and #1031 (@eth-p)
- Added `-P` short flag to disable paging, see #1075 and #1082 (@LordFlashmeow)
- Added `--force-colorization`/`-f` flag to provide an alias for forced color and decoration output, see #1141 (@alexanderkarlis)

## Bugfixes

- Fixed non-printable characters display for redirected output, see #1061 (@gsomix)
- Handle file extension conflicts in `--list-languages`, see #1076 and #1135 (@Kienyew)

## Other

- Switched to "·" (U+00B7) Middle Dot from "•" (U+2022) Bullet for non-printing spaces, see #1056 and #1100 (@LordFlashmeow)
- Added zsh shell completion script, see #1136 (@Kienyew)
- Improved `--help` text (@sharkdp)
- Added custom languages/themes sections to manpage (@eth-p)

## Syntaxes

- Update AsciiDoc syntax, see #1034 (@rxt1077)
- GLSL (@caioalonso)
- Add Nginx and Apache config file syntax, see #1137 (@kjmph, @niklasmohrin)
- Use `fstab` syntax for `crypttab` files, see #1073 (@sharkdp)
- Support syntax highlighting for files in `$XDG_CONFIG_HOME/git/`, see #1191 (@ahmedelgabri)

## New themes

- Gruvbox, see #1069 (@kyleondy)
- base16-256 for [base16-shell](https://github.com/chriskempson/base16-shell) users, see #1111 (@mk12)

## `bat` as a library

- Add APIs to provide `Input` descriptions with `InputDescription` (@eth-p)
- Add function to directly provide `Input`s to `PrettyPrinter` (@eth-p)
- **Breaking:** `Input::theme_preview_file` is no longer available. (@eth-p)

## Packaging

- Removed build dependency on `liquid` (@sharkdp).

## Commit Statistics

<csr-read-only-do-not-edit/>

 - 245 commits contributed to the release over the course of 128 calendar days.
 - 0 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 3 unique issues were worked on: [#1021](https://github.com//sharkdp/bat/issues/1021), [#1061](https://github.com//sharkdp/bat/issues/1061), [#1076](https://github.com//sharkdp/bat/issues/1076)

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **[#1021](https://github.com//sharkdp/bat/issues/1021)**
    - Add support for NO_COLOR env var ([`8b481dd`](https://github.com//sharkdp/bat/commit/8b481dd41f50e889031ed198dd66a95f32d6fb5b))
 * **[#1061](https://github.com//sharkdp/bat/issues/1061)**
    - Enable non-printable chars for redirected output ([`3c5ce9f`](https://github.com//sharkdp/bat/commit/3c5ce9f86cdb2a5e915d78770137a7d55a9c1dae))
 * **[#1076](https://github.com//sharkdp/bat/issues/1076)**
    - Handle file extension conflicts in --list-languages ([`f976340`](https://github.com//sharkdp/bat/commit/f97634011e9f2a4947c297b017acb0fb0c55cb71))
 * **Uncategorized**
    - v0.16.0 ([`6258dda`](https://github.com//sharkdp/bat/commit/6258dda0f851256c2e1d65cf87e997023a4f997b))
    - Update syntaxes ([`e6565b2`](https://github.com//sharkdp/bat/commit/e6565b2134fdf8f754b0559f9949b8737b2339f2))
    - Bump version to 0.16.0 ([`2f9ecf9`](https://github.com//sharkdp/bat/commit/2f9ecf90443c6741497d2b8d81fe1f7ade4be8f1))
    - Update CHANGELOG for 0.16 ([`a74d355`](https://github.com//sharkdp/bat/commit/a74d35582e09694772c0982cc026e1cafb45b8e5))
    - Update dependencies ([`8886faa`](https://github.com//sharkdp/bat/commit/8886faa0199253ee228b7829c23776bba02fb60e))
    - Add comment about Mandoc, closes #1145 ([`4bd1b15`](https://github.com//sharkdp/bat/commit/4bd1b157aef209b516f7de7bdd955df61ef3801c))
    - Update email syntax output ([`1ecb70e`](https://github.com//sharkdp/bat/commit/1ecb70e082638a391cd7f9c2d27ad1f5ad22615e))
    - Add Email example code ([`9eed16b`](https://github.com//sharkdp/bat/commit/9eed16beb4c4cd8f7d7997a86c433866fe9e4cc7))
    - Bump assets/syntaxes/02_Extra/Email from `ee3e68f` to `e89d09d` ([`c432a06`](https://github.com//sharkdp/bat/commit/c432a064a43df1a06ed4daac3ea6fd3d68bd3d47))
    - Fix zsh completion path ([`2d1a92b`](https://github.com//sharkdp/bat/commit/2d1a92b7cc2af6c35f868cbefe9dbfe466c59fcb))
    - Bump semver from 0.10.0 to 0.11.0 ([`cb31127`](https://github.com//sharkdp/bat/commit/cb31127b324223b7d1c6bf4367c70eb5eec80853))
    - Add comment about missing man page / completions, see #1198 ([`2beec1f`](https://github.com//sharkdp/bat/commit/2beec1f3eab9259983e08d651f49027b8d3ce681))
    - Update CHANGELOG ([`8d531b6`](https://github.com//sharkdp/bat/commit/8d531b6436437ca05a44863399b81a3552ddd85f))
    - Simplify code ([`30993a8`](https://github.com//sharkdp/bat/commit/30993a8bfc5c9913e7c35d225edba5fee21120f4))
    - Remove .unwrap() in insert call to prevent crashes ([`2258fb2`](https://github.com//sharkdp/bat/commit/2258fb27137da909a4f7fb1e2e5ffd12a3bfa826))
    - Remove .unwrap() to prevent crashes ([`a46191b`](https://github.com//sharkdp/bat/commit/a46191b8bfb69b18a0865b6c7d2b0fabf4011ede))
    - Remove non-working unit test ([`9c4c3e9`](https://github.com//sharkdp/bat/commit/9c4c3e965bfa8fe8367fc8539ab0af3cbc602221))
    - Support XDG_CONFIG_HOME git config files ([`31acbe2`](https://github.com//sharkdp/bat/commit/31acbe20d38d33b442a3ee0abf785b85d028f91f))
    - Add custom languages/themes sections to manpage ([`fd52c01`](https://github.com//sharkdp/bat/commit/fd52c01e7802e39a28dda1f96fb8dec19a02f2ac))
    - Bump assets/syntaxes/02_Extra/Elixir from `d223f67` to `4ee539b` ([`6ca5543`](https://github.com//sharkdp/bat/commit/6ca5543648c0f0ebd959e5e178bb849fafbd8abb))
    - Bump assets/syntaxes/02_Extra/Nginx from `dc70858` to `15a1db1` ([`83afcdc`](https://github.com//sharkdp/bat/commit/83afcdcf4923bf23fbd2d9a5479dabff4f6eca9a))
    - Bump git2 from 0.13.8 to 0.13.11 ([`59ca006`](https://github.com//sharkdp/bat/commit/59ca0069a6232f377be68e1562e2fab38fcc2bde))
    - Bump serde from 1.0.114 to 1.0.116 ([`6d61845`](https://github.com//sharkdp/bat/commit/6d6184500eb1955624c7dc5d9a88aafca3761a6b))
    - Update CHANGELOG ([`1c24ec1`](https://github.com//sharkdp/bat/commit/1c24ec16e263120b13a3f8d3e395b58ae3d10a52))
    - Add updated, highlighted output ([`5efc003`](https://github.com//sharkdp/bat/commit/5efc0033a1785b0ebf4ad7c75ba5b5af056c6194))
    - Add AWK file for syntax tests ([`c9237cc`](https://github.com//sharkdp/bat/commit/c9237cccab7c86c246fc435aaa151729754045ff))
    - Bump assets/syntaxes/02_Extra/AWK from `1ce5f90` to `e593eb6` ([`02cd68d`](https://github.com//sharkdp/bat/commit/02cd68db3788775ee3fb45054cf6576c145f1d2a))
    - Highlight crypttab files with fstab highlighting, closes #1073 ([`e4df564`](https://github.com//sharkdp/bat/commit/e4df5643dc86ae5c2d2f89bcd9c7359a761f92e3))
    - Add update script ([`89b62d5`](https://github.com//sharkdp/bat/commit/89b62d579fa8deb98e4af276fb77c40635fc1982))
    - Add syntax test for /etc/fstab ([`436a16a`](https://github.com//sharkdp/bat/commit/436a16aba62c1a2fb5021a68c9fdf06b65b534d5))
    - Improve --style help text, closes #1146 ([`32d22f4`](https://github.com//sharkdp/bat/commit/32d22f464d512d22b4feeb49059e094b585ffeff))
    - Code formatting ([`71b2089`](https://github.com//sharkdp/bat/commit/71b20893844857c11f26a92c600cadaa24cc0f3a))
    - Improve --file-name help text, closes #1109 ([`57aa88a`](https://github.com//sharkdp/bat/commit/57aa88a13a76434e3ab2d4a8065f6d8e208e5a3e))
    - Re-enable LTO ([`5ef35a1`](https://github.com//sharkdp/bat/commit/5ef35a10cf880c56b0e1c1ca7598ec742030eee1))
    - Fix unwrap error ([`f84acee`](https://github.com//sharkdp/bat/commit/f84aceec3d9bc37ea4eb8bfaecefd1cb1281f6a7))
    - Remove 'liquid' dependency ([`f18009e`](https://github.com//sharkdp/bat/commit/f18009e5d57bb4ee03511a9a2b6fd933d58d539f))
    - Use unwrap ([`83c7750`](https://github.com//sharkdp/bat/commit/83c775065657dfc5bbaa83060f073a83d7fec911))
    - Handle file extension conflicts in --list-languages ([`c477e23`](https://github.com//sharkdp/bat/commit/c477e23fe9c8c0d677f8ed2b2b94c81262a48cc6))
    - Revert to previous commit ([`31fb708`](https://github.com//sharkdp/bat/commit/31fb7087f176d559858d251d94e0b9b824f20183))
    - Update CHANGELOG.md ([`6bc477e`](https://github.com//sharkdp/bat/commit/6bc477e88fc2462c1fdb5f92946766a88029c222))
    - Fix undesired behavior ([`52d6701`](https://github.com//sharkdp/bat/commit/52d6701f8fe7593c7ecd4aa060d120021f575d04))
    - Update CHANGELOG ([`a4ffc9d`](https://github.com//sharkdp/bat/commit/a4ffc9d5ed6aa63853c41ebfa36ec8ff40e1ee4b))
    - Fix packaging and Use parameterized names for zsh completion ([`a86f3e5`](https://github.com//sharkdp/bat/commit/a86f3e5b81bbc79e5ae3f4be11f091019db2079f))
    - Handle zsh completion when packaging ([`d9e8bbc`](https://github.com//sharkdp/bat/commit/d9e8bbcb10f9d4631e66b086a6cd7e358ef9e7a8))
    - Fix typo ([`8b0886a`](https://github.com//sharkdp/bat/commit/8b0886ac3249a64d200eb71c76749b5464f7cea6))
    - Update CHANGELOG.md ([`aa182da`](https://github.com//sharkdp/bat/commit/aa182dad76b1561ca15d92c22a6109a3e538f6d6))
    - use _describe instead of _values on subcommand ([`41a1952`](https://github.com//sharkdp/bat/commit/41a1952928ca6154f92a94d15a27281fe3eb33fd))
    - add completion for 'cache' subcommand ([`9885d4e`](https://github.com//sharkdp/bat/commit/9885d4ebf0936bb078d418cb243237fabceb7f53))
    - Fix character escape problem ([`222e080`](https://github.com//sharkdp/bat/commit/222e080ce7c3a3aefdd29c71a9e20088c97e501a))
    - Add zsh completion ([`046409d`](https://github.com//sharkdp/bat/commit/046409d7c9ca4e67ae7f9ac7fda145048ebf80f9))
    - Remove unnecessary parenthesis ([`5df449b`](https://github.com//sharkdp/bat/commit/5df449bcf370666b666835d6a307de8469b41850))
    - removed printlns, updated Changelog, updated man ([`6017989`](https://github.com//sharkdp/bat/commit/6017989c4cb1683cc7d454d0f9175022e973ccee))
    - updated PR based on comments ([`d349974`](https://github.com//sharkdp/bat/commit/d349974089dc26a0f385a94a2d654537586a7f55))
    - remove println ([`ccdea6a`](https://github.com//sharkdp/bat/commit/ccdea6a429801c52967fa75868491f65b3b83ce7))
    - added additional alias for color=always when always-decorations flag is triggered ([`fdf1132`](https://github.com//sharkdp/bat/commit/fdf11326efff48caf3c8c4d59a144c49948787f5))
    - Added new alias for 'decoration=always' ([`565a803`](https://github.com//sharkdp/bat/commit/565a80305ca045e40eb5ba4b2fa127eb9d63e027))
    - Document PR in CHANGELOG.md ([`57a8122`](https://github.com//sharkdp/bat/commit/57a8122a6b9e77f9d1814b1c76823e21267f907f))
    - Document manual change in Apache sublime syntax file ([`bc5517d`](https://github.com//sharkdp/bat/commit/bc5517da7893b6025901410eb06e4e0eb4f1ae16))
    - Adds custom mapping for nginx and apache config files ([`ad18f07`](https://github.com//sharkdp/bat/commit/ad18f070aecfaeda6fa4dd7173c767517992d092))
    - Add manually generated sublime syntax file for apache configs ([`e305402`](https://github.com//sharkdp/bat/commit/e3054022127d0323ccea1ac4ee752c9f8a36b914))
    - Adds Apache config file syntax highlighting ([`d62b99f`](https://github.com//sharkdp/bat/commit/d62b99f54320dc3de7a3d2e8ebcc293bbd33a79e))
    - Adds Nginx config file syntax highlighting ([`4d8133e`](https://github.com//sharkdp/bat/commit/4d8133eb345659c35a6886674ff82d2c15e9b733))
    - Bump assets/syntaxes/02_Extra/SCSS_Sass from `b98a3f3` to `bc6332c` ([`5cba5b9`](https://github.com//sharkdp/bat/commit/5cba5b99894f6ac993b741672ed813ad8913b85c))
    - Bump clap from 2.33.1 to 2.33.3 ([`31f2312`](https://github.com//sharkdp/bat/commit/31f23123ef4c669554983f812cff0d0089a56ce4))
    - Bump syntect from 4.2.0 to 4.4.0 ([`b9dac6c`](https://github.com//sharkdp/bat/commit/b9dac6ca5d82120cf7f8d7dd96d2b30c7497f027))
    - Add Less file for syntax tests ([`c3f85c8`](https://github.com//sharkdp/bat/commit/c3f85c8372d9b4d7587f2309bea61c3ad0dbd937))
    - Add SASS file for syntax tests ([`7f5d6d9`](https://github.com//sharkdp/bat/commit/7f5d6d9791e7ca49bfb7fa102ce0a81cb64e1b5d))
    - Bump assets/syntaxes/02_Extra/ssh-config from `bee376c` to `1ddcb32` ([`96d4cdb`](https://github.com//sharkdp/bat/commit/96d4cdb7cb7be51a11345ffaef3b9f7019d94f6b))
    - Bump error-chain from 0.12.3 to 0.12.4 ([`9f1f3ab`](https://github.com//sharkdp/bat/commit/9f1f3ab5e3f4fb3a30638ea140a428e21d9ac412))
    - Bump console from 0.11.3 to 0.12.0 ([`6e90f17`](https://github.com//sharkdp/bat/commit/6e90f17a159b047eb5ea4633cd644ab2979d51ac))
    - Bump liquid from 0.21.1 to 0.21.4 ([`5b65448`](https://github.com//sharkdp/bat/commit/5b654480f4fb0aeb3ce74b5e32cac24534f195a4))
    - Stylistic, spelling and punctuation fixes in README-ru ([`803447e`](https://github.com//sharkdp/bat/commit/803447e2a1a6967f09cd7962183facdabc2b7a13))
    - update README to clarify disjunctive licensing ([`99c04a8`](https://github.com//sharkdp/bat/commit/99c04a87406ffe9eb45ce4e5cead4cf0dc7bf78b))
    - change docs dir name to match package name ([`e4370d8`](https://github.com//sharkdp/bat/commit/e4370d8d3f83c51da0ca7c09e11ed843d3b39f69))
    - add Depends line ([`883cc96`](https://github.com//sharkdp/bat/commit/883cc964aeff6931329c32ae2d34f7b4f42e4008))
    - include changelog in package ([`1d4cee1`](https://github.com//sharkdp/bat/commit/1d4cee11b7c1b492ada4d9607c882a67c782715a))
    - include years in copyright notice ([`9f91a7d`](https://github.com//sharkdp/bat/commit/9f91a7d797c9c89eb15f9fe8be76107b4d2ea4aa))
    - ensure copyright is mode 644 ([`4a6b4fb`](https://github.com//sharkdp/bat/commit/4a6b4fb632e95b51b7e6314ab1638017a0abaf52))
    - remove leading article from description ([`7c730d1`](https://github.com//sharkdp/bat/commit/7c730d11c36e9c75dc319e31cd7a3007bde3acb2))
    - invoke gzip with -n ([`21de52e`](https://github.com//sharkdp/bat/commit/21de52ebeadcc29957ddecf362e7a3407a0ca34e))
    - Fix Typo ([`cab5ddf`](https://github.com//sharkdp/bat/commit/cab5ddf6ae91c8a84bf34385831a30963713755c))
    - Bump assets/syntaxes/02_Extra/ssh-config from `3ec06d0` to `bee376c` ([`b3657ae`](https://github.com//sharkdp/bat/commit/b3657aef2bc34ef09cd86e2ea044e6bdee0f8141))
    - Fix typo ([`b0f8841`](https://github.com//sharkdp/bat/commit/b0f884121757456128e39a818510ca55933233de))
    - Clarified which pager is used in docs ([`febe335`](https://github.com//sharkdp/bat/commit/febe335831ae804bb5cc8be755782cf5344649ed))
    - Make sure that environment is properly set up ([`cbe68ba`](https://github.com//sharkdp/bat/commit/cbe68ba84c0291598d9060a73fa2d60549b4a85e))
    - Rebuild bat with new assets ([`9110b00`](https://github.com//sharkdp/bat/commit/9110b00e2ea6ec4b762c86820a817773ac173ea0))
    - Make test script executable ([`4b012de`](https://github.com//sharkdp/bat/commit/4b012de7dbe17b37c974b180ccfbce96fcd9ecd9))
    - Add CI job ([`49370e2`](https://github.com//sharkdp/bat/commit/49370e2175c9c372e5d46e6e64ccfaa8d1ba0c2a))
    - Add comparison script ([`02a89d8`](https://github.com//sharkdp/bat/commit/02a89d8ee82865f50015ec19e675ba81b61d0960))
    - Add proper CLI ([`a70efae`](https://github.com//sharkdp/bat/commit/a70efae79b99e0c4b772d9bba43f25992d1940dc))
    - Initial version of syntax regression tests ([`78a681f`](https://github.com//sharkdp/bat/commit/78a681f02782f627756eeabfecbdb50dbd2516b7))
    - Update themes.bin ([`3a85fd7`](https://github.com//sharkdp/bat/commit/3a85fd767bd1f03debd0a60ac5bc08548f95bc9d))
    - Add base16-256 theme to unit test ([`2ffbe08`](https://github.com//sharkdp/bat/commit/2ffbe08eb171ef71ee08b4da38c87ffa75934f72))
    - Add new theme: base16-256 ([`f9d5e81`](https://github.com//sharkdp/bat/commit/f9d5e81f0e72a130ccbe466c82ee1a0acb919c3d))
    - Bump dirs from 2.0.2 to 3.0.1 ([`00d1267`](https://github.com//sharkdp/bat/commit/00d1267bddd88e2008910414c25b20b13045c088))
    - Bump assets/themes/onehalf from `970abdf` to `3aa42a3` ([`001ee34`](https://github.com//sharkdp/bat/commit/001ee34d012f6643a5dd35c01ed641138e20cf9c))
    - Use GitHub Actions badge ([`db15756`](https://github.com//sharkdp/bat/commit/db157567fd168d62e965d299c74c39bc0d8c1624))
    - Fix here-doc indentation, part 2 ([`9e0fab1`](https://github.com//sharkdp/bat/commit/9e0fab16ff2ec56c307c06babf19ccce3ea2f5e0))
    - Fix spelling of LICENSE ([`a9af4f4`](https://github.com//sharkdp/bat/commit/a9af4f4ca1e000856ef6008d8d2803260f743464))
    - Use ansi-dark theme in GA output ([`96d682c`](https://github.com//sharkdp/bat/commit/96d682ca49e9b1ecb6eaafe6981d2c261b9f824d))
    - Add list of languages/themes ([`7056bea`](https://github.com//sharkdp/bat/commit/7056bea9e03e07df420ef5414a93bfa80cff44ad))
    - Colored output ([`63e13d8`](https://github.com//sharkdp/bat/commit/63e13d8e540d91bb768d49480f3798139be5f79d))
    - Add checks for all feature combinations ([`81587b3`](https://github.com//sharkdp/bat/commit/81587b3578653d453daa4c2aa8ba34fb067510ef))
    - Add bat test run ([`663c7cf`](https://github.com//sharkdp/bat/commit/663c7cf1d7b8dbb8feaac496d9706fb847cf63ee))
    - Fix here-doc indentation ([`8705d01`](https://github.com//sharkdp/bat/commit/8705d018fdfb75c9a7c978cb7b2a9d2575214d77))
    - Add old copyright, control files; add man page and autocompletions ([`8c33a32`](https://github.com//sharkdp/bat/commit/8c33a32256df0c20a0375cf5cb77605a5220582b))
    - Use recursive checkout ([`37a720b`](https://github.com//sharkdp/bat/commit/37a720be062b64a2a22263928559eeefc4442e34))
    - Add --path argument ([`aacbdcf`](https://github.com//sharkdp/bat/commit/aacbdcfc44cec9dffc506fe94f14f3e07ee90336))
    - Remove --release argument ([`794f3d0`](https://github.com//sharkdp/bat/commit/794f3d08f4f3e927cebe52d5fdc53568b801d928))
    - Add job to run tests with new syntaxes/themes ([`3147f05`](https://github.com//sharkdp/bat/commit/3147f0536a1094b6534c20dc17101097e306631e))
    - Adapt job step names ([`42de486`](https://github.com//sharkdp/bat/commit/42de486e940a2b5db27dfd525e0fca4d7e11137b))
    - Remove unneeded i586-* jobs ([`54c9d7c`](https://github.com//sharkdp/bat/commit/54c9d7cdb2258a90a6d3e3d1d283ce802a475ad0))
    - Remove TravisCI and AppVeyor configuration files ([`c8e394f`](https://github.com//sharkdp/bat/commit/c8e394f3f010fb5fc5cf0e7f9bf94b8b1cc1973a))
    - Change MinSRV => Minimum supported Rust version ([`4b65e6e`](https://github.com//sharkdp/bat/commit/4b65e6e4433a05545a7ed6324971e25ce45d15cf))
    - Disable code-coverage comments ([`5a6c65c`](https://github.com//sharkdp/bat/commit/5a6c65c589a9d88ce620668f5373de1fdd202af0))
    - Bump liquid from 0.20.1 to 0.21.1 ([`75246fb`](https://github.com//sharkdp/bat/commit/75246fb25b1e68724d636861d24ffa39845c58f2))
    - Bump git2 from 0.13.6 to 0.13.8 ([`0cc1084`](https://github.com//sharkdp/bat/commit/0cc10848196e732d6ee8e0649478d62f6a85fced))
    - Bump error-chain from 0.12.2 to 0.12.3 ([`21eedaf`](https://github.com//sharkdp/bat/commit/21eedaf76e6c1e9cd6671856c87d17644c083d4c))
    - Bump predicates from 1.0.4 to 1.0.5 ([`2656c50`](https://github.com//sharkdp/bat/commit/2656c503a45e67bdc7ee8220d20232bfcf934812))
    - Update --help text for --color ([`2b3457e`](https://github.com//sharkdp/bat/commit/2b3457e8d6bb52a79a3ffce0db7db7612c8afd12))
    - Always use 'batcat' in info.sh, if needed ([`f5d606c`](https://github.com//sharkdp/bat/commit/f5d606c4fceadd3d3f75c981fcb04cc52b538eb3))
    - Update changelog ([`0e38c17`](https://github.com//sharkdp/bat/commit/0e38c1728e65902f72bb53bb100275b01145f79d))
    - Fix integration test ([`6faf615`](https://github.com//sharkdp/bat/commit/6faf61552eeca7f6d27c73ed65c92c931bb5f25c))
    - Add syntaxes.bin changes ([`fcf5ec2`](https://github.com//sharkdp/bat/commit/fcf5ec2f7714788f3c9bc7aa0f0e47b884d69b55))
    - Switch to "·" (U+00B7) Middle Dot from "•" (U+2022) Bullet ([`9981908`](https://github.com//sharkdp/bat/commit/99819087f1cae6324b166f9062401f6044b935ea))
    - Add clarification about how the indentation works ([`3956e96`](https://github.com//sharkdp/bat/commit/3956e96e748b1e89b7a717f39a244a6946c82511))
    - Update generated config to reflect pager argument behaviour ([`a0b89a6`](https://github.com//sharkdp/bat/commit/a0b89a68d9d74ee1d68cb344efe63507d482ea67))
    - Make Windows 10 dependencies more clear ([`62014c1`](https://github.com//sharkdp/bat/commit/62014c1094470833b2b72179d3f1bf7f803404e3))
    - Merge pull request #1082 from LordFlashmeow/master ([`58dcabf`](https://github.com//sharkdp/bat/commit/58dcabffe6d98b0fed066c41e07af59398c3c37f))
    - Remove outdated Docker installation method ([`05027b7`](https://github.com//sharkdp/bat/commit/05027b7c7400645294468772d30249371a2c200a))
    - Merge pull request #1066 from gsomix/feature/1061-show-all-redirected ([`3a62e3d`](https://github.com//sharkdp/bat/commit/3a62e3d18835dce57294e5cec48e9d878351629b))
    - Merge pull request #1069 from KyleOndy/add_gruvbox_theme ([`963d9ee`](https://github.com//sharkdp/bat/commit/963d9ee584dc3fbc19641a39a3fc2024a242384a))
    - Add updated themes.bin ([`12e4fd1`](https://github.com//sharkdp/bat/commit/12e4fd138b6c4ee9d8e9203d3d66e008ff85148e))
    - Add new themes to unit test ([`2461d22`](https://github.com//sharkdp/bat/commit/2461d222127f6c7938b10068a1a12c5cd3d151a4))
    - Merge branch 'master' into add_gruvbox_theme ([`611c6e9`](https://github.com//sharkdp/bat/commit/611c6e9c744685658b3a3260923c025bdb398e25))
    - Bump serde_yaml from 0.8.12 to 0.8.13 ([`38efdb5`](https://github.com//sharkdp/bat/commit/38efdb5148ae2dfcacc721fef440f3faa982794c))
    - Hide alias in help, update man and completions ([`c264f74`](https://github.com//sharkdp/bat/commit/c264f747714a42318f1b33309778560ff9d6a8b0))
    - Merge remote-tracking branch 'sharkdp/master' ([`f53ea60`](https://github.com//sharkdp/bat/commit/f53ea60ed4cc47445239dab8bc6850016b22b32c))
    - Change to name no-paging, add long flag and alias ([`5fdeeab`](https://github.com//sharkdp/bat/commit/5fdeeabb444c3c23e9785c13b79768c54c97c57d))
    - Merge remote-tracking branch 'sharkdp/master' ([`b0d1975`](https://github.com//sharkdp/bat/commit/b0d19752c659170e7f9a4ab681803dbe7b5cdddd))
    - Merge pull request #1090 from sharkdp/dependabot/cargo/liquid-0.20.1 ([`a95a577`](https://github.com//sharkdp/bat/commit/a95a57762236e9c72f85f1688fa4612c93611bd5))
    - Bump unicode-width from 0.1.7 to 0.1.8 ([`ca44a80`](https://github.com//sharkdp/bat/commit/ca44a80a427ca445f02939dde71b649b6eef5af7))
    - Bump serde from 1.0.111 to 1.0.114 ([`d357227`](https://github.com//sharkdp/bat/commit/d357227e5760d1141acc7fb23271e0a9b14a5c28))
    - Bump liquid from 0.20.0 to 0.20.1 ([`bf841e6`](https://github.com//sharkdp/bat/commit/bf841e63bd7c149a3bb22c891d2b5e5b5ffc4d0e))
    - Merge pull request #1088 from sharkdp/dependabot/cargo/wild-2.0.4 ([`993d703`](https://github.com//sharkdp/bat/commit/993d703ba028efed92fd7e65c035bd81b5ffe2a2))
    - Bump wild from 2.0.3 to 2.0.4 ([`b449b82`](https://github.com//sharkdp/bat/commit/b449b8219172f3b8bcf023c995fd259ee1b4b015))
    - Add integration tests ([`bfac6fd`](https://github.com//sharkdp/bat/commit/bfac6fd85c43caf0b24ff1b9c9619d1099360a2c))
    - Add short flag for --paging=never ([`2575aae`](https://github.com//sharkdp/bat/commit/2575aae945a78b5d976c0a21faba730b7bc837f1))
    - Merge pull request #1070 from caioalonso/master ([`b0191cd`](https://github.com//sharkdp/bat/commit/b0191cdf2c52496fd92b835adf3cb7b0787154fa))
    - Defaults *.fs to F# ([`fc121f0`](https://github.com//sharkdp/bat/commit/fc121f0c8702a23951596850f7bec2bb2de5b45b))
    - Update doc/README-ru.md ([`fa10b48`](https://github.com//sharkdp/bat/commit/fa10b48c712138e2aff020d19bc39166a4b43a3d))
    - Update doc/README-ru.md ([`d4db1c6`](https://github.com//sharkdp/bat/commit/d4db1c6af379a6c62ece45aff12941f74c57bd33))
    - Update doc/README-ru.md ([`f651e2c`](https://github.com//sharkdp/bat/commit/f651e2cfcab94f97f6a352338e2d4ec109ed854d))
    - Update doc/README-ru.md ([`9d9f99c`](https://github.com//sharkdp/bat/commit/9d9f99c2d1ed3207c05196f9f77b296981cf3318))
    - Update doc/README-ru.md ([`9c8dda1`](https://github.com//sharkdp/bat/commit/9c8dda1f9dfded33494f3718a8e2615b00f17846))
    - Update doc/README-ru.md ([`89521a5`](https://github.com//sharkdp/bat/commit/89521a513444eefe94c6f169a4d96744a50dcf5e))
    - Update doc/README-ru.md ([`d11355f`](https://github.com//sharkdp/bat/commit/d11355f985c1343bf08164790f91f07c3578257e))
    - Update doc/README-ru.md ([`7ccda6e`](https://github.com//sharkdp/bat/commit/7ccda6e6e9cb832dbb3250cb33b03c2fca5beb27))
    - Update doc/README-ru.md ([`1de423c`](https://github.com//sharkdp/bat/commit/1de423c78b3085c2057f38173fad4c03bc091411))
    - Update doc/README-ru.md ([`3943857`](https://github.com//sharkdp/bat/commit/394385708711be80c0a76112b402bd2c6e1f79ee))
    - Update doc/README-ru.md ([`27bb5c5`](https://github.com//sharkdp/bat/commit/27bb5c597638e35b76a320fdfafda050c9ae4cac))
    - Update doc/README-ru.md ([`3f614cb`](https://github.com//sharkdp/bat/commit/3f614cb274366d4b054f07fcc080335e08ae6ff5))
    - Update doc/README-ru.md ([`1e407d9`](https://github.com//sharkdp/bat/commit/1e407d972ff24b9e1508d625ed98e27afdebb47d))
    - Update doc/README-ru.md ([`52b3c5f`](https://github.com//sharkdp/bat/commit/52b3c5fce43c5331bf5fc12cdc318e04b408eb92))
    - Update doc/README-ru.md ([`5d53273`](https://github.com//sharkdp/bat/commit/5d53273ef49920b06dfea95be021b07fa0546f7c))
    - Update doc/README-ru.md ([`bf9faa9`](https://github.com//sharkdp/bat/commit/bf9faa9c28696bb4a5443fec6a3105b187bf811b))
    - Update doc/README-ru.md ([`0356b7f`](https://github.com//sharkdp/bat/commit/0356b7f2c308d6978bcc8e8373f8cd3c55736d6b))
    - Update doc/README-ru.md ([`19c8558`](https://github.com//sharkdp/bat/commit/19c85581524c3fde97a02d190758a7b10396c3d4))
    - Update doc/README-ru.md ([`a137db8`](https://github.com//sharkdp/bat/commit/a137db86a40f0d0dc04b45aa474de580c5f3e669))
    - Update doc/README-ru.md ([`2a28f04`](https://github.com//sharkdp/bat/commit/2a28f04e0948434e84a2b6f05fbd6c52b25c20c3))
    - Update README-ru.md ([`8dfa69f`](https://github.com//sharkdp/bat/commit/8dfa69fde4b0744ffc2aac91c80a9764ba4f6bf9))
    - Adds GLSL syntax ([`233d375`](https://github.com//sharkdp/bat/commit/233d3759831e8fb49ce7f5b458ef89ead2f82e1f))
    - add peaceant's port of gruvbox themes ([`541f014`](https://github.com//sharkdp/bat/commit/541f014d50507077006b117b1df69704e754cf51))
    - modify japanese doc. ([`b09d245`](https://github.com//sharkdp/bat/commit/b09d245dea7e2f45cf461fda978f3256e7162465))
    - Highlight pacman hooks as ini ([`44deddb`](https://github.com//sharkdp/bat/commit/44deddbbfe92d0c50ce8e0fa2918f4ae0d8cc60f))
    - Highlight systemd files as ini ([`aa8a2b1`](https://github.com//sharkdp/bat/commit/aa8a2b1769e7731d6cabef98315c7929a996482e))
    - Revert "Highlight systemd files and Pacman hooks as .ini files" ([`4cdac18`](https://github.com//sharkdp/bat/commit/4cdac180d8c9af14bfa96f1fedce67bb8fc0b36c))
    - Highlight systemd files and Pacman hooks as .ini files ([`d079bf8`](https://github.com//sharkdp/bat/commit/d079bf86b12fa5eff7761d6c1dfe6d9823525feb))
    - fix typo in Japanese readme ([`62b4452`](https://github.com//sharkdp/bat/commit/62b445205721ff07931c110654718c5a455bff04))
    - Remove outdated Ansible installation method ([`c3ec92a`](https://github.com//sharkdp/bat/commit/c3ec92a13fe051142ab23d8a5521ab18115bd5dd))
    - README layout ([`c3230a6`](https://github.com//sharkdp/bat/commit/c3230a68bb12a855142617053a7de24b5b1f4665))
    - translation update with commit #6e3e7dafcaa840ed72748f2ca1e99e420769907b ([`d9f163f`](https://github.com//sharkdp/bat/commit/d9f163f4668c6269cba8a3583ffd14364f6ef830))
    - fix link to translation ([`d0da5af`](https://github.com//sharkdp/bat/commit/d0da5af810450094e80171373e3e7832b9ceb21b))
    - minor fixes ([`a2b2ec4`](https://github.com//sharkdp/bat/commit/a2b2ec467ac6fc7b0e76676dd6aa4c3392b37b71))
    - full translated ([`83b3009`](https://github.com//sharkdp/bat/commit/83b3009f0f73dd9bf53abab2ee0e21063bbb05be))
    - customization is translated ([`4ea70f8`](https://github.com//sharkdp/bat/commit/4ea70f84095443a5e5b4ffb1f6582d79f67d2fa5))
    - Installation is translated ([`0d2ef02`](https://github.com//sharkdp/bat/commit/0d2ef02c9ee2480a3fc62ee0a055dcd595de839f))
    - First part of russian translation ([`2d77962`](https://github.com//sharkdp/bat/commit/2d7796284bc51401e7d3b359e808e058723ee1dd))
    - Remove explicit type annotation ([`19aa878`](https://github.com//sharkdp/bat/commit/19aa878a0875a309ef9a5402d31ba10aa8dc3f06))
    - Prevent allocation of additional Strings ([`9f52012`](https://github.com//sharkdp/bat/commit/9f52012443bb3f46fd136bcc1c45978a26662111))
    - Use 'or_insert_with' ([`e57e9b6`](https://github.com//sharkdp/bat/commit/e57e9b6dbbc7e26692399547be8cf17c2f891021))
    - Return mappings as a slice ([`74e8373`](https://github.com//sharkdp/bat/commit/74e8373e34fed4c30d0efdc8b3b4864070129aec))
    - Use 'if let' instead of 'match' ([`ec2722d`](https://github.com//sharkdp/bat/commit/ec2722d46520c0fd8fe28b17a788dea5b5e4b7c3))
    - Incorporating feedback ([`9e2ea5f`](https://github.com//sharkdp/bat/commit/9e2ea5fdd43d90b4c8cecd8e1f0354fabdbe344c))
    - Adds a little logic to main to get other mappings from config ([`48b4a6a`](https://github.com//sharkdp/bat/commit/48b4a6a9064b86d099fb8ff38f40aae45bbe9a32))
    - Revert f8ed8aa7 (add test instrumentation to help understand failing tests) per PR feedback/owner request ([`92e9368`](https://github.com//sharkdp/bat/commit/92e93682c6655503465fc36a029552bc9330b430))
    - Revert 9ed8db22c ('windows' pager process execution fix) based on PR feedback/owner request ([`99226b7`](https://github.com//sharkdp/bat/commit/99226b745f20e267cebfeed411ad46741eb16d84))
    - Tests ~ add predicate normalization for tests using `echo` ([`4840c7c`](https://github.com//sharkdp/bat/commit/4840c7cd78910d54a7959d8d0d4187ca84d90c2d))
    - Tests ~ revert to `echo` (after fixed 'windows' pager process execution) ([`71ab4a2`](https://github.com//sharkdp/bat/commit/71ab4a20587defb436f260a602043e9cb2a14f0a))
    - Fix pager process execution under 'windows' ([`a2c09b4`](https://github.com//sharkdp/bat/commit/a2c09b41bc2e6a586310fd72d76ca1d14f95fbeb))
    - Tests ~ (fix) always check out test fixtures with known LF line endings ([`0ef792a`](https://github.com//sharkdp/bat/commit/0ef792ac1e127dcf919e77908035faf63616352b))
    - Tests ~ add instrumentation to visualize text differences in failing tests ([`01af28b`](https://github.com//sharkdp/bat/commit/01af28b781889a4cc52f563a6c4375422f48a336))
    - Tests ~ `echo` has portability issues; for CI, replace with `printf` ([`3a7743f`](https://github.com//sharkdp/bat/commit/3a7743ffc5ed7c770bd61bbf67ba1b52419dcfd1))
    - Fix compiler warning for 'windows' tests (dead_code) ([`9cc8e52`](https://github.com//sharkdp/bat/commit/9cc8e52512f8950340e3044e9494f2136ddf2ae2))
    - Maint/CICD ~ GHA - add additional linux builds and packaging support ([`698a6b5`](https://github.com//sharkdp/bat/commit/698a6b5acfc462ca442795044852d5405a6b93c3))
    - Maint/CICD ~ GHA - disable windows-gnu builds with unresolvable linker errors ([`7a51c7f`](https://github.com//sharkdp/bat/commit/7a51c7fd3087aa57d63e815c1b84fe843fe9c8b0))
    - Maint/CI ~ add GitHub-Actions CI (aka GHA) ([`7c3e9f8`](https://github.com//sharkdp/bat/commit/7c3e9f81cb2d99c47c7bba162655aac40fb10faf))
    - Clarify more wording when reading from stdin ([`6e3e7da`](https://github.com//sharkdp/bat/commit/6e3e7dafcaa840ed72748f2ca1e99e420769907b))
    - Clarify stdin syntax highlighting in the README ([`fca83bf`](https://github.com//sharkdp/bat/commit/fca83bf79a9a96fab871fea54df1d090eec7d88b))
    - fix typo in readme ([`b8ffb8d`](https://github.com//sharkdp/bat/commit/b8ffb8d4631da41c306cd5bdb06589e98586dd82))
    - Bump semver from 0.9.0 to 0.10.0 ([`1dae5bb`](https://github.com//sharkdp/bat/commit/1dae5bbc95c5f2297ebd1d12bc109f520c0351d2))
    - Bump console from 0.11.2 to 0.11.3 ([`1a1932b`](https://github.com//sharkdp/bat/commit/1a1932b67a30c5643710d2a131856a2e338b559c))
    - Bump git2 from 0.13.5 to 0.13.6 ([`8be375e`](https://github.com//sharkdp/bat/commit/8be375e75b4fb293db0eba666b448114cf296bbf))
    - Bump serde from 1.0.110 to 1.0.111 ([`e63e423`](https://github.com//sharkdp/bat/commit/e63e4232db6791894e6258dcef0f756759aeb65e))
    - Manually convert Crystal.sublime-syntax ([`cef0466`](https://github.com//sharkdp/bat/commit/cef04665eb82c3c777dc1ce88fcc36b1fea74597))
    - Bump assets/syntaxes/02_Extra/Crystal from `2ee9d66` to `5e032ff` ([`76569b9`](https://github.com//sharkdp/bat/commit/76569b952774d08d8aba4c355f6caae90e053002))
    - Manually convert TypeScript.sublime-syntax ([`1d46eb8`](https://github.com//sharkdp/bat/commit/1d46eb8ea300a786d05bd66286c453eb37dda1d1))
    - Bump assets/syntaxes/02_Extra/TypeScript from `9cd994a` to `f21aba5` ([`6b79ac9`](https://github.com//sharkdp/bat/commit/6b79ac995e2f77f01e8e8429a26b69b8d5232631))
    - Bump assets/syntaxes/02_Extra/Elixir from `1f010d5` to `d223f67` ([`ee928b0`](https://github.com//sharkdp/bat/commit/ee928b034e7a4333851c64c8e489072da56991ef))
    - Bump assets/syntaxes/02_Extra/LESS from `3020993` to `44632e1` ([`fc5790e`](https://github.com//sharkdp/bat/commit/fc5790eb4a484b1785043b85949eb02788593c86))
    - Bump clap from 2.33.0 to 2.33.1 ([`5213ffe`](https://github.com//sharkdp/bat/commit/5213ffee0e82a254bfc5c9c61ac961033a0adacf))
    - Update CHANGELOG ([`5ebf021`](https://github.com//sharkdp/bat/commit/5ebf0215c5b9b1ee74a41a0726eaa4c3cc43abbe))
    - Remove <font> tag from CHANGELOG.md ([`9957da6`](https://github.com//sharkdp/bat/commit/9957da6b93a22cc91ea7392568c55d93664e572e))
    - Change use of 'BatInput' to 'input::Input' ([`342cae2`](https://github.com//sharkdp/bat/commit/342cae2dc604db92aeff8f8166372b35700b10f4))
    - Remove unnecessary '-> ()'s ([`d2b26a5`](https://github.com//sharkdp/bat/commit/d2b26a5f1f8d704bf1e10ec9c36b6ad3516dbe4e))
    - Replace Input::stdin_as_file with bat-application functions ([`9d08c01`](https://github.com//sharkdp/bat/commit/9d08c0102eba2a6a2e613f2bc9cf3083252f580b))
    - Remove unused with_name for theme preview file ([`a335754`](https://github.com//sharkdp/bat/commit/a3357547ea782b6eee6509b07a9b685873926e76))
    - Remove Input.as_file and add Input::stdin_as_file ([`7a9deca`](https://github.com//sharkdp/bat/commit/7a9decad709c5bee97f682e1641740f33b2d0a36))
    - Update input API example ([`c4d0d06`](https://github.com//sharkdp/bat/commit/c4d0d068e4f716535e20ec43ea49ddd2011fe20e))
    - Move PR changes in README to unreleased section ([`c03a027`](https://github.com//sharkdp/bat/commit/c03a027240c7689df76f5bbf0246d67a05af1898))
    - Improve documentation for Input API ([`a8d7141`](https://github.com//sharkdp/bat/commit/a8d7141c4c157db30d60ed6dd55a6b2a763e5391))
    - Update examples ([`3eb704e`](https://github.com//sharkdp/bat/commit/3eb704e0169e3f2d56d44656a2d3c9579e6c9e93))
    - Reexport bat::pretty_printer::Input as bat::Input ([`0f06d3b`](https://github.com//sharkdp/bat/commit/0f06d3b90da3ed0d89bc4722c8cbc83ec1b8e234))
    - Fix regression with --list-themes and --language ([`589c94a`](https://github.com//sharkdp/bat/commit/589c94aa934ca66743971a37d4909d321e7571a7))
    - Refactor InputDescription API into Input API ([`798b742`](https://github.com//sharkdp/bat/commit/798b742617fe789647a34611716d60e5ece9ebf8))
    - Fix a typo ([`11a72ab`](https://github.com//sharkdp/bat/commit/11a72ab765f6c3266f3bad11524ff2d26cf0878a))
    - [breaking] Remove special handling for theme previews ([`0319149`](https://github.com//sharkdp/bat/commit/0319149b4d05f1457658f842710b92f7b91b635c))
    - Add PrettyPrinter::input function ([`2f823d5`](https://github.com//sharkdp/bat/commit/2f823d59b05b49f3286948dd8fdda5469dc22b8f))
    - Add InputDescription API ([`9813529`](https://github.com//sharkdp/bat/commit/981352992b773cc3ce8325463c12da9f48d60008))
    - Add documentation to InputDescription ([`384ea38`](https://github.com//sharkdp/bat/commit/384ea38bd9fa5d39388f346e82f00d92f0ad54b6))
    - Updated AsciiDoc syntax submodule ([`268c096`](https://github.com//sharkdp/bat/commit/268c0963b41f64407f6114ebd84a043c311d6fff))
    - Update CHANGELOG.md ([`6f88ba0`](https://github.com//sharkdp/bat/commit/6f88ba0e6fd2d6671657cb8a132eacd0b6836549))
</details>

# v0.15.4 (2020-05-27)

## Bugfixes

- Added missing Solarized themes, see #1027
- Fixed highlighting bug in Haskell source files, see #1026

## Commit Statistics

<csr-read-only-do-not-edit/>

 - 7 commits contributed to the release over the course of 1 calendar day.
 - 0 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' where seen in commit messages

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Updates for v0.15.4 ([`9e65ecd`](https://github.com//sharkdp/bat/commit/9e65ecd03e60336bd54dea12fbc4354c4fb8d8b0))
    - Update tests ([`9cb43ef`](https://github.com//sharkdp/bat/commit/9cb43ef64d9bb919679a3a43d5b22e91dfa95c84))
    - Remove 'Haskell (improved) syntax' ([`bb18ece`](https://github.com//sharkdp/bat/commit/bb18ece8a4e0b1356b3ac59d3fbe96e1c87c5de1))
    - Add unit test to make sure we are not missing any themes ([`2ab6474`](https://github.com//sharkdp/bat/commit/2ab647476132c10419bc0ff79e0aeaf0c7be0c79))
    - Revert "Bump assets/themes/Solarized from `87e0109` to `3707f68`" ([`fe20328`](https://github.com//sharkdp/bat/commit/fe20328ab762e74080c7aa6cf1c522d2f6e8aad2))
    - Update .gitattributes ([`d84ac09`](https://github.com//sharkdp/bat/commit/d84ac09abd7b721f0962ffc7433228f8586ddfc6))
    - Fix CHANGELOG ([`69e6cae`](https://github.com//sharkdp/bat/commit/69e6cae0a77bd036eeb754b0540b2efbdaf1e59c))
</details>

# v0.15.3 (2020-05-26)

## Bugfixes

- Cannot run `bat` with relative paths, see #1022
- bat mishighlights Users that start with digits in SSH config, see #984

## New syntaxes

- SML, see #1005 (@kopecs)

## Other

- Some syntaxes and themes have been updated to the latest version

## Commit Statistics

<csr-read-only-do-not-edit/>

 - 5 commits contributed to the release.
 - 0 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' where seen in commit messages

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Bump version to 0.15.3 ([`302707f`](https://github.com//sharkdp/bat/commit/302707f3ce75b8846ee84175bfb5a86dac9d0304))
    - Update CHANGELOG for 0.15.3 ([`2e20439`](https://github.com//sharkdp/bat/commit/2e2043971b753aaa76bec3569af17ead4be8a861))
    - Update binary assets ([`e54e218`](https://github.com//sharkdp/bat/commit/e54e218e3b51c842d104547034c164c7c91db364))
    - Switch from path-absolutize to path_abs ([`bd17fd5`](https://github.com//sharkdp/bat/commit/bd17fd571f99d52b4d76a24aee719dcd89a0244d))
    - Remove dummy line from CHANGELOG ([`3f6e88b`](https://github.com//sharkdp/bat/commit/3f6e88b4be22d71e3417ed09e3b7dee6516e1dbb))
</details>

# v0.15.2 (2020-05-25)

## Bugfixes

- Fix syntax detection for files called 'rails', see #1008
- Fix potential errors with syntax detection for symlinked files, see #1001
- `--map-syntax` doesn't work with names provided through `--file-name` (@eth-p)

## Other

- Add padding above headers when not using a grid, see #968 and #981 (@pt2121)
- bat now prints an error if an invalid syntax is specified via `-l` or `--map-syntax`, see #1004 (@eth-p)

## `bat` as a library

- `PrettyPrinter::vcs_modification_markers` has been marked deprecated when building without the `git` feature, see #997 and #1020 (@eth-p, @sharkdp)

## Packaging

- Compilation problems with `onig_sys` on various platforms have been resolved by upgrading to `syntect 4.2`, which includes a new `onig` version that allows to build `onig_sys` without the `bindgen` dependency. This removes the need for `libclang(-dev)` to be installed to compile `bat`. Package maintainers might want to remove `clang` as a build dependency. See #650 for more details.

## Commit Statistics

<csr-read-only-do-not-edit/>

 - 48 commits contributed to the release over the course of 13 calendar days.
 - 0 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' where seen in commit messages

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Bump version to 0.15.2 ([`28af18b`](https://github.com//sharkdp/bat/commit/28af18ba27c2888bd87998e703228ab2c2debbed))
    - Mark 'vcs_modification_markers' as deprecated ([`8f1babf`](https://github.com//sharkdp/bat/commit/8f1babf346e12279b3a52d24d047bf9bf4425e6e))
    - Make 'vcs_modification_markers' change non-breaking ([`fa25695`](https://github.com//sharkdp/bat/commit/fa25695b74fed1a5857e7f176ea43372256f9762))
    - Fix syntax detection for symlinks ([`943b1f8`](https://github.com//sharkdp/bat/commit/943b1f82b63506faff4f5d2e74c99d17de0b8535))
    - Update CHANGELOG ([`b7a0951`](https://github.com//sharkdp/bat/commit/b7a09516e70aaaec929da124af2fc4e77de42c09))
    - Fix syntax detection for files called 'rails' ([`c4031ad`](https://github.com//sharkdp/bat/commit/c4031ad65c72650938039dce9be5dcc0672a3d8a))
    - Bump wild from 2.0.2 to 2.0.3 ([`2eaced4`](https://github.com//sharkdp/bat/commit/2eaced4d305252d01738d7d2cb438f81bad3ee28))
    - Bump serde from 1.0.106 to 1.0.110 ([`bdea2fe`](https://github.com//sharkdp/bat/commit/bdea2fe94c6e9caca24ac3b62f7e19b849d50458))
    - Bump serde_yaml from 0.8.11 to 0.8.12 ([`c96ff30`](https://github.com//sharkdp/bat/commit/c96ff3087617bd88c0e881a394cddf94d78a613a))
    - Bump shell-words from 0.1.0 to 1.0.0 ([`4c100cf`](https://github.com//sharkdp/bat/commit/4c100cffdb9fde4a1a88b59806a6c81dbf70fb15))
    - Update README and CHANGELOG, see #650 ([`d6abad9`](https://github.com//sharkdp/bat/commit/d6abad908a51cf9396cc8521a5ec94b1fc9206ea))
    - Bump syntect from 4.1.1 to 4.2.0 ([`f112918`](https://github.com//sharkdp/bat/commit/f1129189ea19b905cb4ef6c25d92f6d4bdca80b5))
    - Add CHANGELOG entry ([`7e84477`](https://github.com//sharkdp/bat/commit/7e844774085f3ecd10182daea9a0122ee008bb82))
    - Move syntax to 02_Extra folder ([`6b86ab5`](https://github.com//sharkdp/bat/commit/6b86ab5cd09556b00225b97e404e9c677c629240))
    - Add syntax highlighting for SML ([`5074b96`](https://github.com//sharkdp/bat/commit/5074b96cd54094dd9000ebd1bb37ca325a81af53))
    - Update CHANGELOG entry ([`3094ac9`](https://github.com//sharkdp/bat/commit/3094ac90b70515cb0c476b4f3984b0559c94635a))
    - Print error message when invalid syntax is specified ([`35b6067`](https://github.com//sharkdp/bat/commit/35b60674962dcc5b0b09d121aa667c0d2c34d813))
    - Refactor HighlightingAssets::get_syntax to return Result type ([`5aa20c0`](https://github.com//sharkdp/bat/commit/5aa20c090bb01abf1fd2266bad35413447eff846))
    - Merge pull request #997 from eth-p/misc-fixes-1 ([`9c371ed`](https://github.com//sharkdp/bat/commit/9c371ed8a26dc51038c79b79e43f2f6e28461f5a))
    - Update CHANGELOG.md ([`f03ab11`](https://github.com//sharkdp/bat/commit/f03ab116239ee21b3450cbed82cb5b8f8a6b1e59))
    - Allow application to be built without git support ([`887e61a`](https://github.com//sharkdp/bat/commit/887e61a99d503b000fb289ebd9014204e1f64961))
    - Remove vcs_modification_markers from PrettyPrinter 'git' feature disabled ([`0aca8ca`](https://github.com//sharkdp/bat/commit/0aca8cab0b23cbc28630d694e106c3e5071abcaf))
    - Merge pull request #985 from eth-p/string-input ([`d08d1f8`](https://github.com//sharkdp/bat/commit/d08d1f8c4599e96a96a821499aeb46383619ce89))
    - Update CHANGELOG.md ([`403ab6e`](https://github.com//sharkdp/bat/commit/403ab6e44398ca081e5c72c6978904e7bdc4ba9c))
    - Merge branch 'master' of github.com:sharkdp/bat into string-input ([`b8687bc`](https://github.com//sharkdp/bat/commit/b8687bc9695027b967bdbae906f63b291f626525))
    - Remove unnecessary clone in integration tests ([`9166c9d`](https://github.com//sharkdp/bat/commit/9166c9dd355700067adb117382d6246bdefb8c7d))
    - Add helper fn for checking if opened input is theme preview file ([`cc52f79`](https://github.com//sharkdp/bat/commit/cc52f79e421d8f4e18600605340cdeee7ecd6c02))
    - Bump assets/syntaxes/02_Extra/Julia from `0ca4cc2` to `6c0d770` ([`c2c7070`](https://github.com//sharkdp/bat/commit/c2c70707125720619f7dd7d2ad6001a07cf97df6))
    - Bump assets/syntaxes/02_Extra/Email from `fc034fb` to `ee3e68f` ([`daf06bb`](https://github.com//sharkdp/bat/commit/daf06bb026fa2bd892da289e937ac555a64e171c))
    - Bump assets/themes/onehalf from `fdcbffe` to `970abdf` ([`5b11b1b`](https://github.com//sharkdp/bat/commit/5b11b1ba47393c6ff2305ab4f376316c6242a46c))
    - Update CHANGELOG ([`ee28744`](https://github.com//sharkdp/bat/commit/ee287447c5cbc13e0d6755a804307e187184f09a))
    - Bump assets/syntaxes/02_Extra/ssh-config from `16e19d5` to `3ec06d0` ([`257f978`](https://github.com//sharkdp/bat/commit/257f978cebab7942b813cc3bd5ad74a833d35b97))
    - Bump assets/themes/dracula-sublime from `811a634` to `26c57ec` ([`8e803bd`](https://github.com//sharkdp/bat/commit/8e803bdb02babbe095a41dab57816c2258127196))
    - Bump assets/themes/Solarized from `87e0109` to `3707f68` ([`a265242`](https://github.com//sharkdp/bat/commit/a265242e6eb66fd173f0ab7410664eae6397ea94))
    - Bump assets/syntaxes/02_Extra/Elm from `dfbb8bd` to `e266d27` ([`3aa9b31`](https://github.com//sharkdp/bat/commit/3aa9b31bda551d30063127bdb17085627e1b7798))
    - Bump assets/syntaxes/02_Extra/Elixir from `89b3bad` to `1f010d5` ([`c692c47`](https://github.com//sharkdp/bat/commit/c692c4770a7cca0fd79afc278308b6499af77976))
    - Consolidate syntax detection behavior for all InputKind types ([`59140b4`](https://github.com//sharkdp/bat/commit/59140b458c98fd9585b6c7207a106d73bbbb39f1))
    - Add regression test for detected syntax differing for stdin and files ([`82981c9`](https://github.com//sharkdp/bat/commit/82981c96636a0e1a418a8611ed67cc6a56ef7096))
    - Rename test for checking if inputkinds are consistent ([`157b8dd`](https://github.com//sharkdp/bat/commit/157b8dd848a90b021372c8d0a64b2e70abfb36f9))
    - Test that OrdinaryFile consistent with CustomReader ([`1fb669a`](https://github.com//sharkdp/bat/commit/1fb669ae1abfff0712dc370133ae92af56aa439c))
    - Change assets.rs tests to use InputKind::CustomReader ([`7d07aa3`](https://github.com//sharkdp/bat/commit/7d07aa395a90f33bb939625de8576c0fa667aef7))
    - Add 'mkdir -p …' to Ubuntu instructions, see #982 ([`6ceba56`](https://github.com//sharkdp/bat/commit/6ceba56288a0d05247d5b422a0d9c2ecb1c57896))
    - Update README.md ([`4be66f8`](https://github.com//sharkdp/bat/commit/4be66f8b8874f41913da68848360fa14a8be392c))
    - Add instructions for ~/.local/bin/bat symlink ([`54a85d9`](https://github.com//sharkdp/bat/commit/54a85d98f18d53cdf826405e1a7468d89b678c2d))
    - Update CHANGELOG ([`491a5a1`](https://github.com//sharkdp/bat/commit/491a5a17f32a28fc26c55b070ad58e234f2589d2))
    - Remove padding if the header style is not enabled ([`b337339`](https://github.com//sharkdp/bat/commit/b3373398e81d25ece5c654e4e755a38eb0de135e))
    - Add padding above headers when no grid ([`0040fef`](https://github.com//sharkdp/bat/commit/0040fef2153d70a4d184d5f0b77415e238f2b4e0))
    - Update README to mention batcat for Debian/Ubuntu ([`1a6e8d2`](https://github.com//sharkdp/bat/commit/1a6e8d297ffbfa2e82a20daa9872bac73e9f5005))
</details>

# v0.15.1 (2020-05-11)

## Bugfixes

- Fix highlighting of Markdown files, see #963 and #977
- Fix `base16` theme (was broken since in v0.14), see #972, #934 and #979 (@mk12).
  Users suffering from #865 ("no color for bat in ssh from a Windows client") can use the `ansi-dark` and `ansi-light` themes from now on.

## New syntaxes

- Fortran, see #957
- Email (@mariozaizar)
- QML, see #962 (@pylipp)

## Commit Statistics

<csr-read-only-do-not-edit/>

 - 21 commits contributed to the release over the course of 16 calendar days.
 - 0 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' where seen in commit messages

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Bump version to v0.15.1 ([`2596bfe`](https://github.com//sharkdp/bat/commit/2596bfe7a75f6867d803c1e9af0fa3467873a0e3))
    - Update syntaxes and themes ([`406bffa`](https://github.com//sharkdp/bat/commit/406bffa8c8df890c7d3a9897232c7574fa8cec0f))
    - Patch Markdown syntax ([`48441b9`](https://github.com//sharkdp/bat/commit/48441b99efb2670127460cebe0609eddfce9ec50))
    - Make syntax detection more consistent for Reader and File inputs ([`f39487f`](https://github.com//sharkdp/bat/commit/f39487fca9301c45eaf50af437a269d3c0600f61))
    - Update CHANGELOG ([`ce47171`](https://github.com//sharkdp/bat/commit/ce4717144a8869ec894d62107f5c9deb237f6968))
    - Fix base16, and combine 00 and 0f alpha encodings ([`f59d00d`](https://github.com//sharkdp/bat/commit/f59d00d4c722de05af561666d58d15600c749c1d))
    - Do not warn if both 'bat' and 'batcat' are available in info.sh ([`dcfdbf8`](https://github.com//sharkdp/bat/commit/dcfdbf82dd6abaef8047fe0deb7a6a808f15bb15))
    - Prevent batcat warning from being in output of info.sh ([`0cb884d`](https://github.com//sharkdp/bat/commit/0cb884d5012ff5182dd50fc0fbf5769dbd8775a9))
    - Check for 'batcat' in info.sh ([`7bc4020`](https://github.com//sharkdp/bat/commit/7bc4020a9235e39144d0017e904deb5d62c40b64))
    - Properly add QML submodule ([`0792195`](https://github.com//sharkdp/bat/commit/07921951568210d95e0dc62dbce5783ed2882ee0))
    - Bump git2 from 0.13.3 to 0.13.5 ([`83f24ff`](https://github.com//sharkdp/bat/commit/83f24ffcfbe4691c8dc6165b0b36e7469fd55a47))
    - Add QML syntax ([`74ec390`](https://github.com//sharkdp/bat/commit/74ec390770229ee70a3c484b1f534986836b94ad))
    - Adds Email (https://github.com/mariozaizar/email.sublime-syntax) ([`9014ffc`](https://github.com//sharkdp/bat/commit/9014ffcfd84570d6d740076dc9ca6ff80cd65ceb))
    - Use console 0.11.2, see #955 ([`81af375`](https://github.com//sharkdp/bat/commit/81af3752893b4fa2548738af9b3ad6c75be84eae))
    - Revert "Use console 0.11.1, see #955" ([`d707eab`](https://github.com//sharkdp/bat/commit/d707eab95cf6874a703743a7653233881c54cca4))
    - Use console 0.11.1, see #955 ([`d64c55e`](https://github.com//sharkdp/bat/commit/d64c55ef114e80a3feb423a98590c3c569242410))
    - Add '--locked' flag to all 'cargo install' commands ([`2b1d53f`](https://github.com//sharkdp/bat/commit/2b1d53fb27865f4784db64e96ebb99412b13541c))
    - Pin version of console to 0.10.0 ([`bf696f5`](https://github.com//sharkdp/bat/commit/bf696f58c89bc8115e2ead1a66a3250d33db1376))
    - Update CHANGELOG ([`296bad8`](https://github.com//sharkdp/bat/commit/296bad8ffd45928a1771c17f5f503f979df9ed81))
    - Add Fortran syntax, closes #957 ([`94310af`](https://github.com//sharkdp/bat/commit/94310af30c6d3b005786b84f98461c53d9294c3c))
    - Update CHANGELOG ([`38c096b`](https://github.com//sharkdp/bat/commit/38c096bf7908f762fa1a79a696f1212e52651e36))
</details>

# v0.15.0 (2020-04-25)

## Features

- Add a new `--diff`/`-d` option that can be used to only show lines surrounding
  Git changes, i.e. added, removed or modified lines. The amount of additional
  context can be controlled with `--diff-context=N`. See #23 and #940

## Bugfixes

- Error message printed in the middle of the output for another file, see #946
- Performance improvements when using custom caches (via `bat cache --build`): the `bat` startup time should now be twice as fast (@lzutao).

## Themes

- Updated version of the Solarized dark/light themes, see #941

## `bat` as a library

- There are a few changes in the "low level" API (the `Config` struct has changed and
  the error handler needs a new `&mut dyn Write` argument). The high-level API is not
  affected.

## Commit Statistics

<csr-read-only-do-not-edit/>

 - 20 commits contributed to the release over the course of 2 calendar days.
 - 1 commit where understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' where seen in commit messages

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Update CHANGELOG ([`696d1b3`](https://github.com//sharkdp/bat/commit/696d1b3ed3eae9ea9bd94fa7c4c05306dfd621b6))
    - Bump version to v0.15.0 ([`17d9872`](https://github.com//sharkdp/bat/commit/17d98724a5cd8cbdbcdc0893729351732a285786))
    - Update cached verison of themes ([`7d7ee1b`](https://github.com//sharkdp/bat/commit/7d7ee1b594560fdc65a5ea63b3307f26ef60e96d))
    - Update solarized themes to braver/Solarized, closes #941 ([`682b420`](https://github.com//sharkdp/bat/commit/682b4209f2ffd36c350af903d44ede1c541dcf23))
    - Remove old Sublime themes ([`41ddcbd`](https://github.com//sharkdp/bat/commit/41ddcbd2407507e1a1075fd0994eaebbf17352a3))
    - Write error messages to pager, if attached ([`48a7ce3`](https://github.com//sharkdp/bat/commit/48a7ce3bf23edee81274e6448cf98288ec6f7d31))
    - lto=true and codegen-units=1 together ([`3bcc4d0`](https://github.com//sharkdp/bat/commit/3bcc4d0d5556ee329f3f351b034e4bb298eed9c3))
    - Use rustup minimal profile ([`8821bca`](https://github.com//sharkdp/bat/commit/8821bca6567264fd5901e659fcddfd86e37822d9))
    - simplify build.rs ([`56111aa`](https://github.com//sharkdp/bat/commit/56111aa20dc7473e36d1e5996dc0ffb804a8f123))
    - Add annotations again ([`5fe8a83`](https://github.com//sharkdp/bat/commit/5fe8a8342b8550eca8b8a923c81fb7af9754647b))
    - Add '-d' for '--diff' ([`9891091`](https://github.com//sharkdp/bat/commit/989109145a39ee1ce65cfe2e17c502ce2e1a673c))
    - Skip non-file inputs when using --diff ([`23afc8e`](https://github.com//sharkdp/bat/commit/23afc8e90c4c98c405a07c1d0bfc54a8d719852c))
    - Run 'cargo fmt' ([`8e18786`](https://github.com//sharkdp/bat/commit/8e187865560aaf5349f85c0e42babb08958883c2))
    - Use unreachable!(…) ([`81488ad`](https://github.com//sharkdp/bat/commit/81488adf8b4a6b528f76c1b18ebd1da134db2cc3))
    - Fix some clippy lints ([`e37e9c1`](https://github.com//sharkdp/bat/commit/e37e9c12141a40fc65c207a501dad24f11709ee0))
    - simplify build.rs ([`a482838`](https://github.com//sharkdp/bat/commit/a4828387c1a09e5185aa33b338b2d70dc2510f27))
    - Bump min. required Rust version to 1.40 ([`abeac8b`](https://github.com//sharkdp/bat/commit/abeac8b12e9e4c0bc6d9c03ce0d12751a81e9757))
    - Implementation of 'bat --diff' ([`82e7786`](https://github.com//sharkdp/bat/commit/82e7786e747b1fcfac1f963ae6415a22ec9caae1))
    - cargo fmt ([`0064321`](https://github.com//sharkdp/bat/commit/00643213232a9e67ef1a811cec57ed40a99f374d))
    - Update version in README ([`9e622a1`](https://github.com//sharkdp/bat/commit/9e622a1a5f3a4328e37b73ec6cec491ff0c34508))
</details>

# v0.14.0 (2020-04-22)

## Features

- Added a new `--file-name <name>…` option to overwrite the displayed filename(s)
  in the header. This is useful when piping input into `bat`. See #654 and #892 (@neuronull).
- Added a new `--generate-config-file` option to create an initial configuration file
  at the right place. See #870 (@jmick414)

## Bugfixes

- Performance problems with C# source code have been fixed, see #677 (@keith-hall)
- Performance problems with Makefiles have been fixed, see #750 (@keith-hall)
- Fix bug when highlighting Ruby files with unindented heredocs, see #914 (@keith-hall)
- A highlighting problem with Rust source code has been fixed, see #924 (@keith-hall)
- Windows: short files that do not require paging are displayed and then lost, see #887
- `--highlight-line` did not work correctly in combination with `--tabs=0` and `--wrap=never`,
  see #937

## Other

- When saving/reading user-provided syntaxes or themes, `bat` will now maintain a
  `metadata.yaml` file which includes information about the `bat` version which was
  used to create the cached files. When loading cached files, we now print an error
  if they have been created with an incompatible version. See #882
- Updated `liquid` dependency to 0.20, see #880 (@ignatenkobrain)

## `bat` as a library

- A completely new "high level" API has been added that is much more convenient
  to use. See the `examples` folder for the updated code. The older "low level"
  API is still available (basically everything that is not in the root `bat`
  module), but has been refactored quite a bit. It is recommended to only use
  the new "high level" API, if possible. This will be much easier to keep stable.
  Note that this should still be considered a "beta" release of `bat`-as-a-library.
  For more details and some screenshots of the example programs, see #936.
- Stripped out a lot of binary-only dependencies, see #895 and #899 (@dtolnay)

  This introduces a `features = ["application"]` which is enabled by default and pulls in
  everything required by `bat` the application. When depending on bat as a library, downstream
  `Cargo.toml` should disable this feature to cut out inapplicable heavy dependencies:
  ``` toml
  [dependencies]
  bat = { version = "0.14", default-features = false }
  ```
  Other optional functionality has also been put behind features: `paging` and `git` support.
- Allow using the library with older syntect, see #896 and #898 (@dtolnay)

## New syntaxes

- Rego, see #872 (@patrick-east)
- Stylo, see #917


## Commit Statistics

<csr-read-only-do-not-edit/>

 - 116 commits contributed to the release over the course of 31 calendar days.
 - 3 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 2 unique issues were worked on: [#911](https://github.com//sharkdp/bat/issues/911), [#917](https://github.com//sharkdp/bat/issues/917)

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **[#911](https://github.com//sharkdp/bat/issues/911)**
    - Add cat alias detection to info.sh ([`96aedf6`](https://github.com//sharkdp/bat/commit/96aedf624031901c34763fa44513628e23ded6ca))
 * **[#917](https://github.com//sharkdp/bat/issues/917)**
    - Add Stylus syntax ([`61e47e0`](https://github.com//sharkdp/bat/commit/61e47e0c13390e0433779f7ae5161c574a6fcc74))
 * **Uncategorized**
    - Update dependencies ([`59ca933`](https://github.com//sharkdp/bat/commit/59ca933aeed2a9cc2f11397cc2a63866f343a769))
    - Add a syntax mapping for bats own config file ([`793af6b`](https://github.com//sharkdp/bat/commit/793af6b91101041c35b1e558a3db6b33c295fbd4))
    - Update CHANGELOG ([`c8abe3f`](https://github.com//sharkdp/bat/commit/c8abe3f235cd1b25e93d21c5f2c2d058ac64a7eb))
    - Fix import in app ([`5d71056`](https://github.com//sharkdp/bat/commit/5d710562091d1be4bd592d169a2ecb8ff3e7744a))
    - Update documentation ([`eb7d4d0`](https://github.com//sharkdp/bat/commit/eb7d4d002a68b57129e1db53438849739255946e))
    - Make module private ([`62b32bd`](https://github.com//sharkdp/bat/commit/62b32bd848b9b861cce8fc7caf7d10b78e4cb6dd))
    - Move PagingMode to separate module ([`8961f7a`](https://github.com//sharkdp/bat/commit/8961f7aef8a24e35007b6e6d4eed5bdaa30763a9))
    - Simpler highlight method ([`17f3a3b`](https://github.com//sharkdp/bat/commit/17f3a3b95da60b27fcd341b0a47a76aaa2784bc5))
    - Add *_with_name methods ([`261a7ea`](https://github.com//sharkdp/bat/commit/261a7ea1549cbcd01986f891bbaf1b5936e7641d))
    - Add syntaxes and themes method ([`53a973e`](https://github.com//sharkdp/bat/commit/53a973e9dd9ecf102d7bcc345846f2632585159a))
    - Add paging to advanced example ([`cba9df7`](https://github.com//sharkdp/bat/commit/cba9df746e9a719c59563962ff815210d8304833))
    - Fix warnings for --no-default-features mode ([`12eee0c`](https://github.com//sharkdp/bat/commit/12eee0c59056dbc131214139e01e5373cb233803))
    - Use unwrap ([`b76f5e7`](https://github.com//sharkdp/bat/commit/b76f5e72d44cdf5850268cc4a867516953436092))
    - Rename error module ([`702cb19`](https://github.com//sharkdp/bat/commit/702cb198da986203aefe9c2fa6d024d8cbe96346))
    - Add YAML example ([`5f82641`](https://github.com//sharkdp/bat/commit/5f826419d13e74fcdea851947364a1d5d69d152e))
    - Update examples ([`13f671b`](https://github.com//sharkdp/bat/commit/13f671b49984ff68ca77e213429f00173b8a6755))
    - Fix example ([`49f4322`](https://github.com//sharkdp/bat/commit/49f43220543883d78183c12373b00707e5eaab62))
    - Update 'cat' example ([`0c9e044`](https://github.com//sharkdp/bat/commit/0c9e044e4152c403691640d8c6b44fb4120554ec))
    - Add advanced example ([`74d4377`](https://github.com//sharkdp/bat/commit/74d4377ed28c4350807c967597ba5081f2ce7a74))
    - Easier configuration of style components ([`6a12459`](https://github.com//sharkdp/bat/commit/6a124591df487d0872a0d4963907a69f77fe68ee))
    - Fix #937 ([`7a87315`](https://github.com//sharkdp/bat/commit/7a87315b940ae7b8a38e83bddedd8f8b908c4c7c))
    - Numbers => LineNumbers ([`0cde4e9`](https://github.com//sharkdp/bat/commit/0cde4e9121a34390688825c09dc170a31c04a075))
    - Simplify style_components ([`36dde92`](https://github.com//sharkdp/bat/commit/36dde9275af3f513b77395a92a0023f7e6134fa6))
    - Rename wrap => wrapping ([`a8f759c`](https://github.com//sharkdp/bat/commit/a8f759c0809fab3839c01e83440700740601f246))
    - rename run => print ([`f034be7`](https://github.com//sharkdp/bat/commit/f034be71e7c1e8336d04ad3c6fdedca899c37d63))
    - Update documentation ([`eee35e5`](https://github.com//sharkdp/bat/commit/eee35e54e962898dfd8be1b6b0d48d885a4f0319))
    - Better API for highlighting lines ([`ec0ce05`](https://github.com//sharkdp/bat/commit/ec0ce054559e3821ddfcbbd0fa60b0b30117b6fa))
    - Reduce public API ([`13e6b3f`](https://github.com//sharkdp/bat/commit/13e6b3fac7440a487f4c5952345f20ac681b571f))
    - Fix warnings, sort imports, input from string ([`26c951f`](https://github.com//sharkdp/bat/commit/26c951fec485e21e2f28f6704590b55c16f00257))
    - Completely refactor 'input' module ([`590960f`](https://github.com//sharkdp/bat/commit/590960f7f5b5454c2da8c36409a8d57a58b2980c))
    - Fix comment ([`b4d5410`](https://github.com//sharkdp/bat/commit/b4d54106fefebd2a3d41323084eea8c1a199489e))
    - Add InputDescription ([`f3b90dd`](https://github.com//sharkdp/bat/commit/f3b90ddb383cfcbd575f24785a5a9400870d162e))
    - Allow fluent style ([`3bacfc5`](https://github.com//sharkdp/bat/commit/3bacfc5184184c4a845d56f9e7226e87eae6d172))
    - Rename InputFile => Input ([`f8d0956`](https://github.com//sharkdp/bat/commit/f8d0956893a0a6e42f9caddbb8e54a25c74d1166))
    - Separate inputs from config ([`1dc328a`](https://github.com//sharkdp/bat/commit/1dc328ad495edbd98bff9e89a059d1e66f9780ef))
    - Add all builder options ([`5e5cb89`](https://github.com//sharkdp/bat/commit/5e5cb89da6e778bb896b8bcc99caa1e1a65f18f3))
    - Large refactoring towards a better builder structure ([`057e4ec`](https://github.com//sharkdp/bat/commit/057e4eced1a74972dd5aef90c2c054acb7963c60))
    - Initial verison of PrettyPrinter builder ([`2797461`](https://github.com//sharkdp/bat/commit/27974616bfe7ec0c132fc259fdce1b471c99c93e))
    - Use 4-bit ANSI codes for base16 theme ([`319ab77`](https://github.com//sharkdp/bat/commit/319ab779ee6bc8954e7beb2ae34c43d9f327322e))
    - Fix README phrasing about dark mode ([`37831cd`](https://github.com//sharkdp/bat/commit/37831cdcb3cd5970fb07274a6eaf9338f15b9569))
    - Bump version to v0.14.0 ([`70480ee`](https://github.com//sharkdp/bat/commit/70480ee9d4e4169c5436f3ab3a7fa1c18ff1849e))
    - Update CHANGELOG ([`371c929`](https://github.com//sharkdp/bat/commit/371c929edee3331b008dffe5d32e779aa2600e93))
    - Pass --no-init on Windows if less version < 559 ([`864656b`](https://github.com//sharkdp/bat/commit/864656bd11d4f96235408e026f7ac68d86995919))
    - Fix compilation of library ([`2e9cf63`](https://github.com//sharkdp/bat/commit/2e9cf63a5fd63bd5bcf16cfce67b5999042b89cb))
    - Do not store metadata.yaml file in git ([`886b22e`](https://github.com//sharkdp/bat/commit/886b22e0ee082d1096e6c8bc71e10270a9bd1d7a))
    - Add metadata information to cached assets ([`72618db`](https://github.com//sharkdp/bat/commit/72618db179e75f41c2df5303c4106c0c2443524b))
    - Make get_cache_dir private ([`c827336`](https://github.com//sharkdp/bat/commit/c8273369ccd49cfbdf00560251cf133925f0f084))
    - Typo in CHANGELOG ([`95a56a2`](https://github.com//sharkdp/bat/commit/95a56a279579c61bb08890527c517dc60c204d69))
    - Update default config file, add comment about tmux mouse scrolling support ([`859ff50`](https://github.com//sharkdp/bat/commit/859ff50766e877ac96f7350daa18d6e38fa1d30e))
    - Revert "Pass '--mouse' to less versions >= 551" ([`ce583eb`](https://github.com//sharkdp/bat/commit/ce583eb9bfd60e7fcccd964d03298353cd6d62c7))
    - Revert "Only enable --mouse when running from tmux, see #904" ([`3a195be`](https://github.com//sharkdp/bat/commit/3a195be14e59ddf1c70fa672b530b91ded70c39f))
    - Only enable --mouse when running from tmux, see #904 ([`5f6e310`](https://github.com//sharkdp/bat/commit/5f6e310152f45f5a90a8e4b7312e85ec284745db))
    - Remove invalid UTF-8 file from repo, use temp file instead ([`5449472`](https://github.com//sharkdp/bat/commit/5449472f15e4b5f063f9933fe426c8d3138bfa45))
    - Fix bug for file with invalid-utf8 filenames ([`82e20bf`](https://github.com//sharkdp/bat/commit/82e20bfe14b05c3b3224cb1feb529c655a1e74e3))
    - Patch Rust syntax file ([`47abb19`](https://github.com//sharkdp/bat/commit/47abb192bc7476379986b13834d7c02406ed6a6b))
    - Disable Travis caching ([`03a7d6b`](https://github.com//sharkdp/bat/commit/03a7d6be92f50404163f4fc3231c26dc638a00fa))
    - Update CHANGELOG ([`a927815`](https://github.com//sharkdp/bat/commit/a9278152924d712634326883f8a54bb8f70a2cb2))
    - Update dependencies ([`3447a03`](https://github.com//sharkdp/bat/commit/3447a03433378286d4b4becf76ec12cc892d0803))
    - Add do-not-panic! regression tests ([`03c2281`](https://github.com//sharkdp/bat/commit/03c22818286e5a4606d14e7c48488382b5367bb8))
    - Pass '--mouse' to less versions >= 551 ([`c386cb3`](https://github.com//sharkdp/bat/commit/c386cb35fb5f5fd2d2d3dedcb841812982728c2d))
    - Update CHANGELOG ([`97ea3dd`](https://github.com//sharkdp/bat/commit/97ea3dd4dc73c48e29c782ef5a6846b10a76d5bd))
    - Improve error message ([`a6d9d15`](https://github.com//sharkdp/bat/commit/a6d9d1551fb54ee2cb190a62a989defaf3461dcc))
    - Small refactoring, handle invalid UTF-8 filenames ([`34619a2`](https://github.com//sharkdp/bat/commit/34619a2e89b8f65d362a596b86337ca17409f13c))
    - Extract common syntax functionality to helper fns ([`83d408b`](https://github.com//sharkdp/bat/commit/83d408bab3ea93314d4b0d7e3a496c8601d3fec7))
    - Fix build on 1.37 ([`8067fd9`](https://github.com//sharkdp/bat/commit/8067fd9eda6147b7e80a6520b677721deb4680b5))
    - Use to_string_lossy ([`98ba9e0`](https://github.com//sharkdp/bat/commit/98ba9e010162ed2cd7714c95c505ea0839e28395))
    - Pass stdin as a generic BufRead, fix stdin tests ([`1b8ce60`](https://github.com//sharkdp/bat/commit/1b8ce60054306b6ab1e7ec50938557cef9be54f6))
    - Remove commented-out code ([`d5a31dc`](https://github.com//sharkdp/bat/commit/d5a31dc2ecfcb6304eacd1fbe7672ac822831c57))
    - Make filename method private ([`051dd4b`](https://github.com//sharkdp/bat/commit/051dd4ba870a437fddefe132c2da2a75ec7e1e51))
    - Do not take optional as argument ([`2ad1848`](https://github.com//sharkdp/bat/commit/2ad18488597e98e55e924321c843653ee4274f39))
    - Rename field name, new constructors ([`90e7d2f`](https://github.com//sharkdp/bat/commit/90e7d2fe335052e7bd83a48a8925934978dcc37b))
    - Moved user_provided_filename to be contained within OrdinaryFile struct ([`04fa84a`](https://github.com//sharkdp/bat/commit/04fa84aea7d02074bc650a552abb397b35c6720c))
    - Use --file-name to detect syntax highlighting ([`a3f8140`](https://github.com//sharkdp/bat/commit/a3f8140fbe89d4980014e8af4e34469202ce2799))
    - Clarify the Ubuntu/Debian install instructions ([`4cc2989`](https://github.com//sharkdp/bat/commit/4cc2989fe95b064baeee09e1f57586c74e2ce043))
    - Add performance patch for Makefile syntax ([`0ecc949`](https://github.com//sharkdp/bat/commit/0ecc94956b88beed27ca13a130c8ba09f1a220d8))
    - Move Rego syntax ([`f5145ef`](https://github.com//sharkdp/bat/commit/f5145ef1300ff89b671d6cf80de512397f3a19c0))
    - Add a way to directly search for a pattern ([`edd2764`](https://github.com//sharkdp/bat/commit/edd27645d07e4842e1feca5851964695d9cd8bf7))
    - Add documentation ([`fc6f4f3`](https://github.com//sharkdp/bat/commit/fc6f4f31dd7cc9741a313cc010f2ec1407eb45af))
    - Add script to find slow-to-highlight files ([`f2cef70`](https://github.com//sharkdp/bat/commit/f2cef702a02265fef883efba58ef0de5080916f7))
    - Update CHANGELOG ([`8b3d54c`](https://github.com//sharkdp/bat/commit/8b3d54ce29b07b53b6dc422a956b39392b04acc3))
    - Add patch for C# syntax ([`47a3721`](https://github.com//sharkdp/bat/commit/47a37218900d9a40d7704973c015eef7bee3024a))
    - cargo fmt ([`495fab2`](https://github.com//sharkdp/bat/commit/495fab24a57b75b0b773acb708164950221d3800))
    - Add regex-onig feature to 'cargo check' tests ([`fccbe4f`](https://github.com//sharkdp/bat/commit/fccbe4f4f29748acc7a982a0fafaa5a93962b7aa))
    - Update to syntect 4.1, fancy-regex support ([`9793bb3`](https://github.com//sharkdp/bat/commit/9793bb3938820f2fdea32c1e17fe360753538fba))
    - Add -i option for bash/zsh to output wrapper function ([`496dd29`](https://github.com//sharkdp/bat/commit/496dd29cb96f79e9ac77668c685bd248849cfd4b))
    - Reformat info.sh ([`116d76a`](https://github.com//sharkdp/bat/commit/116d76a00f968cfd225d7186c335717761609bd9))
    - Fix wrapper detection failing on non-English LANG in info.sh ([`dd336e6`](https://github.com//sharkdp/bat/commit/dd336e6fb2625992ace63a836415fd61e9cbef41))
    - Fix missing shell '-i' option when checking for wrapper in info.sh ([`c33af35`](https://github.com//sharkdp/bat/commit/c33af3581aa641831662e476cfe751e67b626f39))
    - Add zsh wrapper detection to info.sh ([`2f4ecf8`](https://github.com//sharkdp/bat/commit/2f4ecf8fc5492bfbb6753983e32d7e45163c5096))
    - Add custom theme/syntax detection to info.sh ([`e037afe`](https://github.com//sharkdp/bat/commit/e037afeacaa33ce08e08dd123c13a26e3d8067e1))
    - Update dependencies, closes #476 ([`7b87af1`](https://github.com//sharkdp/bat/commit/7b87af1748c315627ea0246cc8fecb95abe5022f))
    - Bump assert_cmd from 0.12.0 to 1.0.1 ([`0eee26f`](https://github.com//sharkdp/bat/commit/0eee26fb0914e826c9672cf280cc40fb3c666b14))
    - Update CHANGELOG ([`3355aeb`](https://github.com//sharkdp/bat/commit/3355aeba22948df238c1f7a4495b01e3bcb2bc81))
    - Updates for review of PR 899 ([`42e3825`](https://github.com//sharkdp/bat/commit/42e3825dafcfc0255471c2213d3c97ddb5a21fa6))
    - Move paging support behind a feature ([`014d754`](https://github.com//sharkdp/bat/commit/014d7545889667e62eb30f6a5717f00a90616c06))
    - Move git changes support behind a feature ([`4e11abd`](https://github.com//sharkdp/bat/commit/4e11abdf9b0b28dfda2ec4904ddf0a82e524a847))
    - Strip dependencies of bat-as-a-library ([`570805b`](https://github.com//sharkdp/bat/commit/570805bc985227a8e612878d8f62becd70b7b16a))
    - Allow using bat-as-a-library with older syntect ([`e7e1967`](https://github.com//sharkdp/bat/commit/e7e1967bb0c4ef4a5d90f093e2562b214e28c7c8))
    - Update CHANGELOG ([`9a050cd`](https://github.com//sharkdp/bat/commit/9a050cd87fde4309db5af0179a4773fe97d3d846))
    - Add OPA Rego syntax ([`d395078`](https://github.com//sharkdp/bat/commit/d39507889a9e9596db5bde3b8ae8d36fa750e20a))
    - Fix for Rust 1.37 ([`3e8d444`](https://github.com//sharkdp/bat/commit/3e8d444c7824d50050dfc4fdcfe6c7bdff62ec85))
    - Add entries for upcoming release ([`522ab7a`](https://github.com//sharkdp/bat/commit/522ab7a83c779af61670a7284b69185b4f63f7bb))
    - Add CHANGELOG to the repository ([`0c5b4fc`](https://github.com//sharkdp/bat/commit/0c5b4fcd4ad56f8af0ce492146494b25f2b95623))
    - Merge pull request #871 from neuronull/fix_654_stdin_filename ([`37b3b87`](https://github.com//sharkdp/bat/commit/37b3b8730d313cef0fc9faa1e1ecc98406c62ed7))
    - Document the --generate-config-file option ([`7c50fe5`](https://github.com//sharkdp/bat/commit/7c50fe5fecf94199a43a07db3d329135fb012cea))
    - Graceful handling of error conditions ([`40a827e`](https://github.com//sharkdp/bat/commit/40a827ebcb8de132e346c0104b419dc2b8263782))
    - Add option to generate a default config file, fixes #870 ([`376c556`](https://github.com//sharkdp/bat/commit/376c5568628c6d95a8b0b16ddf99055c8ea93e89))
    - fix manpage syntax link ([`4aef8c1`](https://github.com//sharkdp/bat/commit/4aef8c180a7a889a81968c3cc77077786dc8d4c1))
    - Minor --file-name code hygeine ([`83772bd`](https://github.com//sharkdp/bat/commit/83772bd2cfd2603fd4185ee26cdd883383ad46cb))
    - Merge branch 'fix_654_stdin_filename' of github.com:neuronull/bat into fix_654_stdin_filename ([`131d113`](https://github.com//sharkdp/bat/commit/131d113ff5c4e873e1c0c742136d2f2001ed3e76))
    - Merge branch 'master' into fix_654_stdin_filename ([`38178fe`](https://github.com//sharkdp/bat/commit/38178fedf463cb704509b772d5e274461086d368))
    - Implemented --file-name for multiple files + tests ([`59f2e2d`](https://github.com//sharkdp/bat/commit/59f2e2d58d3dc8ca31b74b9bad5899c4521baf0c))
    - Adds `Homepage` to Debian package `control` file ([`8d3136e`](https://github.com//sharkdp/bat/commit/8d3136eb6f94a1a4dffd9cf6a88d083c693d1616))
    - Update liquid to 0.20 ([`136a745`](https://github.com//sharkdp/bat/commit/136a745c5de804ee9cf0ec601302e17cef6334a3))
</details>

# v0.13.0 (2020-03-22)

## `bat` as a library

Beginning with this release, `bat` can be used as a library (#423).

This was a huge effort and I want to thank all people who made this possible: @DrSensor, @mitsuhiko, @mre, @eth-p!

- Initial attempt in #469 (@mitsuhiko)
- Second attempt, complete restructuring of the `bat` crate, see #679 (@DrSensor)
- Updates to example, public API, error handling, further refactoring: #693 #873 #875 (@sharkdp)

I want to stress that this is the very first release of the library. Things are very likely to change. A lot of things are still missing (including the documentation).

That being said, you can start using it! See the example programs in [`examples/`](https://github.com/sharkdp/bat/tree/master/examples).

You can see the API documentation here: https://docs.rs/bat/

## Bug Fixes

 - <csr-id-260c5dcb75f6e868be784359f6eddbe7facda777/> doc/README-ja.md
   - Synchronized the contents of `README.md`.

## New Features

 - <csr-id-83dc8468b0321958c119ad7c51655db2ddbe87ac/> Solarized light and dark themes

## Features

- (**Breaking change**) Glob-based syntax mapping, see #877 and #592. With this change,
  users need to update their bat config files (`bat --config-file`), if they have any `--map-syntax` settings
  present.

  The option now works like this:
  ```bash
  --map-syntax <glob-pattern>:<syntax-name>
  ```

  For more information, see the `--help` text, the man page or the README.

  This new feature allows us to properly highlight files like:
  * `/etc/profile`
  * `~/.ssh/config`

- `--highlight-line` now accepts line ranges, see #809 (@lkalir)
- Proper wrapping support for output with wide Unicode characters, see #811 #787 and #815 (@Kogia-sima)
- A lot of updates to existing syntaxes via #644 (@benwaffle, @keith-hall)
- `BAT_CACHE_PATH` can be used to place cached `bat` assets in a non-standard path, see #829 (@neuronull)
- Support combination of multiple styles at the same time, see #857 (@aslpavel)

## Bugfixes

- Do not pass '--no-init' on newer less versions, see #749 and #786 (@sharkdp)
- 'bat cache' still takes precedence over existing files, see #666 (@sharkdp)
- `.sc` files should be treated as scala files, see #443 (@benwaffle)
- Allow underscores and dashes in page names, see #670 (@LunarLambda)
- Keep empty lines empty, see #601 (@mbarbar)
- Wrapping does not work when piping, see #758 (@fusillicode, @allevo, @gildo)
- Allow for non-unicode filenames, see #225 (@sharkdp)
- Empty file without header produces incomplete grid, see #798 (@eth-p)
- Files named `build` don't respect shebang lines, see #685 (@sharkdp)

## Other

- Parametrizable names for man page and shell completion files, see #659 #673 #656 (@eth-p)
- Enabled LTO, making `bat` about 10% faster, see #719 (@bolinfest, @sharkdp)
- Suggestions non how to configure `bat` for MacOS dark mode, see README (@jerguslejko)
- Extended ["Integration with other tools"](https://github.com/sharkdp/bat#integration-with-other-tools) section (@eth-p)
- Updated [instrutions on how to use `bat` as a `man`-pager](https://github.com/sharkdp/bat#man), see #652, see #667 (@sharkdp)
- Add section concerning file encodings, see #688 and #568 (@sharkdp)
- Updated sort order of command-line options in `--help` text and manpage, see #653 and #700 (@hrlmartins)
- Updates to the man page syntax, see #718 (@sharkdp)
- Japanese documentation updates, see #863 (@k-ta-yamada, @sorairolake and @wt-l00)
- Accept "default" as a theme, see #757 (@fvictorio)
- Updated Windows installation instructions, see #852 (@sorenbug)
- Updated man page, see #573 (@sharkdp)

## New syntaxes

- Jinja2, see #648 (@Martin819)
- SaltStack SLS, see #658 (@Martin819)
- `/etc/fstab`, see #696 (@flopp and @eth-p)
- `/etc/group` and `/etc/passwd`, see #698 (@argentite)
- `/proc/cpuinfo` and `/proc/meminfo`, see #593 (@sharkdp)
- Nim, see #542 (@sharkdp)
- Vue, see #826 (@chaaaaarlotte)
- CoffeScript, see #833 (@sharkdp)

## New themes

- Dracula, see #687 (@clarfon)
- Nord, see #760 (@crabique)
- Solarized light and dark, see #768 (@hakamadare)

## Packaging

- `bat` is now in the official Ubuntu and Debian repositories, see #323 and #705 (@MarcoFalke)
- `bat` can now be installed via MacPorts, see #675 (@bn3t)
- Install fish completions into 'vendor_completions.d', see #651 (@sharkdp)

## Thanks

- To @eth-p for joining me as a maintainer! I'm very grateful for all the work you put into
  managing and responding to issues, improving our deployment, adding PR/issue templates (#837) as
  well as fixing bugs and implementing new features.

## Commit Statistics

<csr-read-only-do-not-edit/>

 - 252 commits contributed to the release over the course of 197 calendar days.
 - 3 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' where seen in commit messages

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Fix path to assets ([`5edd826`](https://github.com//sharkdp/bat/commit/5edd8260e8fef887bd4c7e39c411dfe843d03894))
    - Write completions and manfile to cargo outdir (cargo publish restriction) ([`02f2b55`](https://github.com//sharkdp/bat/commit/02f2b55d5f50d7c897b63584f8732b3033e645d5))
    - Update dependencies, see #845 ([`6386a4b`](https://github.com//sharkdp/bat/commit/6386a4b812ae91b236c62c3cfe813aebf9359ad1))
    - Bump version to 0.13, see #845 ([`c13935f`](https://github.com//sharkdp/bat/commit/c13935f3c06f359cb4990dbbf0d983a029eca291))
    - --file-name for normal files. integration tests. ([`fb3c775`](https://github.com//sharkdp/bat/commit/fb3c775c8b4cf25b648bbdc422b99f93608914c9))
    - Implement --file-name<name> option ([`8adce9f`](https://github.com//sharkdp/bat/commit/8adce9fae8eb9ceb5dac44546a35c592378607fc))
    - Rename test methods ([`eb2f3dd`](https://github.com//sharkdp/bat/commit/eb2f3ddb7cedff7331d8f296275f8d83d10b63be))
    - Add more default mappings, reverse traversal ([`dfd3ef0`](https://github.com//sharkdp/bat/commit/dfd3ef022ec68195a47b2f76ef683125b8d46d44))
    - Update documentation ([`978def2`](https://github.com//sharkdp/bat/commit/978def2d40914a83e245fdbb82b8b1ba9fcf81b5))
    - Use absolute path for lookup in database ([`57aed07`](https://github.com//sharkdp/bat/commit/57aed0781447c21fd3cf270509326c0010f132e5))
    - Initial implementation of glob-based syntax mapping ([`bd8a13d`](https://github.com//sharkdp/bat/commit/bd8a13dbc920b8739ef0f5e3fa84d90b3a24608b))
    - Remove superfluous files ([`ba29e07`](https://github.com//sharkdp/bat/commit/ba29e076366b13b03e5258c65a16d35079fcec55))
    - Move syntaxes, fixes #874 ([`3234661`](https://github.com//sharkdp/bat/commit/32346612b7ae6902601bf9b2faf110f4dde04723))
    - Cargo fmt ([`0550a28`](https://github.com//sharkdp/bat/commit/0550a286bdfefd8673e4689ca39aae8e2c78fb78))
    - Add an even simple example ([`dfe5eb3`](https://github.com//sharkdp/bat/commit/dfe5eb3e989e87db04a3af900d2c49c49308bc0b))
    - Further simplify module structure ([`2c2861d`](https://github.com//sharkdp/bat/commit/2c2861db6abfdcccdb6ca144c35ad5012efb0b6c))
    - Simplify public API module structure ([`bb0a3d5`](https://github.com//sharkdp/bat/commit/bb0a3d586ef00dfa1fc77b665c0ae5735a05b5b9))
    - Hide SyntaxMapping::replace ([`90397a8`](https://github.com//sharkdp/bat/commit/90397a8aac02a8be0250ca54e3fcb6c348dee52d))
    - Rename output component to 'style compinent', move wrap to separate module ([`84ba323`](https://github.com//sharkdp/bat/commit/84ba323b1c3c2fec36ea3999e8675379dc64f50d))
    - Hide methods from line_range ([`a8851e1`](https://github.com//sharkdp/bat/commit/a8851e1307cdae977ac49c2618a8a00e22000d28))
    - Hide API of InputFileReader ([`d4a39f9`](https://github.com//sharkdp/bat/commit/d4a39f99d2f72553daa874e9706218fc8e1367cf))
    - Hide get_syntax, move tests to module ([`8c500a7`](https://github.com//sharkdp/bat/commit/8c500a7e405964b9c60c04f7176bf91faacfd824))
    - Rename methods and parameters ([`acf51bd`](https://github.com//sharkdp/bat/commit/acf51bd7b3fdec64b1e5fe77fa09260135dadcbd))
    - Add fallback theme, remove BAT_THEME_DEFAULT ([`83dc13a`](https://github.com//sharkdp/bat/commit/83dc13a86d4ddc00be23e22f81843bf025517c08))
    - Hide some methods from HighlightingAssets ([`094c526`](https://github.com//sharkdp/bat/commit/094c526a0e5264b509f55c0338970e2f9ed29e9f))
    - Do not export syntax_set and theme_set ([`62f2d0c`](https://github.com//sharkdp/bat/commit/62f2d0c1006937ad4204623e79190d37ba0bd48f))
    - Move error handling to a callback ([`fc1ca08`](https://github.com//sharkdp/bat/commit/fc1ca0875a569f9fd956ae2b424c0ed235f28d27))
    - Run cargo fmt ([`a7338e2`](https://github.com//sharkdp/bat/commit/a7338e2ea2b6fd548c2d896c0224cb3f3d44d91a))
    - move Config struct to separate file ([`9b8ddb2`](https://github.com//sharkdp/bat/commit/9b8ddb24d1de32f82c8da1ff04967ee47a544533))
    - Move error module to separate file ([`7e01156`](https://github.com//sharkdp/bat/commit/7e0115641d2e0582d2abe93c18cec354090ea7d1))
    - Restructure and fix bug in line range module ([`fedd321`](https://github.com//sharkdp/bat/commit/fedd32173e58002dfd5da6ba7ec156a74121c3ff))
    - Rename enum field ([`4855a47`](https://github.com//sharkdp/bat/commit/4855a47105c4cb8c8049cf751dace2aab3e4f2b3))
    - Major restructuring of theme/syntax handling ([`06b7be7`](https://github.com//sharkdp/bat/commit/06b7be7ee90a336bd1f51deea1e90f357cb01916))
    - --file-name for normal files. integration tests. ([`cfa2cb6`](https://github.com//sharkdp/bat/commit/cfa2cb6ec77f8faaab134c296350b982ff1ffe40))
    - remove unnecessary Some() ([`b1b8add`](https://github.com//sharkdp/bat/commit/b1b8addf7e5f7b06b741caf7bdf11c795795ae1f))
    - remove absolute path restraint from BAT_CACHE_PATH ([`4be5adc`](https://github.com//sharkdp/bat/commit/4be5adc6f55dcb4dad4e610aab41cde87b85e55c))
    - Specify cache dir with BAT_CACHE_DIR ([`a9a31dc`](https://github.com//sharkdp/bat/commit/a9a31dca7fce553d0180ae33f6229922a4354900))
    - Implement --file-name<name> option ([`517be5c`](https://github.com//sharkdp/bat/commit/517be5c7bcb034bda56ba8572f2e5f103163bf58))
    - Remove custom .github settings ([`9a9902b`](https://github.com//sharkdp/bat/commit/9a9902bf9ba8d286b1eb70fc496453865e9adf03))
    - Major update for the man page, closes #573 ([`4e40d4c`](https://github.com//sharkdp/bat/commit/4e40d4c7204f7f966bbf798f180a1793f22b9258))
    - Update assets ([`916c1e4`](https://github.com//sharkdp/bat/commit/916c1e435916f747b97955f1113d360a17a519af))
    - Remove duplicate F# syntax ([`439be03`](https://github.com//sharkdp/bat/commit/439be036345fa4475645698df854be89f9fbbe44))
    - Add no-duplicate-extensions unit test ([`b611d2a`](https://github.com//sharkdp/bat/commit/b611d2aef4056c318e460bd12ad3de6b4014d6cb))
    - Update git dependency ([`a2075b0`](https://github.com//sharkdp/bat/commit/a2075b0f24b458e029495bb7f3b45bb40badef79))
    - Update links ([`3c1dd85`](https://github.com//sharkdp/bat/commit/3c1dd8558d50e2c44e0e4c30907f8e139e89c8f6))
    - Update dependencies ([`17382d3`](https://github.com//sharkdp/bat/commit/17382d36b7a56f90c313c3a0de4bf5bb078a2286))
    - fix links ([`2e07aab`](https://github.com//sharkdp/bat/commit/2e07aabb746cddbcd76694b155194d579c23e44f))
    - fix typo and broken links ([`f525fb9`](https://github.com//sharkdp/bat/commit/f525fb974e25a3ad8ae219a9b73ee011cf987ccc))
    - add README-ko.md ([`edf6295`](https://github.com//sharkdp/bat/commit/edf62953c4a00d2da71a9e650ccb8b829a9ccdd1))
    - doc/README-ja.md ([`260c5dc`](https://github.com//sharkdp/bat/commit/260c5dcb75f6e868be784359f6eddbe7facda777))
    - Add syntax detection unit tests ([`1bc62cd`](https://github.com//sharkdp/bat/commit/1bc62cd7b5181d68f4bbc8acdb04d464d4a5f552))
    - Update to new assert_cmd ([`b1183f7`](https://github.com//sharkdp/bat/commit/b1183f72a5d31777a1865f6411b33e73c7f7e1cc))
    - Bump assert_cmd from 0.11.1 to 0.12.0 ([`a5e9814`](https://github.com//sharkdp/bat/commit/a5e98146fbe4025d9a8461795ee6e2e2560e4695))
    - Bump assert_cmd from 0.11.1 to 0.12.0 ([`6d32d3d`](https://github.com//sharkdp/bat/commit/6d32d3dbecf54dcb487030bdef787a262ae73efb))
    - Bump console from 0.9.2 to 0.10.0 ([`c6bbca6`](https://github.com//sharkdp/bat/commit/c6bbca62ef67508e4f717419935bca5aeaa74a55))
    - Update dependencies ([`dd9755f`](https://github.com//sharkdp/bat/commit/dd9755f223eca6e0bca1a6837f24861e60b119d5))
    - Install fish completions into 'vendor_completions.d', fixes #651 ([`61e3915`](https://github.com//sharkdp/bat/commit/61e39158de87b764b8f3bb549b5a224fda52f322))
    - Bump assets/syntaxes/Packages from `71fd0ec` to `759d6ee` ([`65023a9`](https://github.com//sharkdp/bat/commit/65023a987e9f7c8dd5f4aaa4dd388c4bc0837136))
    - Update 'JavaScript (Babel).sublime-syntax' ([`91843f6`](https://github.com//sharkdp/bat/commit/91843f6f65100e38be0dd0542984650c0b85531b))
    - Bump assets/syntaxes/JavaScript (Babel) from `a9a908f` to `f4579f9` ([`696547b`](https://github.com//sharkdp/bat/commit/696547b6469e1d46ec02bcfe0079f413b2ec8119))
    - Update TypeScript.sublime-syntax ([`4d26823`](https://github.com//sharkdp/bat/commit/4d2682325b96f0fc04f72bd1e2c46c65cb0c9bd1))
    - Bump assets/syntaxes/TypeScript from `ab51614` to `9cd994a` ([`65811d9`](https://github.com//sharkdp/bat/commit/65811d9034b0f4100ef2595d00e8f2c2a75bc818))
    - Add link to Choco package ([`f2b7c06`](https://github.com//sharkdp/bat/commit/f2b7c060a346dec379a9829ac531c381ceeccbe8))
    - Update Windows install instructions ([`08dd251`](https://github.com//sharkdp/bat/commit/08dd251b9c1b2e618e21dbf111c6e510b8ce7687))
    - Update README.md ([`d081473`](https://github.com//sharkdp/bat/commit/d081473261dc9555e8e026d0c9636f70e7130cc8))
    - Move VC++ to Windows section ([`3901480`](https://github.com//sharkdp/bat/commit/3901480c5972b2d011dd9f8e04fa99ced632e0c0))
    - Explain how to fix no output issue ([`1cad3f5`](https://github.com//sharkdp/bat/commit/1cad3f56ec68e32d10f4c42f7234f5961001566c))
    - Add CoffeeScript syntax, closes #833 ([`147503b`](https://github.com//sharkdp/bat/commit/147503bd568e254464b47e0ec42582c83ff908ef))
    - Update dependencies ([`d1b0e95`](https://github.com//sharkdp/bat/commit/d1b0e955eaf77951db295a0b1d3d94b0a36183d7))
    - Support combination of multiple styles at the same time ([`4021cf8`](https://github.com//sharkdp/bat/commit/4021cf812828d59e17459570631954ec27e43434))
    - Merge pull request #854 from eth-p/master ([`7155f76`](https://github.com//sharkdp/bat/commit/7155f7696368c6a6a92c6b27d2bcbe1f53ef229d))
    - Fix broken link in issue template. ([`b63f31e`](https://github.com//sharkdp/bat/commit/b63f31e15debd30e2ede6cc59cbe39e7c6792c5a))
    - Bump assets/syntaxes/ssh-config from `e84b78f` to `16e19d5` ([`9c3d8c4`](https://github.com//sharkdp/bat/commit/9c3d8c4028b02d1ad89656214ce6c7e9e5a7aa72))
    - Bump assets/syntaxes/GraphQL from `66ce871` to `c9d8458` ([`d33b707`](https://github.com//sharkdp/bat/commit/d33b7071e39a4e6b3f8a88acb21a2b0337ed6dce))
    - Run 'cargo fmt' ([`ca066bf`](https://github.com//sharkdp/bat/commit/ca066bf7cd9919816c47c8c0dc28f16c61766889))
    - Rename label syntax-request ([`4ac3161`](https://github.com//sharkdp/bat/commit/4ac3161c19a234b81a7e6657ccae011413c75451))
    - Rename label ([`d479e07`](https://github.com//sharkdp/bat/commit/d479e07bcb02185f821177eae15384e0a0674e65))
    - Remove title prefix (we already have labels for that) ([`369ba7b`](https://github.com//sharkdp/bat/commit/369ba7bb0a26c8482596c298c0d27eefcfe1169a))
    - Add 'question' template ([`9df85ed`](https://github.com//sharkdp/bat/commit/9df85edc4cd59074f90534943b6c2a60470175ba))
    - Add simple "Feature request" template ([`d007caf`](https://github.com//sharkdp/bat/commit/d007caf39c97dc1b2bc14c11e5319352cf48c5b0))
    - Simplify issue templates ([`7d453df`](https://github.com//sharkdp/bat/commit/7d453dfdc590251a84887676488930f76ab5e155))
    - Update test data ([`60f48a0`](https://github.com//sharkdp/bat/commit/60f48a0c91e76c3e957ca7442b23009e9001d29d))
    - Fix bug where max_width is not initialized after line break ([`25640a8`](https://github.com//sharkdp/bat/commit/25640a8a81d28384aa58a000c91832f7159d60a9))
    - Fix wrapping method to support unicode text ([`944866a`](https://github.com//sharkdp/bat/commit/944866affdf919ee1b1a7cbb1dcd9d613f863a9b))
    - Add test for unicode wrapping ([`22ded00`](https://github.com//sharkdp/bat/commit/22ded008240fbbc8505429cb163a7a115087b25c))
    - Add unicode-width dependency ([`85e84ab`](https://github.com//sharkdp/bat/commit/85e84ab3eb85ee349dae6ceb14c00f58656033c9))
    - Change bug_report template to accomodate info.sh output ([`993b766`](https://github.com//sharkdp/bat/commit/993b76654dd08a929c7acd0aaf487eae58ed964b))
    - Change output format of info.sh to markdown ([`2b2743b`](https://github.com//sharkdp/bat/commit/2b2743b1209803f226a93f33ca88d1a46c6f4dfa))
    - Fix typo causing missing command warning in info.sh ([`f01263b`](https://github.com//sharkdp/bat/commit/f01263bcff17460450159d9c721bf2c6920f9aa6))
    - Add detection of a wrapper function to info.sh ([`8648b86`](https://github.com//sharkdp/bat/commit/8648b86a73542a4ec65157218cb03d7135d9f72f))
    - Change a word in bug_report.md ([`39d9343`](https://github.com//sharkdp/bat/commit/39d9343a7d3f1e1292e8d558789812ec90a080e1))
    - Change ISSUE_TEMPLATE files based on feedback in #837 ([`5faf7ca`](https://github.com//sharkdp/bat/commit/5faf7ca158356fdb43e71a4244783b7cae136830))
    - Change consent banner (and fix tput) in info.sh ([`ebe4752`](https://github.com//sharkdp/bat/commit/ebe475247c736db09e8d8a3af1aa203419b95972))
    - Add issue templates ([`63f6343`](https://github.com//sharkdp/bat/commit/63f6343cf137b97dc39f84c1a2312e83cc0ba32e))
    - Add script to use for diagnosing issues ([`ad42b27`](https://github.com//sharkdp/bat/commit/ad42b275a936b9ac07c9dcb292dcb958f683480a))
    - Add @sharkdp's regression test for #798 ([`2dac1c8`](https://github.com//sharkdp/bat/commit/2dac1c878543971946067e059132edff83e37a49))
    - Fix #798 ([`1225e65`](https://github.com//sharkdp/bat/commit/1225e65299ceee6f6eda0d144f38c309cab59be3))
    - Bump min. required Rust version to 1.36 ([`d1bb47d`](https://github.com//sharkdp/bat/commit/d1bb47d7419372e4b5925a22430fdbf830e8b031))
    - Bump git2 from 0.11.0 to 0.12.0 ([`f079c14`](https://github.com//sharkdp/bat/commit/f079c140682fa8b9edd628dcd597c4e5c4b155cd))
    - Add ja-transration of Paging on Using bat on Windows ([`388f4c7`](https://github.com//sharkdp/bat/commit/388f4c71fdd53c6aed35129215b8b5b08163e53f))
    - Add Vue syntax ([`b675e1b`](https://github.com//sharkdp/bat/commit/b675e1bb6131c95b678e80e0f036995102d1e454))
    - Bump error-chain from 0.12.1 to 0.12.2 ([`f96220f`](https://github.com//sharkdp/bat/commit/f96220f33db598d4bdfb181c80e765038251c11e))
    - Bump assets/syntaxes/ssh-config from `fcd4378` to `e84b78f` ([`6739230`](https://github.com//sharkdp/bat/commit/67392300399affef1fb3a87b946e206232d5d5fe))
    - Bump assets/syntaxes/Fish from `ad6a6ca` to `2c254cc` ([`710018b`](https://github.com//sharkdp/bat/commit/710018b845d00cbff0f29e48a650f6e5f020e78a))
    - Allow for non-unicode filenames, closes #225 ([`7779d9f`](https://github.com//sharkdp/bat/commit/7779d9f6221b3e98c43a43ceb5596ba285fdf4f8))
    - Bump assets/syntaxes/LESS from `8cd08e3` to `3020993` ([`e98f34b`](https://github.com//sharkdp/bat/commit/e98f34b1e8a420aad4eba313bd1604d640183413))
    - Bump assets/themes/dracula-sublime from `c31934f` to `811a634` ([`9ace915`](https://github.com//sharkdp/bat/commit/9ace915d7173ef8a6b832002b5db89e8bde3c410))
    - Bump console from 0.9.1 to 0.9.2 ([`e5c4155`](https://github.com//sharkdp/bat/commit/e5c415555d75d25d5cea1d41e83fd243801b0245))
    - Grammar touch-ups, more descriptive error message for invalid line ranges ([`5ef1c6c`](https://github.com//sharkdp/bat/commit/5ef1c6cce2bbe2065367b239c2db0547340ddea7))
    - Adds range syntax for line highlights ([`93881d9`](https://github.com//sharkdp/bat/commit/93881d9a6402631855f061adb7ca19d681c7d578))
    - Bump assets/syntaxes/TypeScript from `1d5160e` to `ab51614` ([`87776ae`](https://github.com//sharkdp/bat/commit/87776ae07e9cf17f893c299cc44318b6781f5a2d))
    - Bump assets/syntaxes/Terraform from `c683495` to `64208ea` ([`6c71a43`](https://github.com//sharkdp/bat/commit/6c71a4324b30ef48ad778643078ccad58cfeb3dd))
    - Add information about the license to README files ([`90ee939`](https://github.com//sharkdp/bat/commit/90ee93967115d97491f56ce198e9556008bc29f5))
    - Fix invalid link ([`cae06bf`](https://github.com//sharkdp/bat/commit/cae06bf649b66d69196e46c1317fb7a041416a83))
    - Update copyright year of the MIT License file ([`89220da`](https://github.com//sharkdp/bat/commit/89220da47b5e2a2f27d1b5cc69540e011abb848a))
    - Show `master` state ([`903af3e`](https://github.com//sharkdp/bat/commit/903af3ee096dd4c903700916cd9d37443ac9829f))
    - Bump atty from 0.2.13 to 0.2.14 ([`0418e62`](https://github.com//sharkdp/bat/commit/0418e6294b0f9e320a70634683921130a6aeeb84))
    - Simplify Travis caching ([`eabd100`](https://github.com//sharkdp/bat/commit/eabd100dad906b6606eed733424fcc2b07c6bcd5))
    - Bump assets/themes/dracula-sublime from `1d068cb` to `c31934f` ([`f66a5bf`](https://github.com//sharkdp/bat/commit/f66a5bfa4219ebb3a95ba275412758dac0fdce84))
    - Add test for less 529 ([`8d0b8a2`](https://github.com//sharkdp/bat/commit/8d0b8a227be8fbf053dba04344b454355ad28194))
    - Also pass '--no-init' if version could not be retrieved ([`e400ebd`](https://github.com//sharkdp/bat/commit/e400ebd1bdc6021d21cb8831caf2112caef55321))
    - Remove example line from config file ([`6684c0b`](https://github.com//sharkdp/bat/commit/6684c0b07afe985310275f736b0ae659f1aea0a1))
    - Update README ([`6568d03`](https://github.com//sharkdp/bat/commit/6568d0392488d0a38c7bda779216ac987a8b13cd))
    - Do not pass '--no-init' on newer less versions ([`67fe804`](https://github.com//sharkdp/bat/commit/67fe8042567254d21c82ecda9f116745e2438fc2))
    - Enable LTO ([`126729f`](https://github.com//sharkdp/bat/commit/126729f87a686fdca37e63e727801723e1549366))
    - Add simple /proc/meminfo syntax, see #593 ([`0aca275`](https://github.com//sharkdp/bat/commit/0aca27511abed7ab4d0900dd13e90d6eba343945))
    - Add simple /proc/cpuinfo syntax, see #593 ([`77ff853`](https://github.com//sharkdp/bat/commit/77ff8537391d0661f8f3938f06ee24cd1bdf20be))
    - Update help text, closes #713 ([`20ba984`](https://github.com//sharkdp/bat/commit/20ba984c6d73e59cfeb315c1d17962a10fc64e9c))
    - Add check for terminal-width in output_wrap config value in not interactive case ([`6311ca2`](https://github.com//sharkdp/bat/commit/6311ca22f9a6b413e5c3146c26966cdb321e493a))
    - Fix '日本文' to '日本語' ([`41d5475`](https://github.com//sharkdp/bat/commit/41d547532ff97ee19763c491d9ff2557ff163d50))
    - Bump assets/syntaxes/SCSS_Sass from `0e8e0f9` to `b98a3f3` ([`e9156f8`](https://github.com//sharkdp/bat/commit/e9156f80900a39195e84b150da74cc60c91135b2))
    - Bump assets/themes/dracula-sublime from `b7e8961` to `1d068cb` ([`1eaa681`](https://github.com//sharkdp/bat/commit/1eaa681c7e17012c872568f84a09922f519d3460))
    - Bump assets/syntaxes/Elixir from `044d9af` to `89b3bad` ([`d96ae09`](https://github.com//sharkdp/bat/commit/d96ae099ef290dc0eafbd7de0116b158cdc71ea4))
    - Minor change ([`40e5bef`](https://github.com//sharkdp/bat/commit/40e5befdc7c09edde6fe33d4dd79a9a2bf5d0762))
    - Accept "default" as a theme ([`f94ff93`](https://github.com//sharkdp/bat/commit/f94ff93953b69aba6ca50de93272a53c7e2c458f))
    - Leave blank lines blank terminal output. ([`3523f60`](https://github.com//sharkdp/bat/commit/3523f602996aec1db01a3c2cc13fae265ae08389))
    - Solarized light and dark themes ([`83dc846`](https://github.com//sharkdp/bat/commit/83dc8468b0321958c119ad7c51655db2ddbe87ac))
    - Bump git2 from 0.10.2 to 0.11.0 ([`826624c`](https://github.com//sharkdp/bat/commit/826624c9fa3af2b9aacd2bbdaa03304b9376b832))
    - Bump assets/syntaxes/LESS from `4eada94` to `8cd08e3` ([`0fed098`](https://github.com//sharkdp/bat/commit/0fed098c53e527bca114e483f5c590e034528e8a))
    - Bump assets/syntaxes/TypeScript from `8bfce29` to `1d5160e` ([`cc7eafb`](https://github.com//sharkdp/bat/commit/cc7eafb7fb7f7bcd1adb7cbe6d33785eb13bdbf3))
    - Bump assets/syntaxes/Fish from `0f7d31b` to `ad6a6ca` ([`f35c41b`](https://github.com//sharkdp/bat/commit/f35c41ba1332a43ae51cfbc9c0a9960f5d2ed913))
    - Bump assets/syntaxes/ssh-config from `17e23d7` to `fcd4378` ([`1f4246e`](https://github.com//sharkdp/bat/commit/1f4246eb3cadb39494188d42ebbcdea387e74503))
    - Bump assets/themes/zenburn from `b7f27e2` to `cb746f6` ([`9300392`](https://github.com//sharkdp/bat/commit/9300392c43e3f2cd1e7f3190228100577cb66022))
    - Manually convert PowerShell.sublime-syntax ([`8d3ce5f`](https://github.com//sharkdp/bat/commit/8d3ce5f88a02fe7450bbeb0a4383b4632211e28c))
    - Bump assets/syntaxes/PowerShell from `12b7d72` to `4a0a076` ([`a1d18d8`](https://github.com//sharkdp/bat/commit/a1d18d8c8cd6c977c67e0da6d805ae282de05c6c))
    - Manually convert Crystal.sublime-syntax ([`d43431d`](https://github.com//sharkdp/bat/commit/d43431d8f184355d9477969b7600b1ec5e87b704))
    - Bump assets/syntaxes/Crystal from `2f96cec` to `2ee9d66` ([`8baa76a`](https://github.com//sharkdp/bat/commit/8baa76aa8f8c554290d96b8309dc31df95d33fd7))
    - Manually convert Nix.sublime-syntax ([`01846fa`](https://github.com//sharkdp/bat/commit/01846fad307024109348f98c1b4c83cb785a53f7))
    - Bump assets/syntaxes/Nix from `ebf0657` to `9032bd6` ([`18255f9`](https://github.com//sharkdp/bat/commit/18255f9835ff86acc2fb59a924f52e83be5dda29))
    - Manually convert Cabal.sublime-syntax ([`7b94f2c`](https://github.com//sharkdp/bat/commit/7b94f2c1d715b90c55a03601b7d130921252d7b5))
    - Bump assets/syntaxes/Cabal from `1c054d9` to `2d98cb7` ([`7ca73c2`](https://github.com//sharkdp/bat/commit/7ca73c2e79fff9dc3bc48e7b25178e198ba4b62f))
    - Manually convert Robot.sublime-syntax ([`75cb1ce`](https://github.com//sharkdp/bat/commit/75cb1ce196093483f98baa83f531fc7911e1dd98))
    - Bump assets/syntaxes/Robot from `8e5a349` to `fcd3323` ([`35026a0`](https://github.com//sharkdp/bat/commit/35026a033a4fa626bdcc1a2e071068fa44adcef6))
    - Update Japanese README file ([`48031f2`](https://github.com//sharkdp/bat/commit/48031f282c7e54c1d2105c59201c69047e0c756a))
    - added Nord color theme ([`7b2e98d`](https://github.com//sharkdp/bat/commit/7b2e98d5b70dc2938740c54321e64220a1292bfe))
    - Remove the already translated line ([`fb71b67`](https://github.com//sharkdp/bat/commit/fb71b67a8dd188e751c0d65d4184cab125f7795b))
    - Bump git2 from 0.10.1 to 0.10.2 ([`d48f51e`](https://github.com//sharkdp/bat/commit/d48f51e39d0e703e8967250195b8963c3ce57f32))
    - Bump assets/syntaxes/Packages from `d1494f4` to `71fd0ec` ([`9b1930b`](https://github.com//sharkdp/bat/commit/9b1930b2b303309374502dc98989322a8ddd15be))
    - Bump assets/syntaxes/Docker from `6e521ea` to `9e9a518` ([`96d8013`](https://github.com//sharkdp/bat/commit/96d8013581eb68699bc29cea1e0516182117224d))
    - Bump assets/syntaxes/Jsonnet from `80775c6` to `e2c917e` ([`46b345f`](https://github.com//sharkdp/bat/commit/46b345f2c24978ed7221b79e04ceb48f4681e596))
    - Bump assets/syntaxes/SCSS_Sass from `1195db9` to `0e8e0f9` ([`4e5b40e`](https://github.com//sharkdp/bat/commit/4e5b40e2746fd87631b817ea88197d561d7feb82))
    - Bump assets/syntaxes/Julia from `581805e` to `0ca4cc2` ([`d0e7ca1`](https://github.com//sharkdp/bat/commit/d0e7ca159a4bdfbbf96c51bd51e2e170b8ebe042))
    - Manually convert DotENV.sublime-syntax ([`e981e97`](https://github.com//sharkdp/bat/commit/e981e974076a926a38f124b7d8746de2ca5f0a28))
    - Bump assets/syntaxes/DotENV from `f6cad11` to `a1c9176` ([`a8ec3b8`](https://github.com//sharkdp/bat/commit/a8ec3b826c1010185a4156f0e4a957f8ead3dd24))
    - Manually convert TypeScript.sublime-syntax ([`f4a59d7`](https://github.com//sharkdp/bat/commit/f4a59d74f14255246dab1d3fb0ba8ebd1cc9d624))
    - Bump assets/syntaxes/TypeScript from `19a599a` to `8bfce29` ([`b9b6d9e`](https://github.com//sharkdp/bat/commit/b9b6d9e31e4b6543b8e94f53eea78da015f4d8c4))
    - Rename Fish syntax folder ([`d65ae51`](https://github.com//sharkdp/bat/commit/d65ae517dd868008ce72b37e30604e5480554571))
    - Remove manually converted fish syntax ([`bb675e5`](https://github.com//sharkdp/bat/commit/bb675e57c7baeac5c72d5778974db7af2d82eabc))
    - Bump assets/syntaxes/sublime-fish from `0795764` to `0f7d31b` ([`9fe38e2`](https://github.com//sharkdp/bat/commit/9fe38e200e48edb69465e5df351fc4d314f7a90c))
    - Bump assets/syntaxes/Protobuf from `5115c1c` to `726e21d` ([`a2fcb09`](https://github.com//sharkdp/bat/commit/a2fcb09d7a069eaf395ea5e89ddcebb29bbc8c42))
    - Bump assets/themes/sublime-monokai-extended from `1658f51` to `0ca4e75` ([`8c7916e`](https://github.com//sharkdp/bat/commit/8c7916e12875b74fd5e60b462b63b6a9c749cae2))
    - Bump assets/syntaxes/LESS from `d3ddfe7` to `4eada94` ([`3d23daa`](https://github.com//sharkdp/bat/commit/3d23daa7b463e1dee5bd7eead6767bb25b5a4b55))
    - Bump assets/syntaxes/PureScript from `42a61e3` to `5acebc1` ([`572cbb0`](https://github.com//sharkdp/bat/commit/572cbb00e2413855526612046439584a96334059))
    - Bump assets/syntaxes/Terraform from `3227a2d` to `c683495` ([`dedaebd`](https://github.com//sharkdp/bat/commit/dedaebdf15aea472b877a6654821d6aafc99a90f))
    - Bump assets/syntaxes/ssh-config from `86bfd1b` to `17e23d7` ([`3d90c6a`](https://github.com//sharkdp/bat/commit/3d90c6adee01f841c0c7718e20b35e94753055ba))
    - Bump assets/themes/onehalf from `2516b37` to `fdcbffe` ([`06a521e`](https://github.com//sharkdp/bat/commit/06a521ec3403b0bad2a4c8dc5c52e83125fdd8c4))
    - Properly add Twig submodule, closes #721 ([`37f7c94`](https://github.com//sharkdp/bat/commit/37f7c94ba4b33ba2fd6e70ffac124250dde66032))
    - Add simple pattern for command-line options ([`396e1ac`](https://github.com//sharkdp/bat/commit/396e1ac6b6eefa51515335595a0b1e5519e598a6))
    - Highlight capitalized function calls / man page references ([`49e2073`](https://github.com//sharkdp/bat/commit/49e2073910086be66d0319c594aca7c8e116943b))
    - Bump console from 0.9.0 to 0.9.1 ([`7d18649`](https://github.com//sharkdp/bat/commit/7d1864928419eb2ebb4f48c246d47d2d1b390971))
    - Remove 'advanced' example ([`850677f`](https://github.com//sharkdp/bat/commit/850677f121a28c758e97e040362e75380727f15b))
    - Make a few modules private ([`c132e19`](https://github.com//sharkdp/bat/commit/c132e19e7443febd5d3b1655d9b3da17c931d479))
    - Remove custom 'transpose' function ([`3334f74`](https://github.com//sharkdp/bat/commit/3334f74b729bde5696e161798c99781329215e23))
    - Simplify 'using_controller' example ([`b9ce3c2`](https://github.com//sharkdp/bat/commit/b9ce3c248c2fb18e427677ff122e1447120af3f3))
    - Format "delta" like the other programs. ([`310654d`](https://github.com//sharkdp/bat/commit/310654d49f1cff9fcb30084539aed2b01cf157c5))
    - Point to delta in usage-with-git section of README ([`3f37d29`](https://github.com//sharkdp/bat/commit/3f37d292ec69dfbcb7334c228a3fb69ed9f3a5dd))
    - Add Nim syntax, closes #542 ([`0a42d2b`](https://github.com//sharkdp/bat/commit/0a42d2b89788956345c7979803498a9571e8c36a))
    - Update options sort order in --help and manpage file ([`3b5fcf4`](https://github.com//sharkdp/bat/commit/3b5fcf461991777fd7bd9f4b89c26d30fc5eb7b3))
    - Add more colors to fstab syntax ([`ab2a6ad`](https://github.com//sharkdp/bat/commit/ab2a6ad1d56cd8abdbd53828a944ce6f61ed6eb1))
    - Add syntax highlighting for /etc/resolv.conf ([`9fb14d7`](https://github.com//sharkdp/bat/commit/9fb14d78e1a7e30513d62e2a2a4c4d3f27cb184d))
    - Update README.md ([`921a65f`](https://github.com//sharkdp/bat/commit/921a65f0a1257e0ed56d9d6890c1789b1d9d11f0))
    - Merge pull request #705 from MarcoFalke/patch-1 ([`7db30f9`](https://github.com//sharkdp/bat/commit/7db30f9123d51edce6e80db87a129678c05b3bda))
    - Merge pull request #696 from flopp/fstab-syntax ([`59f2b30`](https://github.com//sharkdp/bat/commit/59f2b30768ccaa3ddd5458c13cdef25950e8db8f))
    - Add Ubuntu/Debian bat package to Readme ([`b9c07e5`](https://github.com//sharkdp/bat/commit/b9c07e52e8aaf87ecf0473d3e8e2ad038dcedd76))
    - Add syntax highlighting for /etc/passwd file ([`edbb4b2`](https://github.com//sharkdp/bat/commit/edbb4b2fc9c631b295a8412855e035cc8f20a789))
    - Add syntax highlighting for /etc/group file ([`15b1676`](https://github.com//sharkdp/bat/commit/15b1676b7205999db3bd0057fd8504937452111c))
    - Add some comments ([`753a124`](https://github.com//sharkdp/bat/commit/753a124ef2310d1c13524e3861febb77b7788afd))
    - Add fstab file syntax ([`58a4d00`](https://github.com//sharkdp/bat/commit/58a4d009fa2c7ea024353d3ae4d44a052297d32a))
    - Remove comment in both examples ([`4664fb6`](https://github.com//sharkdp/bat/commit/4664fb6f91e1c713cd623daf8dfa34462f0dd7f9))
    - Fix compile-error on Rust v1.35.0 ([`e287594`](https://github.com//sharkdp/bat/commit/e287594654b6592f9f7425571b061b62b9a2a138))
    - Add 2 examples ([`1e19404`](https://github.com//sharkdp/bat/commit/1e194047360fa023a220edad3d6823be27291f0a))
    - Derive fmt::Debug whenever possible ([`2253d07`](https://github.com//sharkdp/bat/commit/2253d07341936c0671dee999f03779db72a1c712))
    - Implement trait Default for struct Config ([`7208a63`](https://github.com//sharkdp/bat/commit/7208a63a496fce38bbe2eae9f6d13fce5eac562a))
    - Run `cargo fmt` ([`0273848`](https://github.com//sharkdp/bat/commit/0273848f540f08154a41f294e7fda200063df458))
    - Move back printer.rs and others into lib ([`e542621`](https://github.com//sharkdp/bat/commit/e5426211250c907442717c566a13d38d52d4524d))
    - Move Config,PagingMode from app.rs into lib.rs ([`26439b4`](https://github.com//sharkdp/bat/commit/26439b41d2b5a7d68f8e6fe22ccd1299bd7c5ccc))
    - Move config.rs into src/bin/bat/ ([`a2ee753`](https://github.com//sharkdp/bat/commit/a2ee753b2502ac8d8e8a4ddaf38f96f2b2673c36))
    - Add --bins flag in CI `before_deploy` ([`d324390`](https://github.com//sharkdp/bat/commit/d3243903c0c3eb16a2c56f4646069c5ece2f2487))
    - Fix CI 'src/main.rs': No such file or directory ([`cf68b13`](https://github.com//sharkdp/bat/commit/cf68b13322f4c73671d7ea55324add58059af467))
    - Remove unused macros in bin/bat ([`837d099`](https://github.com//sharkdp/bat/commit/837d0998da6c81c81b56570dc157b9ad98f3b7da))
    - Fix all compile errors in bin/bat ➕ ([`23d80f9`](https://github.com//sharkdp/bat/commit/23d80f9e8473dacb45556dd24234dc0fa3341b31))
    - Move src/bin/* into src/bin/bat/ ([`e981bd8`](https://github.com//sharkdp/bat/commit/e981bd88c1eca9d986b8cd0db4ab28f83bb166ad))
    - Fix all compile warnings in lib.rs ([`710a1df`](https://github.com//sharkdp/bat/commit/710a1df4ff72f2647dfcb1301d6f2b0d9668215e))
    - Fix all compile errors in lib.rs 🚚 ([`cfd3316`](https://github.com//sharkdp/bat/commit/cfd33168af2a64f5ae9de22abf3994420ee222ea))
    - Add lib.rs ([`eefdb18`](https://github.com//sharkdp/bat/commit/eefdb186b8f05aedfa5ca76c89a4d14bebbfac32))
    - Move {main,app,clap_app}.rs into src/bin/ ([`fc0ad4d`](https://github.com//sharkdp/bat/commit/fc0ad4db2e3477ed1ad2d9de5b76c453a5e3a7c6))
    - Update TOML syntax ([`f03b45f`](https://github.com//sharkdp/bat/commit/f03b45f3e5e96dfd592d1279c89c7f600ae10f6c))
    - Disable nightly build ([`9a74bf1`](https://github.com//sharkdp/bat/commit/9a74bf1a88a91a6b571d1d0a338f4488796d4e2e))
    - Add llvm/libclang install instructions. ([`ff0b331`](https://github.com//sharkdp/bat/commit/ff0b331dd6528af484d2878f66b78b917435aea5))
    - Add section concerning file encodings, see #568 ([`00bfbbf`](https://github.com//sharkdp/bat/commit/00bfbbf8841346d97db12a1bfa90c02359393102))
    - Add Dracula theme ([`e44045a`](https://github.com//sharkdp/bat/commit/e44045a71aa3c451dff7ed9f40c64360e15180c3))
    - Update create.sh to quickly check for and update missing submodules ([`962abce`](https://github.com//sharkdp/bat/commit/962abcef80c6b7b5dd2ae960b984a41716514470))
    - Ensure submodules are updated before creating asset cache ([`95f2e5b`](https://github.com//sharkdp/bat/commit/95f2e5bbb726534bd511885177db3a4b291b9af6))
    - Add sponsor links as an experiment :-) ([`f46123e`](https://github.com//sharkdp/bat/commit/f46123e1493e1ea488072d47a914f8a0c696326f))
    - Add SaltStack SLS syntax ([`f0f77b1`](https://github.com//sharkdp/bat/commit/f0f77b1673b0b42a579c74309f6ced503de2e19f))
    - cmake and libz are not needed to install bat ([`39112e8`](https://github.com//sharkdp/bat/commit/39112e80c399f242c80bd8af857c64795a283aa6))
    - cmake and libz are not needed to install bat ([`a84c598`](https://github.com//sharkdp/bat/commit/a84c598e2c5e8bc6bf57f02eda67bab22b98662f))
    - Add MacPorts instructions ([`6b2f78e`](https://github.com//sharkdp/bat/commit/6b2f78ec5132f448245fd1b53dec4c3979bcbac2))
    - Merge pull request #676 from sharkdp/dependabot/cargo/liquid-0.19.0 ([`9380058`](https://github.com//sharkdp/bat/commit/938005864f93aa06fd0b17f6a72733ce9ea8d466))
    - Bump liquid from 0.18.2 to 0.19.0 ([`3a5f8ec`](https://github.com//sharkdp/bat/commit/3a5f8ece491e71e5bfc28517d5f6f934061d5cee))
    - Merge pull request #673 from eth-p/fix-659 ([`047a6f3`](https://github.com//sharkdp/bat/commit/047a6f3b851e792d200548817da2089a952b0b9d))
    - Revert changing generated file names based on parameters ([`f09b75d`](https://github.com//sharkdp/bat/commit/f09b75dafa1481a19e3d3a28c2a8238cd797e57b))
    - Fix CI before_deploy to use parameterized names ([`54ec2e7`](https://github.com//sharkdp/bat/commit/54ec2e7bd05da6390dd9ea4c6b4c716cbbb3ec40))
    - Add generated files to gitignore ([`e57b2e9`](https://github.com//sharkdp/bat/commit/e57b2e9528cd3ec0a467dd79a73ac8effb108b59))
    - Update fish completions to use parameterized names ([`ea2179b`](https://github.com//sharkdp/bat/commit/ea2179b35289c9217669e7fda0685f20d699dd50))
    - Update manual page to use parameterized names ([`c8478ce`](https://github.com//sharkdp/bat/commit/c8478cedc54db60f6149c6e2c72d8103582edfd4))
    - Merge pull request #669 from sharkdp/fix-666 ([`1692e5f`](https://github.com//sharkdp/bat/commit/1692e5fef7d72079396b103e175e9d2b5e07134f))
    - Update sublimehq/Packages to master ([`7ba5bc7`](https://github.com//sharkdp/bat/commit/7ba5bc78b742ba722246f49f20d9029964ddb032))
    - Bump syntect from 3.2.1 to 3.3.0 ([`25c2e17`](https://github.com//sharkdp/bat/commit/25c2e1725e2495925bff9e01007c9cbc981c1c8c))
    - Allow underscores and dashes in page names to fix highlighting issues with pages such as signal-safety or posix_openpt. ([`853e48e`](https://github.com//sharkdp/bat/commit/853e48e3f30989c2e15a0f1d111016cef98d9740))
    - Fix 'bat cache other-arg', closes #666 ([`f5d3599`](https://github.com//sharkdp/bat/commit/f5d359927ad6176d5bd8b9f2cf938f58c5f51575))
    - Replace 'Printer' by 'needle' ([`4df5973`](https://github.com//sharkdp/bat/commit/4df5973fde4a015f6cc94db54ea2e92e6c0650b2))
    - Extend 'man' integration section ([`45f3c73`](https://github.com//sharkdp/bat/commit/45f3c7333133bfb56c886bd5d3acd2d17332319d))
    - Add '-x' flag for 'col', see #652, see #667 ([`4102175`](https://github.com//sharkdp/bat/commit/4102175d981b495c377fd621eb46498b1b77a9f6))
    - Add eth-p/bat-extras integrations to README ([`30ee2a4`](https://github.com//sharkdp/bat/commit/30ee2a464f37f7c5c1ba39fed45681e0867239e6))
    - Fix missing and inconsistent code fences in REAMDE ([`7a5d1f2`](https://github.com//sharkdp/bat/commit/7a5d1f27e78fae5cbc5b7dfd0553cf90df6d8174))
    - Bump console from 0.8.0 to 0.9.0 ([`ac5ff9d`](https://github.com//sharkdp/bat/commit/ac5ff9d454b8a29942d1d3359d00664f5b19ae4e))
    - Use bash syntax highlighting ([`c9c3341`](https://github.com//sharkdp/bat/commit/c9c3341a38838b3b9815c522642d75a95355a4a8))
    - add dark mode script ([`a736889`](https://github.com//sharkdp/bat/commit/a736889ef16f2bc3ee61c20745e1b54db0a76cd4))
    - Bump git2 from 0.10.0 to 0.10.1 ([`ea5e096`](https://github.com//sharkdp/bat/commit/ea5e0967058078cf508172c60d5d01d1e1ac8dc1))
    - Add Jinja2 syntax ([`12330fd`](https://github.com//sharkdp/bat/commit/12330fd1548174596656b7ded4ff0e43a8f7486e))
    - Add list of maintainers to README ([`337ee56`](https://github.com//sharkdp/bat/commit/337ee568b18d44c6d95d3f588f09734103cf6c00))
</details>

<csr-unknown>
Fixed README.md to match the line number of text.Translated to Japanese.Fixed menu link anchor.<csr-unknown/>
# v0.12.1 (2019-09-03)

## Bugfixes

- Fixes a bug for older Windows versions (*"The procedure entry point `CreateFile2` could not be located"*), see #643 (@rivy)

## Commit Statistics

<csr-read-only-do-not-edit/>

 - 3 commits contributed to the release over the course of 1 calendar day.
 - 0 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' where seen in commit messages

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Bump version ([`608cefb`](https://github.com//sharkdp/bat/commit/608cefb05c2860f00397aa68f473fcd494ed21af))
    - Bump ansi_term from 0.12.0 to 0.12.1 ([`8d0aa39`](https://github.com//sharkdp/bat/commit/8d0aa391811f3aeeff3502d2362363550c1a7c91))
    - Add packaging status from repology.org ([`5815364`](https://github.com//sharkdp/bat/commit/581536420652389faa4940d9c55af67937971600))
</details>

# v0.12.0 (2019-08-31)

## Features

- Binary file content can now be viewed with `bat -A`, see #623, #640 (@pjsier and @sharkdp)
- `bat` can now be used as a man pager. Take a look at the README and #523 for more details.
- Add new style component to separate multiple `--line-range`s, see #570 (@eth-p)
- Added `-L` as an alias for `--list-languages`

## Bugfixes

- Output looks unbalanced when using '--style=grid,numbers' without 'header', see #571 (@eth-p)
- issues with filenames starting with "cache", see #584
- Can't build cache with new theme without creating cache dir, see #576 (@frm)
- `--terminal-width -10` is parsed incorrectly, see #611

## Other

- Added fish completions to DEB package, see #554

## New syntaxes

- Emacs Org mode, see #36 (@bricewge)
- `requirements.txt`
- DotENV `.env`
- SSH config syntax (`-l ssh_config`), see #582 (@issmirnov)
- `/etc/hosts`, see #583 (@issmirnov)
- GraphQL, see #625 (@dandavison)
- Verilog, see #616
- SCSS and Sass, see #637
- `strace` syntax, see #599

## Packaging

- `bat` is now in the official Gentoo repositories, see #588 (@toku-sa-n)
- `bat` is now in the official Alpine Linux repositories, see #586 (@5paceToast)
- `bat` is in the official Fedora repositories, see #610 (@ignatenkobrain)

## Commit Statistics

<csr-read-only-do-not-edit/>

 - 61 commits contributed to the release over the course of 94 calendar days.
 - 0 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' where seen in commit messages

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Update syntaxes ([`79f016e`](https://github.com//sharkdp/bat/commit/79f016e15cda054dffec009a4fc0900412bc1e48))
    - Re-formulate help text ([`26f9125`](https://github.com//sharkdp/bat/commit/26f9125ebb32005a87652da97f39aa7acd09551b))
    - Suppress warning in -A mode ([`9814eab`](https://github.com//sharkdp/bat/commit/9814eab13f8c5269e0ea6a689a0d8bda1c1c5db5))
    - Handle non-unicode characters in the preprocessor ([`82ca880`](https://github.com//sharkdp/bat/commit/82ca8804a2e6b427728734d3f8eb9f3ea37c6857))
    - Display binary file content for bat -A ([`b48b9fc`](https://github.com//sharkdp/bat/commit/b48b9fcf3b339e0fb37599e2019dd9bc4c49b379))
    - Update min. required rust version ([`c64ab29`](https://github.com//sharkdp/bat/commit/c64ab29739b1ae1b93c72805cba427e54de148a7))
    - Update man page ([`83dd3fb`](https://github.com//sharkdp/bat/commit/83dd3fb5512832ea640161198ba6a991b965fdc3))
    - Fix headline ([`ed345f2`](https://github.com//sharkdp/bat/commit/ed345f28897ff3b2ec2bcc206550dcaa9c897862))
    - Update syntaxes and themes ([`d249298`](https://github.com//sharkdp/bat/commit/d2492980c02ec9564a1a1491c94f06d6b4100972))
    - Bump version ([`47d5130`](https://github.com//sharkdp/bat/commit/47d51307645eaaf70cb2b8ffdab447fbb3d69fee))
    - Add first version of 'man page' syntax, closes #523 ([`4ba45e2`](https://github.com//sharkdp/bat/commit/4ba45e2ba24b84dceeed0da05b371c00638bd931))
    - Update dependencies ([`7076ae5`](https://github.com//sharkdp/bat/commit/7076ae5cd9f147b75fc0dfe590e7b1d44f29c3c5))
    - Fix another instance of the 'bat cache' bug, closes #584 ([`2e71146`](https://github.com//sharkdp/bat/commit/2e7114680173acd906e53ef5c3dd6e7b77014113))
    - Add explicit terminal width for test ([`f3824ba`](https://github.com//sharkdp/bat/commit/f3824bad17f21276c1d3e758a72a496cee506063))
    - Run 'cargo fmt' ([`ec908d5`](https://github.com//sharkdp/bat/commit/ec908d59374376318b27d64ba4b308f64a988976))
    - Add test for 'snip' feature ([`3a75d85`](https://github.com//sharkdp/bat/commit/3a75d85b80a9caacb2e0045b418ebc8b506db301))
    - Add 'dyn' keyword for trait references ([`63d652d`](https://github.com//sharkdp/bat/commit/63d652dab5e82fb56ea3a347ade98ed93251a1a4))
    - Add new style component to separate multiple '--line-range's ([`7f2e61d`](https://github.com//sharkdp/bat/commit/7f2e61d57961e8fa23a91ec1bd6348cbfc33a2c8))
    - Add short '-L' option as an alias for --list-languages ([`e289a2c`](https://github.com//sharkdp/bat/commit/e289a2c69833151cbcb75c11e16a4d2a7c907957))
    - Add 'strace' syntax, closes #599 ([`83a213b`](https://github.com//sharkdp/bat/commit/83a213bae7de12d2674153d7aa27eec417eee32e))
    - Add SCSS and Sass syntax, closes #637 ([`3967e28`](https://github.com//sharkdp/bat/commit/3967e28dedea1dc26cddbe55aadd013541117a31))
    - Add Verilog syntax, closes #616 ([`2dc2a37`](https://github.com//sharkdp/bat/commit/2dc2a3712a84122b2f535354210b2ed1e56e757c))
    - Install fish completions in DEB package, closes #554 ([`10e0f9e`](https://github.com//sharkdp/bat/commit/10e0f9efb0dfe6e68eda74c2c3ac06f378b48035))
    - Bump console from 0.7.7 to 0.8.0 ([`f8ea075`](https://github.com//sharkdp/bat/commit/f8ea075e78b15c716ed50c64e6a6254c6cbf4f0c))
    - Bump lazy_static from 1.3.0 to 1.4.0 ([`c187dce`](https://github.com//sharkdp/bat/commit/c187dce2c2bbf9469bcb72cfc350a6934f35e9d5))
    - Bump git2 from 0.9.2 to 0.10.0 ([`108c907`](https://github.com//sharkdp/bat/commit/108c907d69d27961fe1c0b9f90c96db04f488c71))
    - Use consistent syntax for command-line options ([`187a3b9`](https://github.com//sharkdp/bat/commit/187a3b9341f3965425e64c84b349769cf1eaffc6))
    - Add libc6-dev dependency for aarch64 ([`9c2d4f6`](https://github.com//sharkdp/bat/commit/9c2d4f6a511beffe111c3618cd1635184e718cf5))
    - Re-enable aarch64 builds ([`c112615`](https://github.com//sharkdp/bat/commit/c1126157d4aa34f266a5f2d28f9fed831d06092d))
    - Bump syntect from 3.2.0 to 3.2.1 ([`dcfa810`](https://github.com//sharkdp/bat/commit/dcfa810112ae11f9e00900bfd7c8b210c061bb22))
    - Add GraphQL sublime syntax submodule ([`f963104`](https://github.com//sharkdp/bat/commit/f963104006bb9139a276c31b66be0314cfe473aa))
    - Temporarily disable aarch64 ([`45e1c0e`](https://github.com//sharkdp/bat/commit/45e1c0ea6fe4d8b74976e97655de2048b79ce62b))
    - Bump minimum supported version to 1.33 ([`590a1e9`](https://github.com//sharkdp/bat/commit/590a1e9a4b1eb3be596eb41a48414578b3ab4866))
    - Update dependencies ([`8e2c6ab`](https://github.com//sharkdp/bat/commit/8e2c6abb136c4cb86034dd590457ea64adc05fba))
    - use explicit dyn with Write to appease compiler ([`28266ee`](https://github.com//sharkdp/bat/commit/28266ee4418e8d4b34e0080a75463215b5387f68))
    - Updated snapshot tests ([`21821f1`](https://github.com//sharkdp/bat/commit/21821f1d4c291e638311cc57fd2a296d561652fb))
    - Print a horizontal header line when 'grid' style without 'header' style ([`772ce2e`](https://github.com//sharkdp/bat/commit/772ce2e4fec27d51094044919f9ce6b4df9a5a32))
    - Bump atty from 0.2.12 to 0.2.13 ([`35ca1c5`](https://github.com//sharkdp/bat/commit/35ca1c51915f923890ce528217d7c382a750ca00))
    - Bump wild from 2.0.1 to 2.0.2 ([`0cc60a1`](https://github.com//sharkdp/bat/commit/0cc60a151951b33103fb8c72d2fa102e919304ac))
    - Bump ansi_term from 0.11.0 to 0.12.0 ([`ee6659d`](https://github.com//sharkdp/bat/commit/ee6659dacfce81cb495d25b60570708991913b77))
    - Allow leading minus in '--terminal-width <value>' ([`76c615a`](https://github.com//sharkdp/bat/commit/76c615a30416c4a0bea07fc7df4f5f1e88ddf6e0))
    - Small improvements for Fedora instructions ([`e8c0467`](https://github.com//sharkdp/bat/commit/e8c04672f086b25a179cd78733f7c939c3f27349))
    - Bump atty from 0.2.11 to 0.2.12 ([`7f5a561`](https://github.com//sharkdp/bat/commit/7f5a5612e44060ad3b931e81371ff6e2d00f9ce8))
    - [Security] Bump smallvec from 0.6.9 to 0.6.10 ([`eee5d42`](https://github.com//sharkdp/bat/commit/eee5d424bbc80bfd967f37fd9419ba8c2aaa8ec9))
    - Bump console from 0.7.6 to 0.7.7 ([`e5311e4`](https://github.com//sharkdp/bat/commit/e5311e4b3038dfbbac4307aa0bffcb607ccc8d5c))
    - Bump console from 0.7.5 to 0.7.6 ([`a1b9334`](https://github.com//sharkdp/bat/commit/a1b9334a44a2c652f52dddaa83dbacba57372468))
    - Add Alpine Linux installation instructions ([`d86b7cf`](https://github.com//sharkdp/bat/commit/d86b7cf7e071f5cabe52af9ad43c99dbe0575da4))
    - Bump git2 from 0.9.0 to 0.9.1 ([`67834da`](https://github.com//sharkdp/bat/commit/67834da681938011d89f6eed33bbc83f397feab1))
    - Add install instructions(ja) for Gentoo Users. ([`dcdf8df`](https://github.com//sharkdp/bat/commit/dcdf8dfaa7abede9efb908292dd56e63c772d31f))
    - Add install instructions for Gentoo Users. ([`9a44fa1`](https://github.com//sharkdp/bat/commit/9a44fa11c0335b71fc9af557faa4c3533552ca2c))
    - Merge branch 'issmirnov-ssh-config' ([`4ee0b2e`](https://github.com//sharkdp/bat/commit/4ee0b2e16dd17dcf451a37e60a5142eba33cb5b6))
    - Merge branch 'ssh-config' of https://github.com/issmirnov/bat into issmirnov-ssh-config ([`5189971`](https://github.com//sharkdp/bat/commit/518997135eb26ee261c8edf36c6b783a8b8cf4ba))
    - Add support for "etc/hosts" files. ([`69fc1ca`](https://github.com//sharkdp/bat/commit/69fc1caead487c478911c5d6eda36a09cd739129))
    - Add SSH config syntax ([`175dd4c`](https://github.com//sharkdp/bat/commit/175dd4c2904c80de7bd50dd4f7fd035a152ce5ce))
    - Bump git2 from 0.8.0 to 0.9.0 ([`e9210c0`](https://github.com//sharkdp/bat/commit/e9210c0f6c390ccffe72a5461c57b8348ceb4a48))
    - Add '.env' (DotENV) syntax ([`3795acb`](https://github.com//sharkdp/bat/commit/3795acbd170fd8246af792c180dc232a2f95f43f))
    - Add 'requirements.txt' syntax ([`c7d08cf`](https://github.com//sharkdp/bat/commit/c7d08cfcfdbc76edbef8d17a445d5fb87e8cce22))
    - Bump dirs from 2.0.0 to 2.0.1 ([`8ec96da`](https://github.com//sharkdp/bat/commit/8ec96da9f0e597d2e538b55edd62596c555d0378))
    - Enforce the creation of the full cache dir path ([`c9d7e36`](https://github.com//sharkdp/bat/commit/c9d7e3652bb0be4becdecfd8d2e7d4e795071c83))
    - Add Org mode syntax, closes sharkdp#36 ([`dfa024f`](https://github.com//sharkdp/bat/commit/dfa024f9e62aa10ebb91a1504f22b0a74eb72de2))
    - Bump dirs from 1.0.5 to 2.0.0 ([`b2757cb`](https://github.com//sharkdp/bat/commit/b2757cb461a811bc8b634516f099c2387fb59b2e))
</details>

# v0.11.0 (2019-05-15)

## Features

- Three new special color themes are available: `ansi-light`, `ansi-dark` and `base16`. These
  are useful for people that often switch from dark to light themes in their terminal emulator
  or for people that want the colors to match their terminal theme colors. For more details,
  see #543 and #490 (@mk12, implementation idea by @trishume)
- Hand-written auto completion script for Fish shell, see #524 and #555 (@ev-dev and @eth-p)
- The `-p`/`--plain` option can now be used twice (typically `-pp`). The first `-p` switches the
  `--style` to "plain". The second `-p` disables the pager. See #560 and #552 (@eth-p)

## Bugfixes

- Do not replace arguments to `less` when using `--pager`, see #509
- Binary files will now be indicated by a warning in interactive mode, see #530 #466 #550 (@maxfilov)
- Empty files are (once again) printed with a single header line, see #504 and #566 (@reidwagner
  and @sharkdp)
- `--terminal-width=0` is now disallowed, see #559 (@eth-p)
- Accidental printing of files named `cache`, see #557

## Other

- New integration tests, see #500 and #502 (@reidwagner and @sharkdp)
- New ["Integration with other tools"](https://github.com/sharkdp/bat#integration-with-other-tools) section in the README.
- Migrated to Rust 2018 (@expobrain)

## New syntaxes

- F# syntax has been updated, see #531 (@stroborobo)
- Fish shell, see #548 (@sanga)

## Packaging

- `bat` is now available on Chocolatey, see #541 (@rasmuskriest)

## Commit Statistics

<csr-read-only-do-not-edit/>

 - 66 commits contributed to the release over the course of 96 calendar days.
 - 2 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' where seen in commit messages

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Fix autocompletion-path ([`416a0fd`](https://github.com//sharkdp/bat/commit/416a0fd439bea69b9fcc3519e22e88f2349588af))
    - Only print the header for empty files ([`9b1b3dd`](https://github.com//sharkdp/bat/commit/9b1b3dda14e059180de55629fec452862a19cf37))
    - Update man page ([`ae7db31`](https://github.com//sharkdp/bat/commit/ae7db313a6683abf81e09e7b100b60a1d6df1c0f))
    - Update syntaxes and themes ([`3c43849`](https://github.com//sharkdp/bat/commit/3c4384990d9146d3c6007f7e2b6f366847a4fdf0))
    - Bump version to 0.11 ([`260814c`](https://github.com//sharkdp/bat/commit/260814c377d1c5696afc40b4a8dbcff99d125ec0))
    - Update dependencies ([`3675a82`](https://github.com//sharkdp/bat/commit/3675a82a1bb98162cabee7a6e51188cd9463b2fe))
    - Revert "Run 'cargo fmt'" (outdated rustfmt) ([`5b2b594`](https://github.com//sharkdp/bat/commit/5b2b5949199cb2e680e14fca21fe59c1289b9cb4))
    - Fix warning ([`9d53ce6`](https://github.com//sharkdp/bat/commit/9d53ce6be6bcb72e31eb09cf547c2f5e80ce3c9c))
    - Update dependencies ([`51e0549`](https://github.com//sharkdp/bat/commit/51e05499a986af75200689c1072b4471cff532ba))
    - Run 'cargo fmt' ([`dd40f75`](https://github.com//sharkdp/bat/commit/dd40f7545d0143980e6b00f4c27ae739c1f02b3d))
    - Print a warning message instead of just '<BINARY>' ([`c709bf2`](https://github.com//sharkdp/bat/commit/c709bf21031859d4f6a98d574e73cd31b6cc3bff))
    - Added printing of "<BINARY>" for binary files in the interactive mode if header decorations are turned off ([`f37dbb8`](https://github.com//sharkdp/bat/commit/f37dbb80a864716a3d55d703ff66203bbfc4cc49))
    - Extend help text for --terminal-width and --wrap, closes #535 ([`0a1e08d`](https://github.com//sharkdp/bat/commit/0a1e08da9679cdccadf111273c05defdb136875c))
    - Update fish completions for new --list-languages format ([`06b8dcb`](https://github.com//sharkdp/bat/commit/06b8dcb7eb2b47b86525506d9e36c33c20bc7fd7))
    - Added non-interactive mode for `--list-languages` ([`493a4e7`](https://github.com//sharkdp/bat/commit/493a4e719e4014706adcc75f17ee76e2cf27fb96))
    - Remove language completions for config files ([`27e0ca9`](https://github.com//sharkdp/bat/commit/27e0ca98d9b4bf68fda79ea753c7bfe70e330c1f))
    - Add fish shell argument completions for --language option ([`1b2066c`](https://github.com//sharkdp/bat/commit/1b2066c5f286c5a4638e3d46add69732ab3d1001))
    - Bump error-chain from 0.12.0 to 0.12.1 ([`4ec3cdb`](https://github.com//sharkdp/bat/commit/4ec3cdb5b85d616c908b07b03b9de1a776a12c69))
    - Fix accidental printing of files named 'cache' ([`7215229`](https://github.com//sharkdp/bat/commit/72152296c79dc1e758bcd8c3111c0c3e475c4bbe))
    - Replace iterator with string starts_with and ends_with ([`2b9d25d`](https://github.com//sharkdp/bat/commit/2b9d25df052e50c60531b1000703b79bf9b6065a))
    - Reformatted with `cargo fmt`. ([`cc81861`](https://github.com//sharkdp/bat/commit/cc8186103b629d3ea1eb4d6247c52b80ca60eea4))
    - Improved logic when encountering ANSI in character wrap mode ([`0ad09df`](https://github.com//sharkdp/bat/commit/0ad09df7abfb253dd9d6f55615db62e84b57337b))
    - Cleaned up terminal-width validation even more ([`7ef0fe0`](https://github.com//sharkdp/bat/commit/7ef0fe091dcd266f8af262d896638fcd7231b0a9))
    - Cleaned up terminal-width validation slightly ([`89fcbb2`](https://github.com//sharkdp/bat/commit/89fcbb2b1570e18da479f30801d6df3bb6d7c1c9))
    - Add validation for --terminal-width option ([`a7e2bb8`](https://github.com//sharkdp/bat/commit/a7e2bb86cbb168a8caa13a7d85278f57cffd6aaa))
    - Slightly modified help text ([`1ce0bc8`](https://github.com//sharkdp/bat/commit/1ce0bc8e0d6364b3dbac07158ad6343f3d5769a3))
    - Add -pp for plain styling and no pager ([`fde00ee`](https://github.com//sharkdp/bat/commit/fde00eec98035b59d3c91b65c0746f81252e3184))
    - Add 3 new themes: ansi-light, ansi-dark, base16 ([`bb6594e`](https://github.com//sharkdp/bat/commit/bb6594e69188a457239654d5d7f7f08fbfef6629))
    - Update Chocolatey package name to bat ([`e7c5561`](https://github.com//sharkdp/bat/commit/e7c5561df736d42734c39712c52abc19ec9aa8d5))
    - Add Chocolatey package instructions ([`0aabbc0`](https://github.com//sharkdp/bat/commit/0aabbc03d72f2ddac4b96fdea21979be22af8a83))
    - Updated manual to include -H, --highlight-line <N> ([`8a5b59f`](https://github.com//sharkdp/bat/commit/8a5b59febce1e8957c347ebd504ddd7f79e5cd8a))
    - cache/misc: be more cautious with bash ([`7afdd44`](https://github.com//sharkdp/bat/commit/7afdd44c4370bf2d7172d09a32e883b90d653dbe))
    - add fish shell syntax definition ([`d72549a`](https://github.com//sharkdp/bat/commit/d72549a873a20f8fb89fdcaecc7a9a4b82883d61))
    - Updated packaging script to include manual fish shell completions ([`1ba8540`](https://github.com//sharkdp/bat/commit/1ba8540e5ff00df72ae1b5c18c6e4ded97c4fb12))
    - Removed completions file accidentaly left in root dir ([`a6cee4d`](https://github.com//sharkdp/bat/commit/a6cee4da67219778d96ef160709fe64f56fbd48d))
    - Updated fish completions to include parameter completion for available themes, added cache subcommand completions ([`cd68509`](https://github.com//sharkdp/bat/commit/cd6850947edd0090a77df6ddaaaf87312f4a74d3))
    - Updated fish completions to include parameter completion for flags: color, italic-text, decorations, paging, wrap ([`14bfede`](https://github.com//sharkdp/bat/commit/14bfedeb209f3f9c9357d139ffca99cf79e15f61))
    - added manual minimal fish shell completions for current flags. Does not include flag sub-options like language lists or subcommands ([`8fe6525`](https://github.com//sharkdp/bat/commit/8fe65252f71882e47038dfaf7c2d2a4c8bb0fd4c))
    - added manual minimal fish shell completions for current flags. Does not include flag sub-options like language lists or subcommands ([`5e93514`](https://github.com//sharkdp/bat/commit/5e9351445fb84669f41231d923efb79864d7a55e))
    - Bump escargot from 0.4.0 to 0.5.0 ([`2fdfb8d`](https://github.com//sharkdp/bat/commit/2fdfb8de40533b55351b9ce18b413f15eb862995))
    - Bump clap from 2.32.0 to 2.33.0 ([`f8c759e`](https://github.com//sharkdp/bat/commit/f8c759edffefdba0144355d551f57d60c7e5ade3))
    - Bump assert_cmd from 0.11.0 to 0.11.1 ([`e34431f`](https://github.com//sharkdp/bat/commit/e34431f92235b9975a6516dae560cbe45d22fd81))
    - Update F# syntax ([`c96927b`](https://github.com//sharkdp/bat/commit/c96927b550bec973091b7b0a2a0f359b6ea07b9c))
    - Add copyright file on Debian package ([`921cc0d`](https://github.com//sharkdp/bat/commit/921cc0d1cbebf4a52e60815c721c1fd68981f0bf))
    - Bump syntect from 3.1.0 to 3.2.0 ([`dbc49e5`](https://github.com//sharkdp/bat/commit/dbc49e534a72d4db553cfb087f990e65434bdf6c))
    - Delete .DS_Store ([`7755ed7`](https://github.com//sharkdp/bat/commit/7755ed79e086a103abfa1e8f6cfb2302b9bb638f))
    - Updated minimum version of Rust ([`50aabf3`](https://github.com//sharkdp/bat/commit/50aabf331dc29201560fecd1b478110964736db5))
    - Formatted code ([`f259ff2`](https://github.com//sharkdp/bat/commit/f259ff2920f5238f6939016965fe5f900d6a42fe))
    - Applied linter fixes ([`82f1412`](https://github.com//sharkdp/bat/commit/82f14121bd70432080820c245e67c0eb3113f2c5))
    - Migrated to Rust 2018 ([`a21ae61`](https://github.com//sharkdp/bat/commit/a21ae614e667fdea9dac64548df9985113c354dd))
    - Bump lazy_static from 1.2.0 to 1.3.0 ([`b94c94e`](https://github.com//sharkdp/bat/commit/b94c94e84556af3228ea39fdfbdc0b62f7c397cc))
    - Bump dirs from 1.0.4 to 1.0.5 ([`161d107`](https://github.com//sharkdp/bat/commit/161d107ec3197cb8503ece4f6d8a7aa1f780be39))
    - Bump syntect from 3.0.2 to 3.1.0 ([`ecf147f`](https://github.com//sharkdp/bat/commit/ecf147fd9666b7ead585748893a6792691e77a4e))
    - Do not replace arguments to pager when --pager is used, closes #509 ([`f0771d6`](https://github.com//sharkdp/bat/commit/f0771d65329d1c01b22e5545f8201bd51d603b1f))
    - Make fd link actually link to fd ([`d8f28c1`](https://github.com//sharkdp/bat/commit/d8f28c177ad4ed18bc828a493ff00d31d16cee56))
    - Add "git show" section ([`fbee9df`](https://github.com//sharkdp/bat/commit/fbee9df71959587656d3deb4d9dce512a49ee7ef))
    - Add "xclip" section ([`6e9ca7d`](https://github.com//sharkdp/bat/commit/6e9ca7d6dc2e5a12242ed8bef999666783982fb2))
    - Add documentation about "tail -f" ([`a9ec5c0`](https://github.com//sharkdp/bat/commit/a9ec5c063a9f296f50f94ed4c5a815306eee6b01))
    - Add "Integration with other tools" section ([`8470fd7`](https://github.com//sharkdp/bat/commit/8470fd7173c6be28c85d1202baac254093522bfe))
    - Only print contents if file wasn't empty, or EOF wasn't first thing received from stdin. ([`76b7418`](https://github.com//sharkdp/bat/commit/76b7418e3554aec45a364013ef6eb3d1052d11cf))
    - Revert "Check result of read_until, and return Error if 0, which indicates EOF was found before delimeter." ([`10c5f79`](https://github.com//sharkdp/bat/commit/10c5f796407b082ad4ddab1560b03a36ef72b21d))
    - Handle UTF-16 encoding errors with replacement characters ([`9e11d66`](https://github.com//sharkdp/bat/commit/9e11d669791bd214cb61a76ce7a337033566ea6c))
    - Add test for UTF-16LE encoding ([`fa3244f`](https://github.com//sharkdp/bat/commit/fa3244f7c19431a386d8da02437e48cdb3b2f196))
    - Integration tests for single-line files ([`906774e`](https://github.com//sharkdp/bat/commit/906774e6d3e55831a29f611757f609fba0717628))
    - Add integration tests running bat with empty input files. ([`cec9cc0`](https://github.com//sharkdp/bat/commit/cec9cc073c477c0a664415b846fd7f2518c69ee3))
    - Update alternatives.md ([`a16789a`](https://github.com//sharkdp/bat/commit/a16789a060e4446f6b2345d487c02c6219070295))
</details>

# v0.10.0 (2019-02-07)

## Features

- Added new `--highlight-line <N>` option, see #453, #346 and #175 (@tskinn and @sharkdp)

## Changes

- **Change the default configuration directory on macOS** to `~/.config/bat`, see #442 (@lavifb). If you are on macOS, you need to copy your configuration directory from the previous place (`~/Library/Preferences/bat`) to the new place (`~/.config/bat`).
- Completely disabled the generation of shell completion files, see #372
- Properly set arguments to `less` if `PAGER` environment variable contains something like `less -F` (which is missing the `-R` option), see #430 (@majecty)
- Report the name of missing files, see #444 (@ufuji1984)
- Don't start pager if file doesn't exist, see #387
- Rename `bat cache --init` to `bat cache --build`, see #498
- Move the `--config-dir` and `--cache-dir` options from `bat cache` to `bat` and hide them from the help text.

## Bugfixes

- Blank line at the end of output when using `--style=plain`, see #379
- EOF must be sent twice on stdin if no other input is sent, see #477 (@reidwagner)

## New syntaxes

- Twig (@ahmedelgabri)
- `.desktop` files (@jleclanche)
- AsciiDoc (@markusthoemmes)
- Assembly (x86_64 and ARM)
- Log files (@caos21)
- Protobuf and ProtobufText (@caos21)
- Terraform (@caos21)
- Jsonnet (@hfm)
- Varlink (@haraldh)

## Other

- Added Japanese version of README (@sh-tech and @object1037)
- Code improvements (@barskern)

## Commit Statistics

<csr-read-only-do-not-edit/>

 - 83 commits contributed to the release over the course of 88 calendar days.
 - 0 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 1 unique issue was worked on: [#474](https://github.com//sharkdp/bat/issues/474)

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **[#474](https://github.com//sharkdp/bat/issues/474)**
    - Protobuf-syntax and ProtobufText syntax ([`94c9b40`](https://github.com//sharkdp/bat/commit/94c9b4069465e6b3b6dcd21108d51430580d7828))
 * **Uncategorized**
    - Move config-dir and cache-dir from 'bat cache' to 'bat' ([`e09499b`](https://github.com//sharkdp/bat/commit/e09499b3df6369e1a1f6e42f95ad4009fa76c00e))
    - Rename 'bat cache --init' to 'bat cache --build' ([`cff01d8`](https://github.com//sharkdp/bat/commit/cff01d81fa21e7ae469b9b4b2ea21e4578870732))
    - Update syntaxes ([`31608c9`](https://github.com//sharkdp/bat/commit/31608c9a031fc1b2cec70595c6fa0026bf77ca4e))
    - Bump version to 0.10 ([`357ce84`](https://github.com//sharkdp/bat/commit/357ce84c5612bb5aec5000a835048cde1146fd67))
    - Update assets/syntaxes/Packages ([`3a5c526`](https://github.com//sharkdp/bat/commit/3a5c52641bc9cabd330408fba3384d358d53d533))
    - Replace less command line arguments only for PAGER ([`6e8fca5`](https://github.com//sharkdp/bat/commit/6e8fca5bb258a8ff8e59a94e2d65904c03f19cac))
    - Ignore flags from PAGER env var if the program is 'less' ([`63c7738`](https://github.com//sharkdp/bat/commit/63c77383ceb4cef1d67b49025380050df74cee0f))
    - Update documentation ([`4df22e6`](https://github.com//sharkdp/bat/commit/4df22e617fa884f996cc2839a8287a035a999a76))
    - Remove import ([`6473e8c`](https://github.com//sharkdp/bat/commit/6473e8c1be22a361e6ee3bce15339b5e1c73d677))
    - Simplify Optional handling ([`972a764`](https://github.com//sharkdp/bat/commit/972a764a7468285bcd9270328f80352e1e24990e))
    - Add --cache-dir option ([`54143d1`](https://github.com//sharkdp/bat/commit/54143d1403b79ac1e28ef5c693ecb3e320601892))
    - Replace 'is_absolute_path' with map and filter ([`6523bbf`](https://github.com//sharkdp/bat/commit/6523bbf62f5aaccefb0ba8db462cd5c7c7a3a0c5))
    - removed compile errors and warnings on non-macs ([`05e2c2c`](https://github.com//sharkdp/bat/commit/05e2c2c66b80806d42c90175e23c833766e3ced5))
    - config and cache now check XDG env vars first ([`6922722`](https://github.com//sharkdp/bat/commit/69227222b58550fce86e728be38b0e922c47c876))
    - Set cache_dir on MacOS to ~/.cache/bat/ ([`49af3e8`](https://github.com//sharkdp/bat/commit/49af3e854bc73384fde8dfab0946e222493e8108))
    - Updated bat config dir for MacOs ([`6a8e475`](https://github.com//sharkdp/bat/commit/6a8e4757b75e3626c22cebb5a0f2ce6cf5e2352d))
    - add varlink syntax ([`91deef8`](https://github.com//sharkdp/bat/commit/91deef8b13384f53f8550feb2a53bb8dcc3dc5cc))
    - Check result of read_until, and return Error if 0, which indicates EOF was found before delimeter. ([`61e888d`](https://github.com//sharkdp/bat/commit/61e888de7fd1c0a434b532b7e4559b0650f93cbc))
    - Add Jsonnet (*.jsonnet) syntax ([`f3f9f10`](https://github.com//sharkdp/bat/commit/f3f9f10f058dfa2d4315359b6e20aa8657f66f63))
    - add Fedora instructions ([`29ffa2a`](https://github.com//sharkdp/bat/commit/29ffa2a832c435e6e3085ad5995f8f6b5fcad91a))
    - update README-ja.md ([`42a873f`](https://github.com//sharkdp/bat/commit/42a873fdb88b8d1aa22a8f92226db607938fff13))
    - Bump assert_cmd from 0.10.2 to 0.11.0 ([`82f048a`](https://github.com//sharkdp/bat/commit/82f048aedbcc7fe2c78b1eef838354a23b4fb0f3))
    - Add Terraform syntax ([`ae08785`](https://github.com//sharkdp/bat/commit/ae08785ec856682ea7dbd0ddcb0bd3b15d1fa42f))
    - Completely disable generation of shell completion files ([`a500629`](https://github.com//sharkdp/bat/commit/a5006293590d9d194f79e71edc09269c101d9365))
    - Fix Fedora instructions in README ([`724a5fa`](https://github.com//sharkdp/bat/commit/724a5fac99feaa0abd11c99650c8bf28be8d1c46))
    - Consistent naming for submodules ([`c483e49`](https://github.com//sharkdp/bat/commit/c483e49b92736d2ef75c2ebb0b257f08461314e6))
    - Bump console from 0.7.3 to 0.7.5 ([`0c9dc3b`](https://github.com//sharkdp/bat/commit/0c9dc3b257cdb691b5c159909c2b069cbf322b91))
    - Add syslog-syntax ([`3b5174b`](https://github.com//sharkdp/bat/commit/3b5174bb1d31621c9eef595c6c0cda0e827399f8))
    - Update README.md ([`3324505`](https://github.com//sharkdp/bat/commit/3324505383c9de3a1bbbc666f592c46011571323))
    - Bump console from 0.7.2 to 0.7.3 ([`de7872f`](https://github.com//sharkdp/bat/commit/de7872f8a870e465484fc7929046b40d8fc9b9b3))
    - Bump escargot from 0.3.1 to 0.4.0 ([`abf0229`](https://github.com//sharkdp/bat/commit/abf02297411d6c0a013158699ec6ce92dc1bcdae))
    - Bump console from 0.7.1 to 0.7.2 ([`e04bccd`](https://github.com//sharkdp/bat/commit/e04bccd12d8f7b32d6323a2638749fdde4a09947))
    - Merge pull request #453 from sharkdp/line-highlight ([`668c8a6`](https://github.com//sharkdp/bat/commit/668c8a67038d0b466c3f6c5c644c02fc71a011f6))
    - Fix the number of values to one ([`bd68684`](https://github.com//sharkdp/bat/commit/bd6868453705ba27b012b6909dac0d6d7fb204a3))
    - Short options for line-range and highlight-line ([`c2847f6`](https://github.com//sharkdp/bat/commit/c2847f6a9f886473bf7a2b5bb4742fcee4e8cc7a))
    - Colorize the whole line ([`cf7ed04`](https://github.com//sharkdp/bat/commit/cf7ed042c170ba41b5ad582271fdf24d2d5c1279))
    - Allow for multiple highlighted lines ([`6b92814`](https://github.com//sharkdp/bat/commit/6b92814ea0009d173060546dc6b459ef1aac238f))
    - Merge branch 'master' of https://github.com/tskinn/bat into tskinn-master ([`a236a9b`](https://github.com//sharkdp/bat/commit/a236a9b1955e78cd4d7cb61e7fa72e249ec7915c))
    - Bump minimum required Rust version to 1.30 ([`b68c5d8`](https://github.com//sharkdp/bat/commit/b68c5d857688201db44a188cbd89c93f9841ea7f))
    - Print ANSI-reset code before newline ([`fee57d7`](https://github.com//sharkdp/bat/commit/fee57d71d97591b79b461afa03941dee2b09216d))
    - Add x86_64 Assembly manually ([`5d3205a`](https://github.com//sharkdp/bat/commit/5d3205acc0feff8ac53fa9dc092b9e440dc62505))
    - Remove problematic 'Assembly (x86_64)' submodule ([`4e6816a`](https://github.com//sharkdp/bat/commit/4e6816abd6eaf05b1006a7c803f7fb2dec644183))
    - Bump git2 from 0.7.5 to 0.8.0 ([`72e8036`](https://github.com//sharkdp/bat/commit/72e8036edc804ee548979ebf2041a4abecf43a4c))
    - Omit launching of pager if NO file exists ([`397def1`](https://github.com//sharkdp/bat/commit/397def13204cbd983cab9ea6edff41a452406f79))
    - Add ARM assembly ([`d2d2918`](https://github.com//sharkdp/bat/commit/d2d29180a716b0153c1cc4a113c4aad162374ebd))
    - Add x86_64 assembly ([`2f263c9`](https://github.com//sharkdp/bat/commit/2f263c9fbfdbadea20bdead19d3bc43e8d8cf741))
    - add quotes around filename ([`4ff3606`](https://github.com//sharkdp/bat/commit/4ff360669f248af31d7bcc8b9c995f90c4e56f93))
    - Report filename if File::open return Err ([`549e5d7`](https://github.com//sharkdp/bat/commit/549e5d7355ecfc2bbfa550934de3ea4397733c82))
    - Add AsciiDoc syntax. ([`dd48d29`](https://github.com//sharkdp/bat/commit/dd48d29c28aff942f6be26f035e004b0984bba5a))
    - Add vimpager to alternatives, closes #429 ([`98ee5f7`](https://github.com//sharkdp/bat/commit/98ee5f7400a42b84129010e7988a00065e415c3a))
    - Bump console from 0.7.0 to 0.7.1 ([`638c04c`](https://github.com//sharkdp/bat/commit/638c04c13ebb12dfcc41881d255f2ce8c65515fe))
    - Update README.md ([`b1c865a`](https://github.com//sharkdp/bat/commit/b1c865acaa891269c67b2e06ece4ac2933e94f52))
    - Removed confusing variable from examples in README.md ([`d3d795f`](https://github.com//sharkdp/bat/commit/d3d795ffb2adad97d507192f21225f1382559d99))
    - Add copyright notice in MIT license ([`0571c51`](https://github.com//sharkdp/bat/commit/0571c514876345cbc309cfbb0eb45241a71db45e))
    - Enhance SyntaxMapping with impl Trait ([`348c9f3`](https://github.com//sharkdp/bat/commit/348c9f3562524d3931cbaf884fae1fe7763a574c))
    - Fix doc comment typo ([`6b6a8f8`](https://github.com//sharkdp/bat/commit/6b6a8f8e160e3dc0ca887253429623b0360913cf))
    - Correct typo in readme ([`6aa0028`](https://github.com//sharkdp/bat/commit/6aa002850cae24360d406f0ee39cdeefb256745f))
    - Bump console from 0.6.2 to 0.7.0 ([`49c22c8`](https://github.com//sharkdp/bat/commit/49c22c8d39244d2157e7440d7a87ed796436cbc1))
    - Bump assert_cmd from 0.10.1 to 0.10.2 ([`673caf6`](https://github.com//sharkdp/bat/commit/673caf6b59f7df4b62b9808eb24a016484839d6b))
    - Update assets/README.md ([`e6ffab3`](https://github.com//sharkdp/bat/commit/e6ffab3cb4dce3d97ebda51eeec41562797c3eb4))
    - Highlight XDG desktop entry as .ini file ([`4e43f8f`](https://github.com//sharkdp/bat/commit/4e43f8fa740d2ebdc9dbff759c1ae81bec4208a0))
    - Consistent spelling of "color" ([`bac1b16`](https://github.com//sharkdp/bat/commit/bac1b1688555d908716969da1da382d247af6e5b))
    - Add twig support ([`778a492`](https://github.com//sharkdp/bat/commit/778a4929a84279fd5d90a5781a8105ecfb739cd4))
    - add close tag ([`5c22dd4`](https://github.com//sharkdp/bat/commit/5c22dd4248b3da55da06297f69d6511b51ce736e))
    - add link to original README ([`214cd0f`](https://github.com//sharkdp/bat/commit/214cd0f8ea64e3942de9d5f983b01dabaea69678))
    - add table of contents ([`9fd61fc`](https://github.com//sharkdp/bat/commit/9fd61fcb6def94def3fb67b46eb90b91b2486097))
    - modify url ([`edf1d3b`](https://github.com//sharkdp/bat/commit/edf1d3b25664ec08c37f5b1bfa560c6747b0a5c3))
    - translate Project goals, alternatives ([`eca47b2`](https://github.com//sharkdp/bat/commit/eca47b2a28635a81a1b300f6062c36ff36eb7da8))
    - translate Customization ([`62424e7`](https://github.com//sharkdp/bat/commit/62424e70d703c52c16ea75b5d8d9094c68bdb6bd))
    - translate Installation ([`4791f15`](https://github.com//sharkdp/bat/commit/4791f15222743ffa657961cffd3df4b30e9189da))
    - translate Key Features & How to Use ([`159f234`](https://github.com//sharkdp/bat/commit/159f234b78b5ad4e4f156a3a650410d4412bacc3))
    - translate table of contents ([`3ec9612`](https://github.com//sharkdp/bat/commit/3ec96127f2ed19fd6fb4464f6b898ceb5a8f236e))
    - copy README.md (NOT translate yet) ([`a9acb27`](https://github.com//sharkdp/bat/commit/a9acb2711b76ff5cf3d1a945c80b72a5730366d5))
    - Update .gitattributes ([`a3182e6`](https://github.com//sharkdp/bat/commit/a3182e68e48710dd4b999e3d1409fd7f5e68545b))
    - Add assets/.gitattributes ([`0691b18`](https://github.com//sharkdp/bat/commit/0691b182313db3f742a25a0f543214973f5238d0))
    - Update .gitattributes ([`c711cd1`](https://github.com//sharkdp/bat/commit/c711cd1b2e3833bb72d7ce2dd2db858b859c8eec))
    - Add .gitattributes ([`14e6b41`](https://github.com//sharkdp/bat/commit/14e6b41a5f9c0ca61860a303fc3d7ad0fc6ad951))
    - Use Rust inclusive range (stable in 1.26) ([`532af65`](https://github.com//sharkdp/bat/commit/532af6556d40b187a9313872e8c0fd051ece02c8))
    - Update README.md ([`26717b0`](https://github.com//sharkdp/bat/commit/26717b0cd2f5d411f9561755bcd5461249da519e))
    - Update README.md ([`1f177f0`](https://github.com//sharkdp/bat/commit/1f177f0a1cd9feba75730ca240e0a7fd2678fcf4))
    - Update README.md ([`54c81fa`](https://github.com//sharkdp/bat/commit/54c81fa543079798ca04de8adab2ceca4b30104b))
    - Updates for README ([`34dee9e`](https://github.com//sharkdp/bat/commit/34dee9e3aa5e53c6a09eee1cf532ddc6a9d6b171))
</details>

# v0.9.0 (2018-11-11)

## Features

- A new `-A`/`--show-all` option has been added to show and highlight non-printable characters (in analogy to GNU `cat`s option):

  ![](https://camo.githubusercontent.com/c3e769482ef3184f6be6adaa34bdc8d19c378254/68747470733a2f2f692e696d6775722e636f6d2f324b54486859542e706e67)

  see #395 and #381 for more details.

- Added `--pager` option (to configure the pager from the configuration file), see #362 (@majecty)

- Added `BAT_CONFIG_PATH` environment variable to set a non-default path for `bat`s configuration file, see #375 (@deg4uss3r)

- Allow for multiple occurrences of `--style` to allow for the configuration
  of styles from the config file, see #367 (@sindreij)

- Allow for multiple `--line-range` arguments, see #23

- The `--terminal-width` option can now also accept offsets, see #376

## Changes

- Use of italics is now *disabled by default* (see #389 for details). They can be
  re-enabled by adding `--italic-text=always` to your configuration file.

- The default tab-width has been set to 4.

- Added new "Sublime Snazzy" theme.

- Shell completions are currently *not* shipped anymore, see #372 for details.

## Bugfixes

- Avoid endless recursion when `PAGER="bat"`, see #383 (@rodorgas)

## Other

- `bat` is now available on openSUSE, see #405 (@dmarcoux)

- Added section about the new configuration file in the README (@deg4uss3r)

- Chinese translation of README (@chinanf-boy)

- Re-written tests for `--tabs` (@choznerol)

- Speed up integration tests, see #394

## Commit Statistics

<csr-read-only-do-not-edit/>

 - 65 commits contributed to the release over the course of 22 calendar days.
 - 3 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' where seen in commit messages

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Add new image ([`c073d4a`](https://github.com//sharkdp/bat/commit/c073d4a11866a8d51ec19963803bb7ff5441d0e0))
    - Bump version, update assets ([`e79c551`](https://github.com//sharkdp/bat/commit/e79c5516bfe672c7917eb7e21f76977f9372870c))
    - Update man page ([`4776c2a`](https://github.com//sharkdp/bat/commit/4776c2a575dee60a8d36616352458c597932919c))
    - Update help text ([`f98a96c`](https://github.com//sharkdp/bat/commit/f98a96c1edb4f281529956b3bc75b73835977736))
    - Update dependencies ([`8a3446d`](https://github.com//sharkdp/bat/commit/8a3446d219a0f2b930da5b9cbf2ed7dc204f9284))
    - Update README.md ([`5d5bf61`](https://github.com//sharkdp/bat/commit/5d5bf616caabb9f9e97fab10cd7dbab7112c0dd6))
    - Update link to new issue ([`b68c038`](https://github.com//sharkdp/bat/commit/b68c038db8078b3afc4086e72efb830b430fbc26))
    - Bump syntect from 3.0.1 to 3.0.2 ([`1117d2f`](https://github.com//sharkdp/bat/commit/1117d2f9cb179b4f171c55b113cff94fb87ee290))
    - Wording ([`d0ba5bc`](https://github.com//sharkdp/bat/commit/d0ba5bcb32a8e1c73f808b5e67f6d419157e7eea))
    - Resolving requested changes ([`f8d39d5`](https://github.com//sharkdp/bat/commit/f8d39d5624b6affc376ba2bc142841e8337aac65))
    - Initial commit of updated README for config file formatting ([`9998e2e`](https://github.com//sharkdp/bat/commit/9998e2eac68e62db7bab80a5449f81b58114ccb7))
    - issue #383: Handle cases like `PAGER=/usr/bin/bat` ([`7cac07d`](https://github.com//sharkdp/bat/commit/7cac07d84559dc6eb02b643f7f640d81a7a4c07e))
    - Fix code style ([`148caa1`](https://github.com//sharkdp/bat/commit/148caa1381fe986a76b1c572bf297c0f9c9e8bed))
    - Remove args when change from bat to less ([`ab27cac`](https://github.com//sharkdp/bat/commit/ab27cac0cc1fe0290ccbb1985cceef7c24815544))
    - Better placement of bat detection ([`01fcbf2`](https://github.com//sharkdp/bat/commit/01fcbf2183e2f34152681036e2aedd1dc1ade2ed))
    - Avoid endless bat executions with PAGER="bat" ([`8a0c30b`](https://github.com//sharkdp/bat/commit/8a0c30b385151698311b683c4c065f5042c5021f))
    - Add openSUSE installation instructions ([`bc21c4d`](https://github.com//sharkdp/bat/commit/bc21c4d0e0f588cbceac0d4e28278e8e3d11f500))
    - Bump content_inspector from 0.2.3 to 0.2.4 ([`0800562`](https://github.com//sharkdp/bat/commit/0800562ba236a42f996f264ffb664e0321aa7c02))
    - Update SHA1 in TOML submodule ([`1f9befb`](https://github.com//sharkdp/bat/commit/1f9befbcd11ca5e264cdf596e31bd5fac05490b6))
    - Update path to TOML syntax remote ([`a5c779c`](https://github.com//sharkdp/bat/commit/a5c779c518abcdb00928d4c7f4c7e13877059845))
    - Bump lazy_static from 1.1.0 to 1.2.0 ([`f37f100`](https://github.com//sharkdp/bat/commit/f37f10035247822a67310d3b60f48d35de6925cf))
    - Changed italics to use-italic-text in config Better placing of italics in help and also added long_help for it ([`18d1a3b`](https://github.com//sharkdp/bat/commit/18d1a3b97337b60e9e3c38694560271384545f2e))
    - Linting ([`dc10246`](https://github.com//sharkdp/bat/commit/dc10246bd229ba0aa4bcd732b2b6409496764163))
    - Linting ([`44bdae0`](https://github.com//sharkdp/bat/commit/44bdae0212b262ec3dc62466aff6871c362af37f))
    - Changed flag name ([`1ece38a`](https://github.com//sharkdp/bat/commit/1ece38a4c455067e84961738874fc0fc11b4f6b7))
    - italics enable|disable working ([`e90308e`](https://github.com//sharkdp/bat/commit/e90308e1f8272b0cb863e568facc4ab4108178ef))
    - Added args ([`d702d67`](https://github.com//sharkdp/bat/commit/d702d6740c23ce67a329d9a1e6a971c6023046fe))
    - Run 'cargo run' only once ([`c6a526f`](https://github.com//sharkdp/bat/commit/c6a526f99cc623a8f8a7419f23d1d02cd4334c2e))
    - Move '--paging' arg to config file. ([`278d841`](https://github.com//sharkdp/bat/commit/278d84140864c65f91ec7a3d19741bcc94987b45))
    - Remove all relevant environment variables during testing ([`5f576f1`](https://github.com//sharkdp/bat/commit/5f576f1ecf0f431370a77abe04a7946c192529da))
    - Simplify integration test setup ([`48c6ea6`](https://github.com//sharkdp/bat/commit/48c6ea6b252ae8720db67b49773a90dd6a9f4536))
    - Changed to unwrap methods, added integration tests ([`558134f`](https://github.com//sharkdp/bat/commit/558134f6c8a9c3601c48863a45f5c3bf7f125972))
    - Ran rust fmt ([`1dd57e6`](https://github.com//sharkdp/bat/commit/1dd57e6d7e0e7325c49bca8baac610c5312922a9))
    - Added in environment variable and the result is a file check in config.rs ([`20ba84d`](https://github.com//sharkdp/bat/commit/20ba84d1fac834c7402af1187204b7620a39b645))
    - Fix `--show-all` for UTF-16 encoding ([`e81f9b2`](https://github.com//sharkdp/bat/commit/e81f9b23e698d17dbe6afd6cb718b70987d5bf70))
    - Display line-feed as  instead of ([`50dc4a7`](https://github.com//sharkdp/bat/commit/50dc4a79b01296701eb4b39ccf430405ed27de44))
    - Use `↹` character if tab-width == 1. ([`8fa8f44`](https://github.com//sharkdp/bat/commit/8fa8f4470697f2308222bec542dca9dafc831d32))
    - Highlight non-printable characters ([`ecd862d`](https://github.com//sharkdp/bat/commit/ecd862d9ff729a5de5e7b2771be02155567b72cf))
    - Remove generation code for tabs_ tests ([`cbed338`](https://github.com//sharkdp/bat/commit/cbed338c3a640c849f932e162ec3685867028f48))
    - Change default tab width to 4 ([`4492d99`](https://github.com//sharkdp/bat/commit/4492d99556bddd7d84c7a466d323c86cbac313c9))
    - Remove tab-related text ([`d96ee22`](https://github.com//sharkdp/bat/commit/d96ee2261f8a1c6624ec77e059f58eb76d1b483e))
    - Allow offset values in `--terminal-width` ([`dda27b2`](https://github.com//sharkdp/bat/commit/dda27b253be828c9c52f040c815e3a5f45d80010))
    - Temporarily disable shipping of shell completions ([`78f26d0`](https://github.com//sharkdp/bat/commit/78f26d0caf2cb9fe58fcbe29bf80de01a2a4f43c))
    - Remove commented-out code ([`abcd09f`](https://github.com//sharkdp/bat/commit/abcd09f8698fb8df21220ec36548f46d7d632063))
    - Only use the last argument of {style,plain,number} ([`41a5ff0`](https://github.com//sharkdp/bat/commit/41a5ff039be920d9013d6bfc5e863bbbd805dae4))
    - Rustfmt ([`155bad3`](https://github.com//sharkdp/bat/commit/155bad3862a0d53a8bfb69c3353a064fddfdc46c))
    - When specifying style multiple times the last occurence wins ([`7c98a1c`](https://github.com//sharkdp/bat/commit/7c98a1c901e5eca9f211ee8ef4df9a600d4de75c))
    - Update help text ([`c1246fc`](https://github.com//sharkdp/bat/commit/c1246fcd5309b5e7740ae950ca6095590e7bb969))
    - Update docstring ([`9f401ea`](https://github.com//sharkdp/bat/commit/9f401ea122dbe5a6a5416f67072471130d16dcb6))
    - Fix format error ([`9c782fb`](https://github.com//sharkdp/bat/commit/9c782fb692705aefb66cfa48f6ecc9896df18a2f))
    - Add --pager option ([`154186a`](https://github.com//sharkdp/bat/commit/154186a58deefd4b837677a5f27000c1a3beae73))
    - Update README.md ([`b22a9f8`](https://github.com//sharkdp/bat/commit/b22a9f8fe3730a3bfd3e488decd8687076552c27))
    - Wrapper script is not needed anymore ([`5219c40`](https://github.com//sharkdp/bat/commit/5219c40ed89e1d564d990e20fd468958195828ef))
    - Add sublime-snazzy theme ([`a25ee0e`](https://github.com//sharkdp/bat/commit/a25ee0e6fdc4041e9a75f3181a6441e28ceffca7))
    - Allow for multiple line ranges ([`496e0bc`](https://github.com//sharkdp/bat/commit/496e0bc04673b88bc6d9634ca45374b3c450557b))
    - Modify TravisCI caching policy ([`7082fd0`](https://github.com//sharkdp/bat/commit/7082fd09f0e8a2504f3386587b77f538ee97fa72))
    - Move link to header line ([`857cc21`](https://github.com//sharkdp/bat/commit/857cc215b407d844ef5a8f3b97e5b23405d8c52b))
    - add Chinese readme link ([`481268f`](https://github.com//sharkdp/bat/commit/481268f018b1a9fe41c9e0eda524e95c1ca01ae5))
    - Remove leftovers from tab-tests ([`f7d9f49`](https://github.com//sharkdp/bat/commit/f7d9f49503edd0470fc6cda62293203f6dd691c1))
    - cargo fmt ([`88fcfd7`](https://github.com//sharkdp/bat/commit/88fcfd76aa98633d16a0b6470b51f073b5d8ef87))
    - Avoid empty line in snapshot ([`76e34b2`](https://github.com//sharkdp/bat/commit/76e34b29ecbc65aef29cc0bf73912607041f6031))
    - Test 'tabs' with 'numbers' ([`01984e1`](https://github.com//sharkdp/bat/commit/01984e1d02f45a7607a85ea43f7bb96e79587c19))
    - remove --wrap for now ([`5363905`](https://github.com//sharkdp/bat/commit/536390509f9844421196416249bdbe5afe101238))
    - rewrite 6 snapshot tests as integration test ([`58198d0`](https://github.com//sharkdp/bat/commit/58198d070070c4cf0f6c9dbac43481b8d7b445c7))
    - Move tabs_* snapshot tests to integration_tests ([`afc5aac`](https://github.com//sharkdp/bat/commit/afc5aacb28563bbe0019a922d536660a14279da6))
</details>

# v0.8.0 (2018-10-17)

## Features

- Support for a configuration file with the following simple format:

  ```bash
  --tabs=4
  --theme="Sublime Snazzy"

  # A line-comment
  --map-syntax .ignore:.gitignore
  --map-syntax PKGBUILD:bash
  --map-syntax Podfile:ruby

  # Flags and options can also be on a single line:
  --wrap=never --paging=never
  ```

  The configuration file path can be accessed via `bat --config-file`. On Linux,
  it is stored in `~/.config/bat/config`.

- Support for the `BAT_OPTS` environment variable with the same format as specified
  above (in a single line). This takes precedence over the configuration file.

  See also #310.

- Support for custom syntax mappings via the `-m`/`--max-syntax` option.

  This allows users to (re)map certain file extensions or file names to an existing syntax:

  ``` bash
  bat --map-syntax .config:json ...
  ```

  The option can be use multiple times. Note that you can easily make these mappings permanent by using bats new configuration file.

  See #169

- Support pager command-line arguments in `PAGER` and `BAT_PAGER`, see #352 (@Foxboron)

- Add support for wildcards in Windows CMD, see #309 (@zxey)

- First-line syntax detection for all input types, see #205

- Encoding support for UTF-16LE and UTF-16BE, see #285

- New syntaxes: Robot framework (@sanga)

## Changes

- Binary files are now detected and not displayed when the output goes to an interactive terminal, see #205

## Bugfixes

- JavaDoc comments break syntax highlighting in .java files, see #81

- Bat Panics on Haskell Source Code, see #314

## Other

- Better `-h` and `--help` texts.

- Updated documentation on how to configure `bat`s pager

- Updated documentation for light backgrounds, see #328 (@russtaylor)

- Generate shell completions during build, see #115 (@davideGiovannini)

- A lot of new tests have been written

- `bat` is now available via [Termux](https://termux.com/), see #341 (@fornwall)

- `bat` is now available via [nix](https://nixos.org/nix), see #344 (@mgttlinger)

- `bat` is now available via [Docker](https://hub.docker.com/r/danlynn/bat/), see #331 (@danlynn)

## Commit Statistics

<csr-read-only-do-not-edit/>

 - 71 commits contributed to the release over the course of 21 calendar days.
 - 2 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 2 unique issues were worked on: [#327](https://github.com//sharkdp/bat/issues/327), [#329](https://github.com//sharkdp/bat/issues/329)

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **[#327](https://github.com//sharkdp/bat/issues/327)**
    - Generate shell completions with clap during build ([`0d71968`](https://github.com//sharkdp/bat/commit/0d7196861523ca14a1df58f7fbf9a75c34723175))
 * **[#329](https://github.com//sharkdp/bat/issues/329)**
    - Add support for wildcards in Windows CMD ([`b39e28d`](https://github.com//sharkdp/bat/commit/b39e28d2c855be1cbb560dd2c6dd65e337b4e218))
 * **Uncategorized**
    - --map-syntax takes just one value ([`30b742e`](https://github.com//sharkdp/bat/commit/30b742e9845314bcac67f37664cfb3df61c1ba3f))
    - Update help text ([`671deee`](https://github.com//sharkdp/bat/commit/671deeef8c0821f6cfd2933f60d510a9f7ff6837))
    - Bump bat version to 0.8 ([`a892cae`](https://github.com//sharkdp/bat/commit/a892caeb3371803204c9e3b1a9b66896371b7883))
    - Update assets ([`a770cbf`](https://github.com//sharkdp/bat/commit/a770cbf25e1fba9e25a673c86e23075a2464046f))
    - Update man page ([`931c6e9`](https://github.com//sharkdp/bat/commit/931c6e941467d3e38eb390ac2f399bfe8cdf1676))
    - Add --config-file option ([`8dc7e2e`](https://github.com//sharkdp/bat/commit/8dc7e2efa349e372ee359ce38fe6c5555c2d294d))
    - Implement syntax mapping ([`10965a6`](https://github.com//sharkdp/bat/commit/10965a6122e3ca6ad847871ec41eb01f0f595945))
    - Small cleanup ([`e43d97d`](https://github.com//sharkdp/bat/commit/e43d97dc15985258b03ade549e81ffdedce8c650))
    - Add integration tests for pager handling ([`2c7087b`](https://github.com//sharkdp/bat/commit/2c7087b8de797d18b7a8d5f8b33fc32adb8ecca1))
    - Add proper handling of empty PAGER variables ([`bb1f5aa`](https://github.com//sharkdp/bat/commit/bb1f5aa841d3495ae99b8796035ad7300b8f9155))
    - Add error handling for parsing errors ([`2109a78`](https://github.com//sharkdp/bat/commit/2109a7830b8f8630ead4313656b86bfd6c05bc81))
    - Remove duplicate 'extern crate' ([`ec27c78`](https://github.com//sharkdp/bat/commit/ec27c78a8a619a2bdd8b2a7675f227631471d851))
    - Add arguments from PAGER/BAT_PAGER ([`67fe833`](https://github.com//sharkdp/bat/commit/67fe833bc87d88ebb395be4a2d1c1d9806812c13))
    - Update syntect to 3.0.1 ([`e956225`](https://github.com//sharkdp/bat/commit/e956225b4d4298528a59551c943201d1c1c94fb0))
    - Bump Rust version ([`314ec88`](https://github.com//sharkdp/bat/commit/314ec881eff2fc0e783a74c41d05cdd40caeffd6))
    - Revert "Fix for older version of Rust" ([`25a6a55`](https://github.com//sharkdp/bat/commit/25a6a55437bd089277579e3c2a644e54c7ffc5c2))
    - Add BAT_OPTS environment variable ([`5bff454`](https://github.com//sharkdp/bat/commit/5bff4548be2575d82e8fd1df5e55aea122c82412))
    - Add error handling ([`866b9e1`](https://github.com//sharkdp/bat/commit/866b9e16a866a51644d10760ea2fdd062f0c902f))
    - Move transpose to util module ([`5af176c`](https://github.com//sharkdp/bat/commit/5af176c94f3e797d2d784b0c3df103a2921fd62d))
    - Fix for older version of Rust ([`9e7da05`](https://github.com//sharkdp/bat/commit/9e7da0545936c2365742136f8caee73649f5ef6c))
    - Fix include ([`1ae02c6`](https://github.com//sharkdp/bat/commit/1ae02c65fb3d6a847d853fdb020446c77f3126e6))
    - Add --no-config option ([`495e7fd`](https://github.com//sharkdp/bat/commit/495e7fd3b1766c889f048a88c9702310f6d7c1a6))
    - Fix tests ([`b48f0fe`](https://github.com//sharkdp/bat/commit/b48f0fe38991e54b5369a336a7aa5cbe8e7d72a6))
    - Add more tests ([`c19c704`](https://github.com//sharkdp/bat/commit/c19c704a4329f7c05af7cd5586b184f8eb1e39a8))
    - Add possibility for spaces on a single line ([`693bd59`](https://github.com//sharkdp/bat/commit/693bd5929d856dd046ca5bd7316c5db4c9a1ee9c))
    - Add simple configuration file ([`8275b04`](https://github.com//sharkdp/bat/commit/8275b0436d9501614f29b831b164687b3bc281c0))
    - Continue with output after "is directory"-error ([`278bde5`](https://github.com//sharkdp/bat/commit/278bde5cee57db6866a925890fac176a1af6eb0a))
    - Removes duplicate explanation. ([`eee7e5a`](https://github.com//sharkdp/bat/commit/eee7e5a575f8ae7a932cab410be5fb66bb6b5e0d))
    - Fixes #328 - adds info for light backgrounds. ([`ad7b634`](https://github.com//sharkdp/bat/commit/ad7b634ee60ad0fc3a5c72f6291deaa543382553))
    - Update Docker alias for bat ([`1310f83`](https://github.com//sharkdp/bat/commit/1310f83c8d3f7d9ac63de63e769aad697abbce8a))
    - Add Docker instructions, closes #331 ([`897b9e7`](https://github.com//sharkdp/bat/commit/897b9e7030b91087b780490b0812831b7be0ebb4))
    - Add simple integration tests ([`1891e19`](https://github.com//sharkdp/bat/commit/1891e194b5ee958450678b6ac739711ba57fae10))
    - be consistent ([`cea05e9`](https://github.com//sharkdp/bat/commit/cea05e9f22b40edf2ccd2a0676827974796972a2))
    - Always remove the local cache when creating new assets ([`d2d01b9`](https://github.com//sharkdp/bat/commit/d2d01b9fe8d0e5f46534731933a26a5174b2320b))
    - Fix formatting ([`2a78515`](https://github.com//sharkdp/bat/commit/2a7851530dbf37ade9e5493c021d34fb4f3e700b))
    - add line highlight ([`1a6709c`](https://github.com//sharkdp/bat/commit/1a6709c2cb3d8403e2f709a7e8b66c031216e559))
    - Updates for syntect 3.0 ([`5842d58`](https://github.com//sharkdp/bat/commit/5842d58c017295e8d99c8712d44b7d62d1afd318))
    - Add robot framework support ([`e3c71ad`](https://github.com//sharkdp/bat/commit/e3c71adba72ef796b028c6b47b84ba75103392ac))
    - Documented nix availability ([`504f28b`](https://github.com//sharkdp/bat/commit/504f28b3a1213786dd5547ed8c125f5ec1570988))
    - Update dependencies ([`d977ba9`](https://github.com//sharkdp/bat/commit/d977ba91945c8ff15be1bafd575245ff6e4f9860))
    - add exec bit for create.sh ([`9c09799`](https://github.com//sharkdp/bat/commit/9c09799e1fc72ffdd5d64983e319a94a49331a29))
    - Update README.md ([`d83c93c`](https://github.com//sharkdp/bat/commit/d83c93c8059eb0d44074880552d9cc366944d427))
    - cargo fmt ([`fc8fd1e`](https://github.com//sharkdp/bat/commit/fc8fd1e63f64ab4aad21ae4832830604ca49177d))
    - Update content_inspector version ([`404d6a4`](https://github.com//sharkdp/bat/commit/404d6a4a81a49c6090c70a854f22658f18796ad5))
    - Add support for UTF-16LE and UTF-16BE ([`25d96da`](https://github.com//sharkdp/bat/commit/25d96da4a216a5fc0766c28d077e7de57fb735a5))
    - Use first-line detection for STDIN ([`e97095b`](https://github.com//sharkdp/bat/commit/e97095b724b763e6841c08d9b89b20089592d310))
    - Do not display binary files in interactive mode ([`ce96df0`](https://github.com//sharkdp/bat/commit/ce96df00b6e77841f7c77fdc50e5b6e5b944e375))
    - Simplify access to first line ([`f98fc5f`](https://github.com//sharkdp/bat/commit/f98fc5f06afd76a8eeb914ef7c2caf384834eade))
    - Update minimum Rust version to 1.27 ([`1dbb4ef`](https://github.com//sharkdp/bat/commit/1dbb4ef683e26b0cc84903153ff35c09a41a9deb))
    - Add first-line detection for all input types ([`0502a3b`](https://github.com//sharkdp/bat/commit/0502a3bd4a3b70958e920e1a49511e2968c00a2c))
    - Add possibility to get first line ([`869cf63`](https://github.com//sharkdp/bat/commit/869cf6368cd043118ffa3ce0cb694a4af638342a))
    - Add unit test for InputFileReader ([`078228d`](https://github.com//sharkdp/bat/commit/078228deac471521f2034728ec1f4f4fe99d337e))
    - Move snapshot tests into separate module ([`e09d7da`](https://github.com//sharkdp/bat/commit/e09d7dabb8f561017b568a166538d9ea5e65503e))
    - Move read_line functionality to inputfile module ([`6d1cc8c`](https://github.com//sharkdp/bat/commit/6d1cc8c2c8bc2bea6d381fd6ec4a29f66dde8699))
    - Use dyn Trait for trait objects ([`87f0210`](https://github.com//sharkdp/bat/commit/87f021078ec4ae266b6f69862644ac59e7cba8f7))
    - Move get_reader into inputfile module ([`d5b0502`](https://github.com//sharkdp/bat/commit/d5b0502419dd919b75df20fd4798dce66c532d63))
    - Move InputFile to separate module ([`860f3e9`](https://github.com//sharkdp/bat/commit/860f3e90068a63e9f08a161a453802c0d2094f47))
    - Add 'plain' version of bat to the benchmarks ([`1be346a`](https://github.com//sharkdp/bat/commit/1be346a03858bb5a396acf28be8b47d65d34584e))
    - Simplify -h help text ([`f9fd5e4`](https://github.com//sharkdp/bat/commit/f9fd5e485173adf71b44888f72980577f16ddcf7))
    - Remove unnecessary scope ([`da09f38`](https://github.com//sharkdp/bat/commit/da09f3877bb00296a75ababe5f1f0aae325ef6b5))
    - Move theme-preview section into own paragraph ([`e30e1ba`](https://github.com//sharkdp/bat/commit/e30e1bab2e4d38c00363f5578fbfbbcbc13447ca))
    - Fixes #273 - describes 'musl' builds. ([`f140f6d`](https://github.com//sharkdp/bat/commit/f140f6da4629b2d3ab9f2424552d96c8739eae6b))
    - Document how to rapidly preview theme on a file ([`3446fdf`](https://github.com//sharkdp/bat/commit/3446fdf5f26f018ebd729a901aaae85f3e4787ea))
    - Update VimL syntax ([`e1345cd`](https://github.com//sharkdp/bat/commit/e1345cdc2ac1c69ddf9edea65e44550c59696e0b))
    - Workaround for #81 ([`a2676cc`](https://github.com//sharkdp/bat/commit/a2676ccc55ab8ebc4b673d1cd888ddaf8874dc71))
    - run cargo fmt ([`80da0dc`](https://github.com//sharkdp/bat/commit/80da0dc6194d94dbbc87185283d4e299b8101dfd))
    - Typo ([`64903bb`](https://github.com//sharkdp/bat/commit/64903bb858c860c41ea0d9207669d69a00512c68))
    - Add a small tutorial ([`3456ce1`](https://github.com//sharkdp/bat/commit/3456ce11874cf06b8cb6164437f2198e6ac7a4aa))
    - Document the situation with less and scrolling ([`886a5b3`](https://github.com//sharkdp/bat/commit/886a5b3d240e0c94ff239d887fb55f7b0cbb770b))
</details>

# v0.7.1 (2018-09-23)

## Features

- Use the `ansi_colours` package by @mina86 for better true-color approximation on 8 bit color terminals, see #319 and #202.

## Bugfixes

- Bat Panics on Haskell Source Code, see #314
- Disable wrapping when `--style=plain`/`-p` is used, see #289

## Other

- Added Ansible install instructions (@aeimer)
- Added section about Cygwin to the README (@eth-p)

## Commit Statistics

<csr-read-only-do-not-edit/>

 - 9 commits contributed to the release over the course of 10 calendar days.
 - 0 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' where seen in commit messages

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Update syntax-set, fixes #314 ([`238482e`](https://github.com//sharkdp/bat/commit/238482e5f3990c1f5ef22a7875066822f10857f8))
    - Use ansi_colours package for better true-colour approximation ([`79b960e`](https://github.com//sharkdp/bat/commit/79b960e17e75339d2093a78bc7f8af89a05a2018))
    - Document INI syntax modifications ([`6aa626f`](https://github.com//sharkdp/bat/commit/6aa626f1c4c2819c37ad1103f038aa8ae7c7a047))
    - Added Mercurial's hgrc-files to the list of INI-extensions ([`1b30539`](https://github.com//sharkdp/bat/commit/1b305394930f31f6226702662590941b2f794471))
    - Fix typos ([`96b24d8`](https://github.com//sharkdp/bat/commit/96b24d8a7ec803da2a950e63170a93ccb2ec581b))
    - Disable wrapping when `--style` is plain ([`20b02e7`](https://github.com//sharkdp/bat/commit/20b02e72b32d419fc1d9627542ea62b00b9c23f4))
    - Added section to README on using Cygwin. ([`254efac`](https://github.com//sharkdp/bat/commit/254efac9c26ab6ca924ccb6a5a89f1ef3789b460))
    - Update README (libz-dev may be required) ([`dcec322`](https://github.com//sharkdp/bat/commit/dcec32211707946cc81abfc9d33dda53650899dc))
    - Adds ansible install instructions for ansible to readme ([`d781156`](https://github.com//sharkdp/bat/commit/d781156327e8eeb03a3815e76bf75e340eccdf19))
</details>

# v0.7.0 (2018-09-12)

## Features

- Tabs are now (optionally) expanded to spaces. This can be controlled with the new
  `--tabs` command-line option or the `BAT_TABS` environment variable. The
  new feature also closes two bugs #166 and #184. For more information, see #302 (@eth-p).

- Added support for the `BAT_STYLE` environment variable, see #208 (@ms2300)

- Added `OneHalf` theme for terminals with a light-gray background, see #256

- Added new syntaxes for CSV, JSX in JavaScript and TypeScript, Cabal, Dart,
  F#, PureScript, Swift, Crystal, PowerShell (Many Thanks to @tobenna and @mimadrid)

## Changes

- Query `git diff` only when needed, see #303 (@ShikChen)

- Disable wrapping when `--plain` is used, see #289 (@eth-p)

## Bugfixes

- Can read files named `cache`, see #275 (@BK1603)

- A lot of bugfixes for Windows, see #252, #264

- Detect `less` reliably and in a portable way, see #271 and #290 (@Aankhen)

- last decoration line is not formatted properly with `--wrap never`, see #299 (@Rogach)

- Do not show file header for directories, see #292

## Other

- Enabled a new `aarch64` build target, see #258 (@rachitchokshi)

- Provide Debian packages for `armhf`, see #280 (@rachitchokshi)

- Added README section about "`bat` on Windows" (@Aankhen)

- Windows installation via scoop (@meltinglava)

## Commit Statistics

<csr-read-only-do-not-edit/>

 - 69 commits contributed to the release over the course of 11 calendar days.
 - 0 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 4 unique issues were worked on: [#1](https://github.com//sharkdp/bat/issues/1), [#275](https://github.com//sharkdp/bat/issues/275), [#289](https://github.com//sharkdp/bat/issues/289), [#301](https://github.com//sharkdp/bat/issues/301)

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **[#1](https://github.com//sharkdp/bat/issues/1)**
    - adding deb packaging for arm64 ([`0fe3bad`](https://github.com//sharkdp/bat/commit/0fe3badf19a2e05c2b5cdde8bf748c393aab91c7))
 * **[#275](https://github.com//sharkdp/bat/issues/275)**
    - Can read files named "cache" now, cache subcommand working too ([`53d0c1d`](https://github.com//sharkdp/bat/commit/53d0c1deca0c22a16cad83c99c7296208650aff1))
 * **[#289](https://github.com//sharkdp/bat/issues/289)**
    - Wrapping disabled when --plain is used. ([`fb61aa4`](https://github.com//sharkdp/bat/commit/fb61aa4f60096eebd59b48367465cd88279d3180))
 * **[#301](https://github.com//sharkdp/bat/issues/301)**
    - provide armhf deb packages ([`f0d9367`](https://github.com//sharkdp/bat/commit/f0d936763c552c3c5ca913333c0aef3a9d43900d))
 * **Uncategorized**
    - Bump version ([`732750c`](https://github.com//sharkdp/bat/commit/732750c2749ebeb2b0bea89028a171c792a2aadf))
    - Update dependencies ([`6b6baa3`](https://github.com//sharkdp/bat/commit/6b6baa31189dcbff96765c4b013e1ca09b18d0cd))
    - Bump version, update assets ([`54c4349`](https://github.com//sharkdp/bat/commit/54c434934ee961a0932dfe549ffedbca419e30de))
    - Update man page ([`ad6e0a2`](https://github.com//sharkdp/bat/commit/ad6e0a2581341c8b98061aadea478ed3c20c3a11))
    - Mention BAT_STYLE in README.md ([`bd619db`](https://github.com//sharkdp/bat/commit/bd619db3caaccdf0009be11ada31930b82c04343))
    - Do not show file header for directories ([`e098eb4`](https://github.com//sharkdp/bat/commit/e098eb43a28f55942f3185033ebe3e09d86198d7))
    - Merge pull request #298 from ms2300/bat_style ([`ea369ee`](https://github.com//sharkdp/bat/commit/ea369ee17f287309f746503233ebc6fcfbdeba2e))
    - Merge remote-tracking branch 'upstream/master' into bat_style ([`63d32bc`](https://github.com//sharkdp/bat/commit/63d32bc818b8a8375ddf70d069e7ee903b63d55c))
    - Use a more streamlined version of style-component collection ([`dccf8d8`](https://github.com//sharkdp/bat/commit/dccf8d82212e24e9015e4346a3d7f9330051a6ff))
    - Add PowerShell syntax, closes #306 ([`97129ab`](https://github.com//sharkdp/bat/commit/97129ab9d8b249611b31740ad597452079458afc))
    - Merge pull request #302 from eth-p/feature-tabs ([`52d0d6c`](https://github.com//sharkdp/bat/commit/52d0d6ca30085192bc0dd523a0ee4c583fa3f457))
    - Make tests more robust ([`a05494e`](https://github.com//sharkdp/bat/commit/a05494e9d9960afa5d6cab14483dc1a64658687f))
    - Fix test for --style=plain ([`84ac92e`](https://github.com//sharkdp/bat/commit/84ac92efbbcf5ff04778f99cf2f6badf2405a840))
    - Make generate_snapshots script more robust ([`b4c6e41`](https://github.com//sharkdp/bat/commit/b4c6e412dc01e6e2e794436ede4b953b7e26d981))
    - Re-generate files again ([`3e21d69`](https://github.com//sharkdp/bat/commit/3e21d69a92a1a3bb388d75c22f3f62c41cd558cb))
    - Undo change in sample.modified.rs ([`b7690d4`](https://github.com//sharkdp/bat/commit/b7690d4bb187ab04cd581cc4097f771498220531))
    - Added validation for --tabs. ([`d404139`](https://github.com//sharkdp/bat/commit/d404139ff7484d6dbf8dbe57ec21b3b4a4176801))
    - Query git diff only when needed ([`4e3ab4d`](https://github.com//sharkdp/bat/commit/4e3ab4d39915979592c23318bbdb9b5c43b7b9ea))
    - Update snapshots. ([`9ef1e9f`](https://github.com//sharkdp/bat/commit/9ef1e9f7924524d4ac62a712a306afa5d336ef87))
    - Fix sample.modified.rs ([`503fe0b`](https://github.com//sharkdp/bat/commit/503fe0b641724be1e8f1a4827e54264f743214f5))
    - Merge remote-tracking branch 'upstream/master' into feature-tabs ([`1807f96`](https://github.com//sharkdp/bat/commit/1807f9653cb27a16f9b7d7e445217562ef8d612a))
    - Ran `cargo fmt`. ([`1e74f0e`](https://github.com//sharkdp/bat/commit/1e74f0e2a9aa74d07da0037a084a64e62ce0cac6))
    - Added three new snapshot tests for --tabs and --wrap. ([`c1e1f75`](https://github.com//sharkdp/bat/commit/c1e1f753cfdae56dc3777e6e512bdb0b56ef3750))
    - Fixed tab expansion not working in --wrap=never mode. ([`d90797f`](https://github.com//sharkdp/bat/commit/d90797f8e947f01da74fd2aa065f48df3e50ab0b))
    - Moved tab expansion to happen after syntax highlighting. ([`b4096e5`](https://github.com//sharkdp/bat/commit/b4096e562726292fa61bf9548b6ab055df31c898))
    - print additional newline if last line in input file was not terminated with a newline (fixes #299) ([`e1ecc17`](https://github.com//sharkdp/bat/commit/e1ecc17f69f1d84fb127203a45e4abd88babab07))
    - BAT_STYLE accounts for multiple styles (ie numbers,header) ([`7897260`](https://github.com//sharkdp/bat/commit/7897260bf00fc017da383a783eede2583206acc5))
    - Updated before_deploy.bash to call version of strip specific to aarch64 target ([`89d4cb9`](https://github.com//sharkdp/bat/commit/89d4cb951a2232a60891b14a79047a37d66c585e))
    - Testing build by removing dependency 'libz-sys = "1.0.20"' ([`e9681bf`](https://github.com//sharkdp/bat/commit/e9681bf22dd3aedd1ae479df0293cb97e32b49c4))
    - Enabled build target aarch64-unknown-linux-gnu for arm64 architecture ([`1b6df8a`](https://github.com//sharkdp/bat/commit/1b6df8a4802cd4580f515cf63756dad1649290fe))
    - Disabled tab expansion when decorations and pager are not used. ([`eb6e43b`](https://github.com//sharkdp/bat/commit/eb6e43b9a91e1b3d59b208dad8c3889f1ddbc0c8))
    - Updated snapshots. ([`7cdcdbb`](https://github.com//sharkdp/bat/commit/7cdcdbb31dbe8442660415626e22358f8ef9591c))
    - Modified snapshot tests to support tab expansion. ([`9159341`](https://github.com//sharkdp/bat/commit/91593417141c49b6f2a0755f989e7a3fb9bae310))
    - Added tab expansion preprocessing step. ([`b23ff24`](https://github.com//sharkdp/bat/commit/b23ff24ebcca480449fbcf6286d81e7e4429b2a8))
    - Run formatting for bat_style changes ([`cf24986`](https://github.com//sharkdp/bat/commit/cf24986edb0647fe3539f71aedc24b3dd3b9503f))
    - Updated long help to reflect BAT_STYLE changes ([`b9c556a`](https://github.com//sharkdp/bat/commit/b9c556a0b606d07313787a8039604501afc50d36))
    - Added BAT_STYLE env variable functionality ([`9d6bde4`](https://github.com//sharkdp/bat/commit/9d6bde48fa69adcc14633adc46025d9d68239159))
    - README.md: Add `Using bat on Windows` ([`d6c8fee`](https://github.com//sharkdp/bat/commit/d6c8fee04419cf6fb69e51fd62aacc32f83bfef8))
    - Run 'cargo fmt' ([`8b4abb0`](https://github.com//sharkdp/bat/commit/8b4abb03db966df9f5ff235e947f750dddab6729))
    - Formatted for newest rustfmt. ([`375cf76`](https://github.com//sharkdp/bat/commit/375cf76e1599e827115c17a4369a14a57123fc8a))
    - Typo ([`1a4acc4`](https://github.com//sharkdp/bat/commit/1a4acc435e2c8063ae8f30bb62c95e1c423d181f))
    - Automatically disable wrapping when style is plain. ([`d803062`](https://github.com//sharkdp/bat/commit/d8030626f89d1a98342eda683b875bfe7f8fac0d))
    - src/output.rs: Handle `less` in a portable way. ([`76df41f`](https://github.com//sharkdp/bat/commit/76df41fa973a4fb1cd4192b0a5865b8c441cdf2c))
    - Add Crystal syntax ([`97efd75`](https://github.com//sharkdp/bat/commit/97efd75cee5f2b42344df60627a890291960b98a))
    - Update alternatives.md ([`155179a`](https://github.com//sharkdp/bat/commit/155179a07abcbcdbccc9fee26b21c3a0c449fcbd))
    - Add Swift syntax ([`ba00caf`](https://github.com//sharkdp/bat/commit/ba00caf9c3a69de79b19f0ca095e8393f446f992))
    - Add PureScript syntax ([`ac418ba`](https://github.com//sharkdp/bat/commit/ac418ba21cd33861d146441fa59ff24281cd0cc2))
    - Add note about 'lesspipe', closes #242 ([`acde83a`](https://github.com//sharkdp/bat/commit/acde83af311190e6a5f597f620236d93a97c448b))
    - Add F# syntax, closes #284 ([`021a68e`](https://github.com//sharkdp/bat/commit/021a68eb2186b6299fb94781bb4d62965aa5f8b5))
    - Merge pull request #279 from tobenna/add-jsx-syntax ([`566aab3`](https://github.com//sharkdp/bat/commit/566aab3b05ee1dfa1cbca0cd87d685ba24b5b95f))
    - Add section concerning missing zlib-devel ([`f88eba6`](https://github.com//sharkdp/bat/commit/f88eba647611dcbddae327b7cba050744029e4a5))
    - Support "(/usr)/bin/less" as PAGER too ([`1d9e7ac`](https://github.com//sharkdp/bat/commit/1d9e7ac4c2f2b4230b081702ad76ddd096d0f406))
    - Merge branch 'master' into add-jsx-syntax ([`80ae2ec`](https://github.com//sharkdp/bat/commit/80ae2ecbf82541a7808315427c429d4cf8c9009f))
    - Merge pull request #282 from tobenna/support-cabal-syntax ([`96d4492`](https://github.com//sharkdp/bat/commit/96d4492d278cd05aaa75cc730f1f635a409b140d))
    - Merge branch 'master' into support-cabal-syntax ([`c90f0b8`](https://github.com//sharkdp/bat/commit/c90f0b8e21d376291b1118661065907944f1572c))
    - Merge branch 'master' into add-jsx-syntax ([`4b866ab`](https://github.com//sharkdp/bat/commit/4b866ab6afcd62108a9eb5f847cd590b60e443c7))
    - Add Dart syntax ([`2dbd91e`](https://github.com//sharkdp/bat/commit/2dbd91e5432ed0c13af8d9a6bc4e95d0b58c0d35))
    - Add support for cabal syntax ([`785da3d`](https://github.com//sharkdp/bat/commit/785da3d9df0fa5aad0946f02431dea6c6f26e3c8))
    - Add support for JSX in javascript and typescript ([`d455cb7`](https://github.com//sharkdp/bat/commit/d455cb7c3271c2ec6790ea9da5e3dab3bc3de31c))
    - Small README update ([`d2b4766`](https://github.com//sharkdp/bat/commit/d2b4766f9216b0feabc1ca518750c0b8bfa0fe96))
    - Windows download with scoop ([`5cf92b4`](https://github.com//sharkdp/bat/commit/5cf92b4f943beee20ca79cee71c7ac8c1b1f12e5))
    - Ignore enable_ansi_support errors ([`7b803a5`](https://github.com//sharkdp/bat/commit/7b803a50af9ed4eeebd2f48f62c878c181c07a3e))
    - Add OneHalf themes, closes #256 ([`67ec5fe`](https://github.com//sharkdp/bat/commit/67ec5fe2b7156b7ee5358e7f6c4995451ef1bbff))
    - Add CSV syntax, closes #254 ([`3447ed4`](https://github.com//sharkdp/bat/commit/3447ed4def7bee1172f8885cba0cc55440d96a5f))
    - Update README.md ([`532fb92`](https://github.com//sharkdp/bat/commit/532fb921a629a850236b1799f4c810b390d53a37))
</details>

# v0.6.1 (2018-08-31)

## Bugfixes

- Fixed panic when running `bat --list-languages | head`, see #232 (@mchlrhw)
- Respect `--color` settings for `--list-themes` and `--list-languages`, see #233
- Git modifications now work on Windows

## Other

- There will be auto-generated Windows releases, starting with this version (@anykao)

## Commit Statistics

<csr-read-only-do-not-edit/>

 - 20 commits contributed to the release.
 - 0 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' where seen in commit messages

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Bump version number ([`ba4d5a2`](https://github.com//sharkdp/bat/commit/ba4d5a2e7dc38ff8475f63f254cca5e4fb969b2f))
    - Enable ANSI support on Windows ([`026273a`](https://github.com//sharkdp/bat/commit/026273a05c50f9067598ffd6a58b2ad9913f7864))
    - Fix for Rust 1.26 ([`8903b64`](https://github.com//sharkdp/bat/commit/8903b64830a09db249d011d35e0a5537ff4ee0ba))
    - Fix relative-path computation for Windows ([`194155f`](https://github.com//sharkdp/bat/commit/194155f062bc67213c09dea1a91494a55084eba3))
    - Fix terminal width to 80 characters in tests ([`a6ff3b9`](https://github.com//sharkdp/bat/commit/a6ff3b900d70d7344758261ee3fa71dec97f3d81))
    - Add hidden --terminal-width option ([`8a52bcf`](https://github.com//sharkdp/bat/commit/8a52bcf92d6b2f03d1ca25ed4281cc5111b40b30))
    - Split snapshot tests into individual tests ([`0b1535f`](https://github.com//sharkdp/bat/commit/0b1535f63b7f1484623a17a073c59f83af7d733c))
    - Re-enable tests on windows ([`0a5b3ea`](https://github.com//sharkdp/bat/commit/0a5b3eaf3e9268d8117a4f38015076cd85487050))
    - Add new key for AppVeyor ([`1129424`](https://github.com//sharkdp/bat/commit/11294249f8bb581eeac63ddb8778c0cd7b1feea5))
    - Update tests.rs ([`35a1236`](https://github.com//sharkdp/bat/commit/35a1236ad05c3427eebcc30da800b9e03ecf90d1))
    - Update appveyor.yml ([`af8da50`](https://github.com//sharkdp/bat/commit/af8da50c2ca049a016ab38bb97318da0cf49686d))
    - Update appveyor.yml ([`649d1e3`](https://github.com//sharkdp/bat/commit/649d1e3df2b83db875ea11d816c72f212d2de06a))
    - Update appveyor.yml ([`199cfd4`](https://github.com//sharkdp/bat/commit/199cfd4519894539d02c46cfebea7ae867895be0))
    - Add AppVeyor badge ([`c7cce07`](https://github.com//sharkdp/bat/commit/c7cce0752b16eba669567495872c6676cb5df310))
    - Create before_deploy.ps1 ([`3a89486`](https://github.com//sharkdp/bat/commit/3a8948618940aa47f8b28d99c5a660fec68df1ca))
    - Create appveyor.yml ([`720a2fb`](https://github.com//sharkdp/bat/commit/720a2fb7a5d199321e34391c47f08ac3d7fe1af6))
    - Respect color settings for list-themes and list-languages ([`e3c990f`](https://github.com//sharkdp/bat/commit/e3c990f11e48b7b4df889c4706d54e7f04c9fdfe))
    - Use stdout locks ([`90c7d0c`](https://github.com//sharkdp/bat/commit/90c7d0c3659ea08898f341792107a0716188cb20))
    - Fix panic when running `bat --list-languages | head` ([`5600226`](https://github.com//sharkdp/bat/commit/56002267d26deb5c0cf2cb891ee9c28ef0f07255))
    - Update alternatives.md ([`84734ea`](https://github.com//sharkdp/bat/commit/84734eac9d19f90f942e587e7ec7d3aad4cda2c2))
</details>

# v0.6.0 (2018-08-28)

## Features

- The `--list-themes` option now shows a preview for each highlighting theme (@ms2300)
- Added `-p`/`--plain` as an alias for `--style=plain`, see #212 (@ms2300)
- Major refactorings, enabling some progress on #150. In non-interactive mode, `bat` will now copy input bytes 1:1.
- New languages: Elm, Kotlin, Puppet, TypeScript, see #215 #216 #217 #218
- New syntax highlighting theme: zenburn (@colindean)

## Changes

- New themes in `$BAT_CONFIG_DIR/themes` are now loaded *in addition* to
  the default themes (they may also override), see #172
- The `Default.tmTheme` symlink is not necessary anymore.

## Bugfixes

* Using `bat cache --init` leads to duplicated syntaxes, see #206

## Other

* Extended and cleaned-up `--help` text.
* Added initial version of a man page, see #52
* New README sections: *Development* and *Troubleshooting*, see #220

## Commit Statistics

<csr-read-only-do-not-edit/>

 - 36 commits contributed to the release over the course of 8 calendar days.
 - 1 commit where understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' where seen in commit messages

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Add --plain ([`acdea97`](https://github.com//sharkdp/bat/commit/acdea97e71f9baa2801747236492ff29f1632ab6))
    - Bump version ([`839e4ee`](https://github.com//sharkdp/bat/commit/839e4ee611ebcfd9db1541b67f5a6f4f176d0e48))
    - Move theme_preview file to constant ([`c7afcde`](https://github.com//sharkdp/bat/commit/c7afcdebf81c4dc0f4dd8a73ca810ea790ab18dd))
    - Add Troubleshooting section ([`08e6db1`](https://github.com//sharkdp/bat/commit/08e6db15811b9cf5259c6e7898a1be2b70f16869))
    - Fix headline levels ([`4d08ecd`](https://github.com//sharkdp/bat/commit/4d08ecd82864a65559c1cb170bb8e35046e5778e))
    - Add development section to README ([`28e1d0e`](https://github.com//sharkdp/bat/commit/28e1d0e613833f87681df8e341a9ad5e76a8e28b))
    - Update install instructions ([`d11839a`](https://github.com//sharkdp/bat/commit/d11839ad030222423a9a754477d2a47c48a0f433))
    - Include theme_preview file in binary ([`8cacd9b`](https://github.com//sharkdp/bat/commit/8cacd9b43209c87404f587ae1f0ec3245771074a))
    - Run cargo fmt ([`6f67444`](https://github.com//sharkdp/bat/commit/6f67444c992e1c891fea1e380209c1e5cbc6aadb))
    - Small style changes to --list-themes ([`fd2b376`](https://github.com//sharkdp/bat/commit/fd2b376ea0f6b0edb4d117e23e18cd19737abc92))
    - Fix build for Rust 1.26 ([`cda54e4`](https://github.com//sharkdp/bat/commit/cda54e4f61c3d995311aed20319418c8170167ea))
    - Mutation done correctly ([`3b90ada`](https://github.com//sharkdp/bat/commit/3b90ada7354a2a1a1a4344b219d09c1a4fe3ceaf))
    - Added bold to theme titles ([`1837d36`](https://github.com//sharkdp/bat/commit/1837d364fe250c31fd6c2e43a62f94933398e0c7))
    - Changed preview code ([`4ea3e6b`](https://github.com//sharkdp/bat/commit/4ea3e6bbb74f095376098a0b3db63cab4b554fc0))
    - Changed to plain style ([`38544ed`](https://github.com//sharkdp/bat/commit/38544ed735df60423bfbc70656061b1dc30adc24))
    - Fixes mut issue ([`5444b0d`](https://github.com//sharkdp/bat/commit/5444b0dfedfae0c9e86143d0d61a5a0f288b3f92))
    - Modifying changing config for efficiency ([`f3dde01`](https://github.com//sharkdp/bat/commit/f3dde0185d82b77e2bcff358f88be492dea0ee68))
    - #213 : output hello world for each theme when --list-theme is flagged ([`2e3784a`](https://github.com//sharkdp/bat/commit/2e3784aa30ce5a445e08b4ba73e9ca19ffa73798))
    - Add syntaxes for Elm, Kotlin, Puppet, TypeScript ([`1088455`](https://github.com//sharkdp/bat/commit/1088455702e698263e6c5191b67bd1900d5a5263))
    - Adds zenburn theme submodule ([`4f812c3`](https://github.com//sharkdp/bat/commit/4f812c32ab68e3bd99dde4b431be1f7794a1d1c5))
    - Add man page to releases ([`b6c59a5`](https://github.com//sharkdp/bat/commit/b6c59a5bd4e0bcd611cd5e4b15082d944c153b80))
    - Add initial version of man page, see #52 ([`b682679`](https://github.com//sharkdp/bat/commit/b682679776121d223300a4d6f14cfdbc81226947))
    - #212 : added -p as an alias for --style=plain ([`8177143`](https://github.com//sharkdp/bat/commit/81771432fb5f5dccea1f52a2e394aba7d18f84e4))
    - Always call the syntax highlighter ([`627181b`](https://github.com//sharkdp/bat/commit/627181bcb657a5ded61648feaef9c63bc6bc7a8c))
    - Add simple loop-through mode ([`226d9a5`](https://github.com//sharkdp/bat/commit/226d9a573acb8025278d7afc19359ca167841536))
    - Split into Controller and Printer ([`246cf79`](https://github.com//sharkdp/bat/commit/246cf79dbd78665cdeccd5d2a141eb2b3548d582))
    - Refactoring, introduce Printer trait ([`ea955c7`](https://github.com//sharkdp/bat/commit/ea955c734dd9c2796c7e8c49f6054bd045be910d))
    - Document Config struct ([`6223ad6`](https://github.com//sharkdp/bat/commit/6223ad6d528db00be469aa64fb4273e9838bb50f))
    - Document Config struct ([`31a9335`](https://github.com//sharkdp/bat/commit/31a9335bd5b026b42c0ef7d8c5f88079b1eded9e))
    - Major refactoring and cleanup ([`9316f2a`](https://github.com//sharkdp/bat/commit/9316f2a758d015ced23af421e1257f5b926f25c4))
    - Re-order options in help texts ([`c884c3c`](https://github.com//sharkdp/bat/commit/c884c3cc125400d65f705aee9403acc6aa8da416))
    - Update the `--help` text ([`08850f0`](https://github.com//sharkdp/bat/commit/08850f04740c1e1bbe591d1d5bcb2ff8e9a0ddd4))
    - mention BAT_THEME in help output ([`8e6c5bc`](https://github.com//sharkdp/bat/commit/8e6c5bc562c78bde0fc0381554042893e09fe4cf))
    - Load customized themes in addition to defaults ([`052425b`](https://github.com//sharkdp/bat/commit/052425b12f79aa42b1ca6453725b501be87dcc8d))
    - Add new '--blank' option for 'bat cache --init' ([`2df3305`](https://github.com//sharkdp/bat/commit/2df3305b94ff85878b61415d6136c8a5b2d3e439))
    - Clean up help messages ([`6882fc1`](https://github.com//sharkdp/bat/commit/6882fc151235eb9d2a35a0edc057dad463a9168b))
</details>

# v0.5.0 (2018-08-19)

## Features

- Added `--line-range n:m` option to print a range of lines, see #159 (@tskinn)
- The syntax highlighting theme can now be controlled by the `BAT_THEME` environment variable, see [README](https://github.com/sharkdp/bat#highlighting-theme) and #177 (@mandx)
- The `PAGER` and `BAT_PAGER` environment variables can be used to control the pager that `bat` uses, see #158 and the [new README section](https://github.com/sharkdp/bat#using-a-different-pager)
- Added syntax highlighting for Nix, see #180
- Added syntax highlighting for AWK (Gert Hulselmans)

## Changes

- The customization of syntax sets and theme sets is now separated. Syntax definitions are now loaded *in addition* to the ones that are stored in the `bat` binary by default. Please refer to these new sections in the README: [Adding new syntaxes](https://github.com/sharkdp/bat#adding-new-syntaxes--language-definitions), [Adding new themes](https://github.com/sharkdp/bat#adding-new-themes), also see #172
- The color for the filename is now the default foreground color. The colors for the grid and the line numbers is now determined from the syntax highlighting theme, which now also works for light backgrounds, see #178.

## Bugfixes

- Escape Sequences get partially stripped, see #182 (@eth-p)
- Use separate Git repository for snapshot testing, see #165 and #161
- Markdown breaking on JavaScript, see #183

## Other

- Binaries for armv7 are now provided, see #196
- `bat` is now in the official [Arch package repositories](https://www.archlinux.org/packages/community/x86_64/bat/).
- Optimizations in the RGB => 8-bit conversion (@mina86)

## Commit Statistics

<csr-read-only-do-not-edit/>

 - 38 commits contributed to the release over the course of 78 calendar days.
 - 0 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' where seen in commit messages

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Extract grid and line-number color from theme ([`2508323`](https://github.com//sharkdp/bat/commit/25083232640dfe06334c929cb16333a9bd873195))
    - Bump version ([`297afad`](https://github.com//sharkdp/bat/commit/297afad337a1ba6299e17a1f8054e7f6b1ae3d27))
    - Separate syntax set and theme set ([`1dddce3`](https://github.com//sharkdp/bat/commit/1dddce3aa11818fd641542f5f79c20e5ecb5b837))
    - Add arm as a compile target ([`76be0d3`](https://github.com//sharkdp/bat/commit/76be0d3933bc3f86a8537f2319e21a74d6b5a354))
    - Bump minimum required rust version ([`e5b8c44`](https://github.com//sharkdp/bat/commit/e5b8c4471a609991f82183ed6648ffa2b20b130c))
    - Revert "replace trait object BufRead by generic" ([`b191691`](https://github.com//sharkdp/bat/commit/b19169176734e50cabdbd3d70e5d51117595653e))
    - Avoid floating point arithmetic in RGB→8-bit ANSI approximation ([`5c95b88`](https://github.com//sharkdp/bat/commit/5c95b8803b7c6b831f16df85e5775e33b1e5f7ea))
    - Update dependencies ([`5b421b4`](https://github.com//sharkdp/bat/commit/5b421b455d2df28e21f3b49b86ede3a738d12b4a))
    - Add documentation on new pager-customization ([`a6d6c28`](https://github.com//sharkdp/bat/commit/a6d6c28723a033f37140e471b177947c691307d0))
    - Update submodules ([`2966939`](https://github.com//sharkdp/bat/commit/2966939e38e6dae477b5d988a8a9540af1b3c869))
    - Use BAT_PAGER and PAGER environment variables, closes #158 ([`d179693`](https://github.com//sharkdp/bat/commit/d179693d1d4a571d6b5ec3279d09390d6115b39c))
    - Run 'cargo fmt' ([`268577c`](https://github.com//sharkdp/bat/commit/268577c6f99fda02c79715bc93a69655732d1d4c))
    - replace trait object BufRead by generic ([`037861e`](https://github.com//sharkdp/bat/commit/037861e58886fd8220714a1f358de6663549b360))
    - Update README ([`94ccc64`](https://github.com//sharkdp/bat/commit/94ccc646e8b12c61f5979f9f324ac75caf0114fa))
    - Update error-chain to fix compiler warnings ([`0cc5d1d`](https://github.com//sharkdp/bat/commit/0cc5d1dab6593c985f92d71adc5dedc3e34742f7))
    - Always show a warning when theme is unknown ([`28397b8`](https://github.com//sharkdp/bat/commit/28397b8f788a3a6a6ecb2946495fb7df777ef699))
    - Use map instead of and_then(..Some(..)) ([`c899849`](https://github.com//sharkdp/bat/commit/c89984910172d1a8bcd245861e66c2860eca6c38))
    - Small style fix ([`bf53b64`](https://github.com//sharkdp/bat/commit/bf53b641252bf3066d1e52428232dfce557816f7))
    - Allow specifying the theme via the `BAT_THEME` environment variable ([`c68aa0f`](https://github.com//sharkdp/bat/commit/c68aa0f42432761c3f4bbcba70cb36c890901627))
    - Style changes ([`6b57f4e`](https://github.com//sharkdp/bat/commit/6b57f4eebc7f1182cc7aceddfc8a3e23e80337ac))
    - Improve logic for SGR sequence passthrough ([`0ddd388`](https://github.com//sharkdp/bat/commit/0ddd388a29c0653f13981f181917929d7068b5a6))
    - Fix #182 ([`34811b8`](https://github.com//sharkdp/bat/commit/34811b8161cabb8667a87212cdb6e74d7dd665d8))
    - Fix Travis link ([`8b92aae`](https://github.com//sharkdp/bat/commit/8b92aae23ffec20f01fe66bc072dc53516c25dbc))
    - Update syntaxes.bin ([`3a0941c`](https://github.com//sharkdp/bat/commit/3a0941cabe047eb6911706ba0cabfc2b47c88c52))
    - Add Nix syntax, closes #180 ([`52877c6`](https://github.com//sharkdp/bat/commit/52877c6d671d21cfbd515542158c25be668c98f5))
    - Update sublimehq/Packages, closes #183 ([`455de98`](https://github.com//sharkdp/bat/commit/455de98a892234732abd9a47a7b2eadb7cc9bb2c))
    - typo ([`8c6a74a`](https://github.com//sharkdp/bat/commit/8c6a74a6c56dcb884d1fb2d03d31ac83a4a53219))
    - Add AWK syntax. ([`3f8b340`](https://github.com//sharkdp/bat/commit/3f8b340ed8648644b7af6ab2b6d983962cef5109))
    - Change help text for --line-range ([`65e6970`](https://github.com//sharkdp/bat/commit/65e6970907e55c4e0c5cc72037bb0053b33e3673))
    - Run 'cargo fmt' ([`90cc019`](https://github.com//sharkdp/bat/commit/90cc0194820c2ae99ad3ec16e1a1774f950362ce))
    - update ([`eaf0b99`](https://github.com//sharkdp/bat/commit/eaf0b99d256351ab72f537aeac1088fa05647340))
    - use map() ([`3b1dcca`](https://github.com//sharkdp/bat/commit/3b1dcca5d01ccb5f97a85174e0fc4a95c726ff5a))
    - update line-ranges ([`26dbdf4`](https://github.com//sharkdp/bat/commit/26dbdf4fa07534509ad7638a7ac4b425410c6b6e))
    - add line-range ([`6691786`](https://github.com//sharkdp/bat/commit/6691786d828cb44fdd5e7150d9f17d75d7322d37))
    - updates archlinux install instructions ([`50209bf`](https://github.com//sharkdp/bat/commit/50209bfe21474402437770c9f23aee724fc63d92))
    - Always run snapshot test ([`c91511c`](https://github.com//sharkdp/bat/commit/c91511cca14655b40263361684df684310c05dc3))
    - Use separate Git repository for snapshot testing ([`c9f7a0c`](https://github.com//sharkdp/bat/commit/c9f7a0c1268781676d21cd430ef4a8e2bd106b9e))
    - Fix command typo ([`69c798e`](https://github.com//sharkdp/bat/commit/69c798eafd93d174d0340158794b7c1d5f4e598d))
</details>

# v0.4.1 (2018-05-31)

(this is just a small bugfix release, see 0.4.0 for all features and changes)

## Bugfixes

- Fix problem with `cargo test` when `bat` is not checked out in a Git repository, see #161

## Commit Statistics

<csr-read-only-do-not-edit/>

 - 3 commits contributed to the release.
 - 0 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' where seen in commit messages

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Update Cargo.lock ([`195d5e0`](https://github.com//sharkdp/bat/commit/195d5e0e16ba76ee3cc554d8fb30382dd22fcbf8))
    - Skip snapshot test if Git is not available ([`86c1777`](https://github.com//sharkdp/bat/commit/86c17772053b10431531101686aec64d76a08122))
    - Exclude assets from upload ([`16874ab`](https://github.com//sharkdp/bat/commit/16874ab016c869eb2ee4523c1dead126ce6d183c))
</details>

# v0.4.0 (2018-05-31)

## Features

* Support for line-wrapping, see #54 and #102 (@eth-p)
* New and updated `--style` parameter, see #74 and README (@pitkley)
* Added `--theme` and `--list-themes` options, see #89 (@rleungx)
* Added syntax highlighting for: Julia (@iamed2), Dockerfiles, VimL, CMake, INI, Less
* Added a few popular Sublime Text highlighting themes, see #133
* Support for bold, italic and underline font styles, see #96
* Support for 32bit systems is now available, see #84
* Added `-u` and `-n` options, see #134
* ANSI color support on Windows 10

## Changes

* The customization folder for own syntaxes has been renamed from `syntax` to `syntaxes`, see README.
* Changed Markdown syntax to the default Sublime Text syntax, see #157
* Sorted language listing (@rleungx)
* Command line arguments like `--theme` or `--color` can now override themselves.
* Improved `--help` text.

## Bugfixes

- Fixed crash for (really) small terminal sizes, see #117 (@eth-p)
- Syntax detection for `.bashrc`, `CMakeLists.txt` etc., see #100
- Properly handle lines with invalid UTF-8, see #7 (@BrainMaestro)
- Better error handling, see #17 (@rleungx and @sharkdp)
- Proper handling of UTF-8 characters in `less`, see #98 (@ghuls)
- Build fix on nightly, see #148 (@tathanhdinh)

## Other

- [Comparison with alternative projects](https://github.com/sharkdp/bat/blob/master/doc/alternatives.md).
- New "bat" logo in the README, see #119 (@jraulhernandezi)
- Output test cases (@BrainMaestro)
- Lots of great refactoring work (@BrainMaestro)

## Commit Statistics

<csr-read-only-do-not-edit/>

 - 96 commits contributed to the release over the course of 22 calendar days.
 - 0 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 3 unique issues were worked on: [#86](https://github.com//sharkdp/bat/issues/86), [#87](https://github.com//sharkdp/bat/issues/87), [#95](https://github.com//sharkdp/bat/issues/95)

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **[#86](https://github.com//sharkdp/bat/issues/86)**
    - Split modules ([`25cee00`](https://github.com//sharkdp/bat/commit/25cee002f90f2d67311fe448f4489f9afe48f904))
 * **[#87](https://github.com//sharkdp/bat/issues/87)**
    - Add 32bit support ([`6343535`](https://github.com//sharkdp/bat/commit/6343535eb2f23416431bb8e5a00e4fd734df78ba))
 * **[#95](https://github.com//sharkdp/bat/issues/95)**
    - add theme option ([`22c8978`](https://github.com//sharkdp/bat/commit/22c8978fcad508bd8c367306f5eb127e601c6e42))
 * **Uncategorized**
    - Updates for 0.4 ([`bf5883f`](https://github.com//sharkdp/bat/commit/bf5883f06f94f5af329cb8d7a8dcac5fbfc1524b))
    - Fix clippy warnings ([`e27bdfc`](https://github.com//sharkdp/bat/commit/e27bdfc7d66f0b9a98ff0dc78113cccb96fefb19))
    - Update README.md ([`a7f4f20`](https://github.com//sharkdp/bat/commit/a7f4f202cb5b6b07cfc890461b4dd22b47f95690))
    - Add TOC ([`3972233`](https://github.com//sharkdp/bat/commit/397223374a84bcc7bccbb7e2377ea03380645f22))
    - Change color of crates.io badge ([`0c2da90`](https://github.com//sharkdp/bat/commit/0c2da90fa4144c6aa71d1cc10e432c7034207f55))
    - Add license badge ([`fe29da6`](https://github.com//sharkdp/bat/commit/fe29da6e7660b350835978ea48f533077a2a15fd))
    - Update header ([`0eccfb5`](https://github.com//sharkdp/bat/commit/0eccfb50d909710b142809f90cf9b6103c9a4326))
    - Update syntect ([`f74263e`](https://github.com//sharkdp/bat/commit/f74263e71dc27ba988c99bd179fe53469e0f4a69))
    - Update syntaxes, fix CMake syntax problem ([`54f4896`](https://github.com//sharkdp/bat/commit/54f4896cedd24350dd0175cd0c47f3270b86fe72))
    - Use default Markdown syntax, closes #157 ([`9d92350`](https://github.com//sharkdp/bat/commit/9d92350cbb732a7f022d1031bbad5a6ad9179cd6))
    - Add 'rouge' to list of alternatives ([`26d409d`](https://github.com//sharkdp/bat/commit/26d409db65f155148fa5c2296570b031f093dc6b))
    - Add 'coderay' to list of alternatives ([`06cd4b2`](https://github.com//sharkdp/bat/commit/06cd4b2ce82b780e48e3bbc6a9d4daab3258c11a))
    - Handle syntax-set loading errors ([`a348a9f`](https://github.com//sharkdp/bat/commit/a348a9f68e66855ee8334a361e7cfa4cb285392f))
    - Add .ignore files to avoid searching asset folders ([`39e2971`](https://github.com//sharkdp/bat/commit/39e2971e96137bd61fb3f577164728cfbcaeb55b))
    - Using slice notation instead of as_ref ([`a13eb60`](https://github.com//sharkdp/bat/commit/a13eb60df3ccec603fe55524a9a411494b1153bf))
    - Explicitly specify which args override themselves ([`9342d0b`](https://github.com//sharkdp/bat/commit/9342d0bb24a468238dd2f836de3c51032d9d4896))
    - Allow arguments to override themselves ([`36d7250`](https://github.com//sharkdp/bat/commit/36d7250af2f27d67ebdb9a195acbb894206831b4))
    - Build bat before generating snapshots ([`2ce8655`](https://github.com//sharkdp/bat/commit/2ce865516797c4b8dd52873f224050aff34ef6ee))
    - Remove unnecessary space if there are no decorations ([`a452467`](https://github.com//sharkdp/bat/commit/a452467e064a7a5be5bc1a4ba7eeb88219dd50b1))
    - Justify header when there are decorations but no grid ([`2ea3758`](https://github.com//sharkdp/bat/commit/2ea3758c4b9cfdfce3612dc4fd425b785c8942e1))
    - Print themes directory in error message if no themes could be loaded from it. ([`65bb4c7`](https://github.com//sharkdp/bat/commit/65bb4c7ee6833913596bcf9da04953681b2077c3))
    - Add comparison benchmark ([`556149a`](https://github.com//sharkdp/bat/commit/556149ad3a0a85c4139cfe3dafbab45cbb4c5443))
    - Fix launching of pager so text is interpreted as UTF-8 by default. ([`33bbf25`](https://github.com//sharkdp/bat/commit/33bbf254684b3cff3f5af01b77ea83b452c9822b))
    - Add logo header to README ([`5ee8913`](https://github.com//sharkdp/bat/commit/5ee891353810e19f7d510b50483291c1e9f8a6aa))
    - Add more benchmark results ([`9b1c1c0`](https://github.com//sharkdp/bat/commit/9b1c1c02717d6b0b8fa3de84f56000194ff62ac0))
    - Move asset clearing to assets module ([`2712d63`](https://github.com//sharkdp/bat/commit/2712d63a4bb2fe6dc46bbd5a6a2917ec4718bd6f))
    - Extract features to separate module ([`b21fb6b`](https://github.com//sharkdp/bat/commit/b21fb6bca8604b096bcc19134cce939dfdd7acd3))
    - Move colors to printer module ([`ee43377`](https://github.com//sharkdp/bat/commit/ee43377a9cc20498021c1205e6826cc501425dcc))
    - Move output to separate module ([`120b33a`](https://github.com//sharkdp/bat/commit/120b33a9dbc88cc21ba0d1a99b72aa655a77792a))
    - Add project goals and list of alternatives ([`17b37fe`](https://github.com//sharkdp/bat/commit/17b37fedcb70ab42dfb2e6b866fe6cf04adda8b9))
    - Bump version ([`1013cdd`](https://github.com//sharkdp/bat/commit/1013cdd80afc9135281489a2fd6d2a8e73cd32e4))
    - Extended help texts for '--help' ([`be0a2d6`](https://github.com//sharkdp/bat/commit/be0a2d6048d2819691cc6102c38261422b9f7331))
    - Add -u and -n options, see #134 ([`d1a1943`](https://github.com//sharkdp/bat/commit/d1a1943998038996f409910c6e461cf2d584f1ae))
    - Permissive error handling, closes #17 ([`2a9f5a2`](https://github.com//sharkdp/bat/commit/2a9f5a24edb1a24a1b17065bfaa460a297c7452f))
    - Reset line number to 0 for new files ([`a0ae089`](https://github.com//sharkdp/bat/commit/a0ae089c4a61bb77f1acb8daf87e5c1043b557ac))
    - Skip '--quit-if-one-screen' for --paging=always ([`96cc391`](https://github.com//sharkdp/bat/commit/96cc391f2d5865ba256d61b823c50225fc12e7e2))
    - Add a few popular Sublime Text themes ([`a1e1170`](https://github.com//sharkdp/bat/commit/a1e117031903819fcfaf791345b57184abd7cd0e))
    - Add syntax docu, add VimL, CMake, INI, LESS syntax ([`54a331d`](https://github.com//sharkdp/bat/commit/54a331d162c8f2fa872f223102508753e3ba9939))
    - Fix code formatting ([`5e3b17e`](https://github.com//sharkdp/bat/commit/5e3b17e6f7041537ce670d75aab130962b39a755))
    - Fixed #124 ([`486e6a1`](https://github.com//sharkdp/bat/commit/486e6a19cd3b569a0098046194a614ed6e0cf03f))
    - Extract syntax finding to assets module ([`247dfbe`](https://github.com//sharkdp/bat/commit/247dfbee83098f8542b1a5e130d31bcf3a972e79))
    - Update snapshots ([`882931a`](https://github.com//sharkdp/bat/commit/882931a77bf280b022e90461b51896d320967f2f))
    - Handle line with invalid UTF-8 ([`12cb438`](https://github.com//sharkdp/bat/commit/12cb438aa4bcfcfea3292e31ca2bdf447839cc95))
    - Add line with invalid UTF-8 ([`d0782ef`](https://github.com//sharkdp/bat/commit/d0782ef954fb836a7cb5a31823de981ff3d8b827))
    - Better error handling ([`c826c2a`](https://github.com//sharkdp/bat/commit/c826c2a43857d73b1cadc35d0fff5bc0951be70a))
    - Update to latest syntect, closes #100 ([`9aa97b6`](https://github.com//sharkdp/bat/commit/9aa97b6c222c11a99e12fc7e273a93f265e6eb13))
    - Add syntax for Dockerfiles ([`f789087`](https://github.com//sharkdp/bat/commit/f7890870ea4fb582c5d3ec5d749b1509695bc5ab))
    - Include syntaxes and themes in repository ([`145b99f`](https://github.com//sharkdp/bat/commit/145b99f01c19416b5d6419d6bcf3d51a771a5518))
    - Add FreeBSD installation instructions ([`9af1d2b`](https://github.com//sharkdp/bat/commit/9af1d2b8917710e3c2ce0a591af5f8bdd0101635))
    - Merge pull request #102 from eth-p/master ([`2eee685`](https://github.com//sharkdp/bat/commit/2eee68599df671f7fd067a1121a4d01039c49d8e))
    - Formatted with newer cargo fmt. ([`d569693`](https://github.com//sharkdp/bat/commit/d569693dab51d95e5e49be25435df2c4ba64ef37))
    - Fix #117 ([`900f610`](https://github.com//sharkdp/bat/commit/900f61032d36eb896a0fc3baf5bd630e38c04a0d))
    - Replace for_line and for_wrap with generate ([`d0ca566`](https://github.com//sharkdp/bat/commit/d0ca5669794902a78eebb14b2ca61b01a67c9ee6))
    - Add simple benchmark scripts ([`54b33c8`](https://github.com//sharkdp/bat/commit/54b33c8c21d327808bfdbd45083daf4638bfaff6))
    - address comment ([`a5de77a`](https://github.com//sharkdp/bat/commit/a5de77af4dcbd6241a692781fb50836174ba771e))
    - sort the language listing ([`870ff7b`](https://github.com//sharkdp/bat/commit/870ff7b538207e1f7d444cd8d47f9774f66a40d0))
    - Updates for new rustfmt ([`15b9acd`](https://github.com//sharkdp/bat/commit/15b9acd12f2a7bc623ad92d1270fc3ac80236ccb))
    - Fix \r character being printed with wrapping enabled. ([`a5a7dc1`](https://github.com//sharkdp/bat/commit/a5a7dc14e430ed3f097cbef60abaa3bb19161cdc))
    - Add ansi_term import for Windows, see #107 ([`fcecd97`](https://github.com//sharkdp/bat/commit/fcecd97224926625cf542e765d0f324c0b6e6c02))
    - Split decorations into a separate file and optimized them a bit. ([`b327127`](https://github.com//sharkdp/bat/commit/b327127f373b104b46d062caa5b7016bc02c3a77))
    - PR #102 Followed @sharkdp's suggestions ([`9214a4a`](https://github.com//sharkdp/bat/commit/9214a4a4f08a1614e40e01235b24c4f175546e77))
    - Ignore errors when clearing cache, closes #91 ([`b9c780c`](https://github.com//sharkdp/bat/commit/b9c780c8c80d1c9bcef7c8e651ed84857f2b9e0d))
    - Enable ANSI support on Windows 10 ([`ca865f9`](https://github.com//sharkdp/bat/commit/ca865f9d260d9c34d49dff9c756758b4d31b1633))
    - Update create.sh script ([`3ee0e56`](https://github.com//sharkdp/bat/commit/3ee0e56315b7fce6448c9a545308a252e86d23ab))
    - Add Julia highlighting support ([`091b0b6`](https://github.com//sharkdp/bat/commit/091b0b6437feacc6061ba9262fabe9ff56dd68b1))
    - PR #102 Followed @BrainMaestro's suggestions ([`870b3c0`](https://github.com//sharkdp/bat/commit/870b3c0daf4d7d5467c51479fbea821f1d882e83))
    - Add snapshot testing ([`0886a24`](https://github.com//sharkdp/bat/commit/0886a24685dbab13bfcf186d9c5d0891d8100344))
    - Add generated snapshots ([`53eb9c5`](https://github.com//sharkdp/bat/commit/53eb9c57d6b75f9edc8c1d4178abc6ca7a312f14))
    - Add sample files and snapshot generator ([`026a9eb`](https://github.com//sharkdp/bat/commit/026a9ebae3d867e45e642969d988ca0473f5014a))
    - Update snapshot test files. ([`9f005d1`](https://github.com//sharkdp/bat/commit/9f005d115d8d1fc3de4bb6ef09f0ce79ad142426))
    - Fix double spaces when outputting without wrapping. ([`b4cfc96`](https://github.com//sharkdp/bat/commit/b4cfc9633c30cbf6d98fcce670caf4bf22dbc4bb))
    - Merge branch 'master' of https://github.com/sharkdp/bat ([`f7e055b`](https://github.com//sharkdp/bat/commit/f7e055b6b7655d0ab013777ea0469a4effe7fd3d))
    - Fix padding, add --wrap argument, disable wrap for non-tty. (Fixed) ([`d4b438b`](https://github.com//sharkdp/bat/commit/d4b438b9d3211b6382f130309bdd54589908e7bd))
    - Add bold, italic and underline font styles ([`cb7b158`](https://github.com//sharkdp/bat/commit/cb7b158172a8336778dc860795a1f2cdc0180356))
    - Fix padding, add --wrap argument, disable wrap for non-tty. ([`cd26d40`](https://github.com//sharkdp/bat/commit/cd26d403a3ba327b81030cb8ba5c2b2c6470028c))
    - Fix off-by-one error with text wrapping and --style grid ([`f95a23f`](https://github.com//sharkdp/bat/commit/f95a23f9482804148923861cc5decf83ca7c3311))
    - Merge branch 'master' into master ([`bdd10b8`](https://github.com//sharkdp/bat/commit/bdd10b8becc277f40899965385cd5fe4272eb7f2))
    - PR #102 Review Changes ([`fc160b0`](https://github.com//sharkdp/bat/commit/fc160b0dcdcd47708cd152b3a7c6848de61c1bef))
    - Forgot to add styling to the horizontal line. ([`fcc36b1`](https://github.com//sharkdp/bat/commit/fcc36b1f79c56d7d5c5d17de4216189c39b3d0da))
    - Added line wrapping. ([`4e4110b`](https://github.com//sharkdp/bat/commit/4e4110bf5015009d584ed0ed5d09d19eb5393430))
    - Split style to separate module ([`f711fb5`](https://github.com//sharkdp/bat/commit/f711fb500629e9331b5c413a62e86cc24e06653c))
    - Split app to separate module ([`64a9341`](https://github.com//sharkdp/bat/commit/64a9341b73b1e677f4fca752033af1fd01dde761))
    - Add and use --style=auto by default ([`ccf88fd`](https://github.com//sharkdp/bat/commit/ccf88fd5d8e767b9306e78ad954cf8cf3d4e3d28))
    - Fix clippy warnings ([`e2ac6de`](https://github.com//sharkdp/bat/commit/e2ac6de7832c4de491e12e9c7d7c5725191cc2db))
    - Add MacOS install instructions, closes #33 ([`13a53d2`](https://github.com//sharkdp/bat/commit/13a53d230b32a46ecc95db9e9b80b4e6810b252d))
    - Remove unused lifetimes ([`ea27053`](https://github.com//sharkdp/bat/commit/ea27053a632a1d379e8beba14fcdaf2fcb7e81a7))
    - Fix header-line if grid is not requested ([`b2b932f`](https://github.com//sharkdp/bat/commit/b2b932f3ed4510c3d08a72fa8330d14cc1340e4f))
    - Replace `Cow` by `String` ([`747d074`](https://github.com//sharkdp/bat/commit/747d074be129f16c34633169b62414a3fa699d57))
    - Create `OutputComponents` struct, use HashSet ([`61109ec`](https://github.com//sharkdp/bat/commit/61109ece15946f11aecf5508440f101aa1818f4c))
    - Combine `OutputComponent`s and `PredefinedStyle`s ([`389edd7`](https://github.com//sharkdp/bat/commit/389edd7239304959b50a18b47eda0db1d5a8b4c1))
    - Make `--style` parameter more flexible ([`23813cc`](https://github.com//sharkdp/bat/commit/23813cc08bd94bd52775922a3fe94cd019b64178))
    - Add usage examples ([`fdeb98e`](https://github.com//sharkdp/bat/commit/fdeb98e8593cc5726cabfec6c3593c183d46c404))
    - Add comment about paging & concatenation ([`6a41788`](https://github.com//sharkdp/bat/commit/6a417886aad37a75b659d98027e9bb38c542cb80))
</details>

# v0.3.0 (2018-05-08)

## Features

* Automatic paging by integrating with `less`, see #29 (@BrainMaestro)
* Added support for reading from standard input, see #2
* Added support for writing to non-interactive terminals (pipes, files, ..); new
  `--color=auto/always/never` option, see #26 (@BrainMaestro)
* Added `--list-languages` option to print all available syntaxes, see #69 (@connorkuehl)
* New option to specify the syntax via `-l`/`--language`, see #19 (@BrainMaestro)
* New option to control the output style (`--style`), see #5 (@nakulcg)
* Added syntax highlighting support for TOML files, see #37

## Changes

* The `init-cache` sub-command has been removed. The cache can now be controlled via
  `bat cache`. See `bat cache -h` for all available commands.

## Bug fixes

* Get git repository from file path instead of current directory, see #22 (@nakulcg)
* Process substitution can now be used with bat (`bat <(echo a) <(echo b)`), see #80

## Thanks

I'd like to say a big THANK YOU to all contributors and everyone that has given us
some form of feedback.

Special thanks go to @BrainMaestro for his huge support with new features, bug reports
and code reviews!

## Commit Statistics

<csr-read-only-do-not-edit/>

 - 43 commits contributed to the release over the course of 5 calendar days.
 - 0 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 1 unique issue was worked on: [#43](https://github.com//sharkdp/bat/issues/43)

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **[#43](https://github.com//sharkdp/bat/issues/43)**
    - Add --style option to disable line numbers and git markers ([`7df9a5f`](https://github.com//sharkdp/bat/commit/7df9a5fe82ddae93190530709c888bfe2367bc75))
 * **Uncategorized**
    - Append newline at the end of the file if necessary ([`8b9991d`](https://github.com//sharkdp/bat/commit/8b9991d7808d65782844ac68a2f9c53fa4225dc6))
    - Update dependencies ([`f59ddd1`](https://github.com//sharkdp/bat/commit/f59ddd1e3607664800b58657404215e8e8cb7592))
    - Do not peek at FIFOs, closes #80 ([`f90b9d1`](https://github.com//sharkdp/bat/commit/f90b9d1accac7ba429cd541c1af7312b088d27de))
    - Filter out languages without extensions ([`f7af537`](https://github.com//sharkdp/bat/commit/f7af53701896cf672bcec10bfd7aadccceed0fa3))
    - Paint file extensions in green ([`fd68af8`](https://github.com//sharkdp/bat/commit/fd68af80310938fe564e1b55f2cc1ee44577aa13))
    - Compute 'longest' for non-hidden languages only ([`ebdef04`](https://github.com//sharkdp/bat/commit/ebdef04c8f9f6f5b1145f11e8d17688890ec3e80))
    - Correctly update num_chars ([`a415060`](https://github.com//sharkdp/bat/commit/a4150600af83606463d0faaef7657e2a37f2fedd))
    - Remove term_width from Printer, its options owns it. ([`49223ee`](https://github.com//sharkdp/bat/commit/49223eed06a0b0a980eeff123c31746906d0139d))
    - Add revisions for line-wrapping and terminal width. ([`7dc7133`](https://github.com//sharkdp/bat/commit/7dc7133b3c82ab7034b16020a028f8b3bdab697e))
    - Add term_width to options struct, move getting term_width to run() ([`e5e4771`](https://github.com//sharkdp/bat/commit/e5e47716b0ca22fd2ee0e3538d9a6d22359c4056))
    - Filter out languages that are hidden. ([`b4f8cd3`](https://github.com//sharkdp/bat/commit/b4f8cd3bae4fb7988ad238579714efb646a85efa))
    - WIP edit desired width. ([`ee3a37f`](https://github.com//sharkdp/bat/commit/ee3a37f3fcdcea213cf7873125ce1b0fc95ee404))
    - Add simple line-wrapping for file extensions. ([`4c60ab1`](https://github.com//sharkdp/bat/commit/4c60ab12ccf546f6f8269c3dcf11398f094b800b))
    - Adds requested revisions. ([`ac32dd1`](https://github.com//sharkdp/bat/commit/ac32dd17c9fd326dfa43b11c3da7d28e2842ede1))
    - Rename and add comment to the match block. ([`4e3c57c`](https://github.com//sharkdp/bat/commit/4e3c57cc7ce297f102ae6bff5d7af0cbc5aed1fa))
    - Size language name column width to the length of the longest language. ([`438a9a9`](https://github.com//sharkdp/bat/commit/438a9a99ed498eae769abad31c998d7f6027c5d0))
    - Display language and extensions from loaded assets. ([`9b08771`](https://github.com//sharkdp/bat/commit/9b0877102c590b804a91873ca8efda14cf261276))
    - Add --list-languages argument to clap. ([`67bed73`](https://github.com//sharkdp/bat/commit/67bed73e1507345a664239be15f0fb543a153421))
    - Improve cache subcommand ([`ef7c39b`](https://github.com//sharkdp/bat/commit/ef7c39b159e920d30badf69a4c869e8527432d9a))
    - Disable paging-mode if we read from an interactive TTY ([`8a399c8`](https://github.com//sharkdp/bat/commit/8a399c8d7d7eb7f3ab9dc16ecd462824abf82932))
    - Simplify stdout handling ([`9a0e444`](https://github.com//sharkdp/bat/commit/9a0e444e09e652eccf8603dc477ee80c0c164fbb))
    - Paint STDOUT bold, unify writeln statement ([`438f3df`](https://github.com//sharkdp/bat/commit/438f3df34567448c915a262c47bf05cb67fcc884))
    - Add support for reading from stdin, closes #2 ([`7e2e0c8`](https://github.com//sharkdp/bat/commit/7e2e0c82ac44583119a5e61cd438b28e9f89d409))
    - Re-format README ([`da92154`](https://github.com//sharkdp/bat/commit/da9215416376d42afe16c794f9b9d54d6e478e72))
    - Mention cmake in build instructions, closes #57 ([`179bd4f`](https://github.com//sharkdp/bat/commit/179bd4f525b0afa640ccf1df495e26fbdc251368))
    - Add printer ([`53d67e2`](https://github.com//sharkdp/bat/commit/53d67e2b6ee4a674a8bb19793d891d1857f02167))
    - Use syntect newlines mode (instead of nonewlines) ([`b625d07`](https://github.com//sharkdp/bat/commit/b625d07c3401237a0e02d2947106e861179654e3))
    - Add option to disable automatic paging ([`ec606e5`](https://github.com//sharkdp/bat/commit/ec606e5dcc9eabb570b98abcf3d99c89fd0f12db))
    - Open pager once for multiple files ([`c253821`](https://github.com//sharkdp/bat/commit/c253821a5e2efa3164a7445ee39fc3f036818da4))
    - Remove short versions for --style and --color for now ([`ccb1d78`](https://github.com//sharkdp/bat/commit/ccb1d78b22181505aa6e6de024f1b3d5762c0182))
    - Discover syntaxes by name and extention ([`9eb64b9`](https://github.com//sharkdp/bat/commit/9eb64b97fe7a227a16768ee71bf00a5352b58944))
    - Use 'plain' style for non-interactive terminals ([`15f0268`](https://github.com//sharkdp/bat/commit/15f0268bdcf91aa3fe84c5e99796d4c5faf72442))
    - Run 'cargo fmt' and enforce via Travis ([`3fa70de`](https://github.com//sharkdp/bat/commit/3fa70deaa7777af32b9480961b91bf70c2d04f20))
    - Add color flag ([`d4553c6`](https://github.com//sharkdp/bat/commit/d4553c6b386c2c71a9a177c3cdcb530eaf7ec59b))
    - Add TOML sublime syntax, closes #37 ([`23d92d7`](https://github.com//sharkdp/bat/commit/23d92d7641ed81d92fc3d758bb7351a8f29964fa))
    - Add elixir sublime syntax ([`9272943`](https://github.com//sharkdp/bat/commit/92729430fcc45cb1e1ad8ac07555be241efb9b33))
    - Compute pathspec from absolute.strip_prefix(workdir) ([`3eb79d6`](https://github.com//sharkdp/bat/commit/3eb79d63ceda959a7b2c1ae46ea34593656d1488))
    - Remove user specific change ([`0483407`](https://github.com//sharkdp/bat/commit/04834077753e2fedc476f13d3fe1cd494aa4e93b))
    - Fix bug where git modification markers would not be shown if directory was not cwd ([`cbdf5c5`](https://github.com//sharkdp/bat/commit/cbdf5c50c475c3a5720fca36547697f846dbe063))
    - Add less pager ([`418b3c5`](https://github.com//sharkdp/bat/commit/418b3c5ea1b0673d061f29ce268a4ee6c07c5219))
    - Hide everything but content for plain option style ([`9dca312`](https://github.com//sharkdp/bat/commit/9dca3126b330c654c3958f6d9af1b53cf6ef99f2))
    - Add option to specify language ([`4bba080`](https://github.com//sharkdp/bat/commit/4bba08062ca5885584458b6a0270793e6f928022))
</details>

# v0.2.3 (2018-05-03)

- Added a new statically linked version of bat (`..-musl-..`)

## Commit Statistics

<csr-read-only-do-not-edit/>

 - 5 commits contributed to the release.
 - 0 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' where seen in commit messages

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Bump version ([`4e8ea81`](https://github.com//sharkdp/bat/commit/4e8ea815af643ebfa93a3264bf228e6b92f6a254))
    - Add statically-linked version of bat, closes #46 ([`b7bc2d5`](https://github.com//sharkdp/bat/commit/b7bc2d54baa770e4784b189163b7eddfc68473b4))
    - Bump minimum required version to 1.24 ([`21f9e2b`](https://github.com//sharkdp/bat/commit/21f9e2b0f79b41037c7195898937f106a65340d9))
    - Update README.md ([`92aa2b5`](https://github.com//sharkdp/bat/commit/92aa2b547eb1bb4766241695eced8f76a64123a8))
    - Small update to README ([`65f66d0`](https://github.com//sharkdp/bat/commit/65f66d0beeda348e675464b4e98359dbac15553b))
</details>

# v0.2.2 (2018-05-02)

- Remove openssl dependency completely, see #30.

## Commit Statistics

<csr-read-only-do-not-edit/>

 - 2 commits contributed to the release.
 - 0 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' where seen in commit messages

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Bump version ([`f5a9236`](https://github.com//sharkdp/bat/commit/f5a9236a47d75d336c9408b197c3f01e21f3e31d))
    - Remove openssl dependency ([`edbe8f3`](https://github.com//sharkdp/bat/commit/edbe8f3e1a3dc6c52dec3b50e0e772dfb82b509c))
</details>

# v0.2.1 (2018-05-01)

- Added Elixir syntax, see #25.
- Use libcurl-openssl instead of libcurl-gnutls, see #30.

## Commit Statistics

<csr-read-only-do-not-edit/>

 - 7 commits contributed to the release.
 - 0 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' where seen in commit messages

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Bump version to 0.2.1 ([`ed09a80`](https://github.com//sharkdp/bat/commit/ed09a80a731bc5b93ea047f9da098355b880e0a3))
    - Add test-output ([`802540e`](https://github.com//sharkdp/bat/commit/802540ea0837c06ca6cfdf3ce637fc0f3c57200a))
    - Use openssl ([`dbf84e7`](https://github.com//sharkdp/bat/commit/dbf84e77e0247ffec33ff558e7c87f8858ea975e))
    - Add Arch Linux installation instructions to README ([`8f18567`](https://github.com//sharkdp/bat/commit/8f18567f2d98374ecdc3b61dd5bdfaff6a18cd58))
    - Update README.md ([`e4d7e00`](https://github.com//sharkdp/bat/commit/e4d7e004ee701cf1620a0d5b3f7cc2bd4e4370eb))
    - Update syntax_set, closes #25, fixes #28 ([`51b57cc`](https://github.com//sharkdp/bat/commit/51b57ccdb7c63665bccea2b87b79be27fd2ad1ef))
    - Remove explicit bincode dependency ([`25f8361`](https://github.com//sharkdp/bat/commit/25f83610f94100ed76915381f04ef4310f9068b7))
</details>

# v0.2.0 (2018-04-30)

- Support for custom syntaxes, add 'Markdown extended' theme
- Bugfix: Git modifications not shown from child folder

## Commit Statistics

<csr-read-only-do-not-edit/>

 - 24 commits contributed to the release over the course of 6 calendar days.
 - 0 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' where seen in commit messages

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Update README.md ([`7f070c9`](https://github.com//sharkdp/bat/commit/7f070c9dcb05d52482f0d1175261c40e7451daa8))
    - Add customization chapter ([`64ef61b`](https://github.com//sharkdp/bat/commit/64ef61b40927d0ac9881cace2b6e37dfc9706f90))
    - Update README.md ([`95a2079`](https://github.com//sharkdp/bat/commit/95a2079bcd1f93cf34f4bc62ee022cef7ed07223))
    - Add binary assets ([`cc60ed6`](https://github.com//sharkdp/bat/commit/cc60ed656386f3435d6b73cbaa296a467e2e3d47))
    - Update install instructions, closes #16 ([`50be143`](https://github.com//sharkdp/bat/commit/50be143c6294bfff2773c798a2ec83437a8e6333))
    - Load assets from the binary ([`59fabd8`](https://github.com//sharkdp/bat/commit/59fabd8ca5a0bde7a3c65a6e54403411244ddbaf))
    - Better error messages ([`ced6801`](https://github.com//sharkdp/bat/commit/ced68017408972ba613854d8002d38934f0918e4))
    - Implement SyntaxSet and ThemeSet caching ([`f81e386`](https://github.com//sharkdp/bat/commit/f81e38618ccaf7c3c7b0ef953d2f472695e01ade))
    - Update README.md ([`1ff46aa`](https://github.com//sharkdp/bat/commit/1ff46aaaf8a69f9c0a98759e0ad6b04b19311292))
    - Specify syntect features ([`74d023a`](https://github.com//sharkdp/bat/commit/74d023a7b1cf057e06e283a65a40f02a23aab551))
    - Use error-chain ([`36bbc77`](https://github.com//sharkdp/bat/commit/36bbc770ebb33883d21e55e3d44605fdd00518fc))
    - Fix clippy warnings ([`f43409f`](https://github.com//sharkdp/bat/commit/f43409f4739f83c726742ca090172db5b8f33766))
    - Update README.md ([`203e4f9`](https://github.com//sharkdp/bat/commit/203e4f9249cd13839bac29365fde7b6464daf524))
    - Update README.md ([`22e785c`](https://github.com//sharkdp/bat/commit/22e785ca498f5b23bde91e1f897830f06818da26))
    - Bump to rust 1.22.1 ([`5707259`](https://github.com//sharkdp/bat/commit/57072597d61e7183e54e0a90f6337ab1d4a4c13c))
    - Update README.md ([`fb8ef40`](https://github.com//sharkdp/bat/commit/fb8ef40f3eb0c1efae7c88b1dd1262880d4dd8cf))
    - Update README.md ([`0e8e39a`](https://github.com//sharkdp/bat/commit/0e8e39a59dbb96862af50fa184081c6f479fc708))
    - Print error if Default.tmTheme is not available ([`64de2a4`](https://github.com//sharkdp/bat/commit/64de2a44d56615c24115566ac75634c257c3fa6b))
    - Pin minimum required rust version to 1.22 ([`716b261`](https://github.com//sharkdp/bat/commit/716b261efdc25073f8352e52e239d19d206f4377))
    - Add minimum required rust version ([`58ca1f6`](https://github.com//sharkdp/bat/commit/58ca1f66d6599118003eb478bd9f9e92f4f1d0d6))
    - Fix #13 ([`86dcb3c`](https://github.com//sharkdp/bat/commit/86dcb3c165e7171ac981ac874e44a1302c47d98d))
    - Update header ([`7b7a5a3`](https://github.com//sharkdp/bat/commit/7b7a5a32d67858b519958b8fb0cb1f14ef77b4f3))
    - Support for custom syntaxes, add 'Markdown extended' theme, closes #10 ([`a7232a6`](https://github.com//sharkdp/bat/commit/a7232a6ecc3daaad2d558f3a594baf652128f8e8))
    - Enable 8 bit color support, closes #11 ([`38762c3`](https://github.com//sharkdp/bat/commit/38762c34d9180ca9e34302bf7f666a9f73a41618))
</details>

# v0.1.0 (2018-04-22)

Initial release

## Commit Statistics

<csr-read-only-do-not-edit/>

 - 22 commits contributed to the release over the course of 1 calendar day.
 - 0 commits where understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' where seen in commit messages

## Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Update .travis.yml ([`9435e16`](https://github.com//sharkdp/bat/commit/9435e16a1be6e7fab41a067284d707217b42afb5))
    - Add install instructions ([`71e40b1`](https://github.com//sharkdp/bat/commit/71e40b1963c9a9fc2f16047b73bc6f183164a2d6))
    - Add badges ([`7f4781c`](https://github.com//sharkdp/bat/commit/7f4781cdbff97b471b4cb039e0f6b12268817f76))
    - Load themes from ~/.config/bat/themes ([`de01989`](https://github.com//sharkdp/bat/commit/de0198920ac4772bab382386e72b1b081d92a595))
    - Update minimal rust version ([`a978ec7`](https://github.com//sharkdp/bat/commit/a978ec760458f6b5afea918141d4dca5d23dc7da))
    - Update README.md ([`8d83da2`](https://github.com//sharkdp/bat/commit/8d83da23f001edfc19d0b67de70a8659c11c1064))
    - Only build on x86_64 ([`77c1f8f`](https://github.com//sharkdp/bat/commit/77c1f8f85e3a3ac20ba7c2fea966537b71848653))
    - Add CI scripts ([`14615eb`](https://github.com//sharkdp/bat/commit/14615ebd43297b6eb2ee310ed5d556b387cbd125))
    - Add .travis.yml ([`eded921`](https://github.com//sharkdp/bat/commit/eded921a307e2117cabdc955462a9e39f04b3ff9))
    - Add license ([`ecb56ba`](https://github.com//sharkdp/bat/commit/ecb56ba52cee1a597b86a7c9f7f00aaa7ddc9413))
    - Added screenshots ([`eddd07e`](https://github.com//sharkdp/bat/commit/eddd07ea39c6c941a4e9c2e6b4a42cf19058a209))
    - Rename get_line_changes ([`b5c119e`](https://github.com//sharkdp/bat/commit/b5c119e802f19d3aed147485b98c647f707ce9f2))
    - Handle broken pipes, closes #9 ([`1f2bcf5`](https://github.com//sharkdp/bat/commit/1f2bcf57baf92c83f8668f7908532d2b849e83ee))
    - Code restructuring ([`de11558`](https://github.com//sharkdp/bat/commit/de11558ad3cdb8a3fdf78c60673c4e5e73dcd5ab))
    - Code cleanup ([`fcfc7c4`](https://github.com//sharkdp/bat/commit/fcfc7c465b76808291b47eb12ac0789c5695d4cf))
    - Update help text ([`70ffd60`](https://github.com//sharkdp/bat/commit/70ffd60b207774a55d74523ae6c206bbd8c810e9))
    - Better error handling ([`36d9236`](https://github.com//sharkdp/bat/commit/36d92361be4261348b76bd5b5a349654f9cd52f2))
    - Print filename ([`6b507d1`](https://github.com//sharkdp/bat/commit/6b507d1e357bf538e9307634cad2b9a4e846460b))
    - Use default for highlighting theme ([`ab10a20`](https://github.com//sharkdp/bat/commit/ab10a20c1fcc0c81bd0f1f4d07ee41d32dc1310d))
    - Add Git support, closes #8 ([`a979608`](https://github.com//sharkdp/bat/commit/a979608f37241fbc41c42fe9326d397658b92ffe))
    - Add README ([`e6f823c`](https://github.com//sharkdp/bat/commit/e6f823cd880d8b134ae33f49146f89b8a4223d85))
    - Initial commit ([`8f5a80e`](https://github.com//sharkdp/bat/commit/8f5a80ea07e0dc98792c259b03d1d7f7678f87fa))
</details>

