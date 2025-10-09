[38;2;117;113;94m#[0m
[38;2;117;113;94m# spec file for package bat[0m
[38;2;117;113;94m#[0m
[38;2;117;113;94m# Copyright (c) 2025 SUSE LLC[0m
[38;2;117;113;94m#[0m
[38;2;117;113;94m# All modifications and additions to the file contributed by third parties[0m
[38;2;117;113;94m# remain the property of their copyright owners, unless otherwise agreed[0m
[38;2;117;113;94m# upon. The license for this file, and modifications and additions to the[0m
[38;2;117;113;94m# file, is the same license as for the pristine package itself (unless the[0m
[38;2;117;113;94m# license for the pristine package is not an Open Source License, in which[0m
[38;2;117;113;94m# case the license is the MIT License). An "Open Source License" is a[0m
[38;2;117;113;94m# license that conforms to the Open Source Definition (Version 1.9)[0m
[38;2;117;113;94m# published by the Open Source Initiative.[0m

[38;2;117;113;94m# Please submit bugfixes or comments via https://bugs.opensuse.org/[0m
[38;2;117;113;94m#[0m


[38;2;249;38;114mName:[0m[38;2;248;248;242m           bat[0m
[38;2;249;38;114mVersion:[0m[38;2;248;248;242m        0.25.0[0m
[38;2;249;38;114mRelease:[0m[38;2;248;248;242m        0[0m
[38;2;249;38;114mSummary:[0m[38;2;248;248;242m        A cat(1) clone with syntax highlighting and Git integration[0m
[38;2;249;38;114mLicense:[0m[38;2;248;248;242m        Apache-2.0 OR MIT[0m
[38;2;249;38;114mGroup:[0m[38;2;248;248;242m          Productivity/Text/Utilities[0m
[38;2;249;38;114mURL:[0m[38;2;248;248;242m            https://github.com/sharkdp/bat[0m
[38;2;249;38;114mSource0:[0m[38;2;248;248;242m        [0m[38;2;255;255;255m%{name}[0m[38;2;248;248;242m-[0m[38;2;255;255;255m%{version}[0m[38;2;248;248;242m.tar.xz[0m
[38;2;249;38;114mSource1:[0m[38;2;248;248;242m        vendor.tar.xz[0m
[38;2;249;38;114mBuildRequires:[0m[38;2;248;248;242m  cargo-packaging[0m
[38;2;117;113;94m# weak-dep-features introduced in 1.60 and already used by libgit2-sys[0m
[38;2;117;113;94m# https://github.com/rust-lang/git2-rs/commit/d8ee105a8f3ce4d5c57cd091b67943aab86b176a[0m
[38;2;249;38;114mBuildRequires:[0m[38;2;248;248;242m  rust >= 1.60[0m
[38;2;249;38;114mExclusiveArch:[0m[38;2;248;248;242m  [0m[38;2;248;248;240m%{rust_arches}[0m

[38;2;249;38;114m%description[0m
[38;2;248;248;242mA cat(1) clone which supports syntax highlighting for a large number of[0m
[38;2;248;248;242mprogramming and markup languages. It has git integration and automatic paging.[0m

[38;2;249;38;114m%package[0m[38;2;248;248;242m bash-completion[0m
[38;2;249;38;114mSummary:[0m[38;2;248;248;242m        Bash completion for [0m[38;2;255;255;255m%{name}[0m
[38;2;249;38;114mRequires:[0m[38;2;248;248;242m       [0m[38;2;255;255;255m%{name}[0m[38;2;248;248;242m = [0m[38;2;255;255;255m%{version}[0m
[38;2;249;38;114mSupplements:[0m[38;2;248;248;242m    ([0m[38;2;255;255;255m%{name}[0m[38;2;248;248;242m and bash-completion)[0m
[38;2;249;38;114mBuildArch:[0m[38;2;248;248;242m      [0m[38;2;190;132;255mnoarch[0m

[38;2;249;38;114m%description[0m[38;2;248;248;242m bash-completion[0m
[38;2;248;248;242mBash command line completion support for [0m[38;2;255;255;255m%{name}[0m[38;2;248;248;242m.[0m

[38;2;249;38;114m%package[0m[38;2;248;248;242m fish-completion[0m
[38;2;249;38;114mSummary:[0m[38;2;248;248;242m        Fish completion for [0m[38;2;255;255;255m%{name}[0m
[38;2;249;38;114mRequires:[0m[38;2;248;248;242m       [0m[38;2;255;255;255m%{name}[0m[38;2;248;248;242m = [0m[38;2;255;255;255m%{version}[0m
[38;2;249;38;114mSupplements:[0m[38;2;248;248;242m    ([0m[38;2;255;255;255m%{name}[0m[38;2;248;248;242m and fish)[0m
[38;2;249;38;114mBuildArch:[0m[38;2;248;248;242m      [0m[38;2;190;132;255mnoarch[0m

[38;2;249;38;114m%description[0m[38;2;248;248;242m fish-completion[0m
[38;2;248;248;242mFish command line completion support for [0m[38;2;255;255;255m%{name}[0m[38;2;248;248;242m.[0m

[38;2;249;38;114m%package[0m[38;2;248;248;242m zsh-completion[0m
[38;2;249;38;114mSummary:[0m[38;2;248;248;242m        Zsh completion for [0m[38;2;255;255;255m%{name}[0m
[38;2;249;38;114mRequires:[0m[38;2;248;248;242m       [0m[38;2;255;255;255m%{name}[0m[38;2;248;248;242m = [0m[38;2;255;255;255m%{version}[0m
[38;2;249;38;114mSupplements:[0m[38;2;248;248;242m    ([0m[38;2;255;255;255m%{name}[0m[38;2;248;248;242m and zsh)[0m
[38;2;249;38;114mBuildArch:[0m[38;2;248;248;242m      [0m[38;2;190;132;255mnoarch[0m

[38;2;249;38;114m%description[0m[38;2;248;248;242m zsh-completion[0m
[38;2;248;248;242mZsh command line completion support for [0m[38;2;255;255;255m%{name}[0m[38;2;248;248;242m.[0m

[38;2;249;38;114m%prep[0m
[38;2;255;255;255m%setup[0m[38;2;248;248;242m -qa1[0m

[38;2;249;38;114m%build[0m
[38;2;255;255;255m%{cargo_build}[0m

[38;2;249;38;114m%install[0m
[38;2;248;248;242minstall -D -m 0755 target/release/[0m[38;2;255;255;255m%{name}[0m[38;2;248;248;242m [0m[38;2;255;255;255m%{buildroot}[0m[38;2;255;255;255m%{_bindir}[0m[38;2;248;248;242m/[0m[38;2;255;255;255m%{name}[0m

[38;2;248;248;242minstall -D -m 0644 $(find target/release/build -name "[0m[38;2;255;255;255m%{name}[0m[38;2;248;248;242m.1") "[0m[38;2;255;255;255m%{buildroot}[0m[38;2;248;248;242m/[0m[38;2;255;255;255m%{_mandir}[0m[38;2;248;248;242m/man1/[0m[38;2;255;255;255m%{name}[0m[38;2;248;248;242m.1"[0m

[38;2;248;248;242minstall -D -m 0644 $(find target/release/build -name "[0m[38;2;255;255;255m%{name}[0m[38;2;248;248;242m.bash") "[0m[38;2;255;255;255m%{buildroot}[0m[38;2;248;248;242m/[0m[38;2;255;255;255m%{_datadir}[0m[38;2;248;248;242m/bash-completion/completions/[0m[38;2;255;255;255m%{name}[0m[38;2;248;248;242m"[0m
[38;2;248;248;242minstall -D -m 0644 $(find target/release/build -name "[0m[38;2;255;255;255m%{name}[0m[38;2;248;248;242m.fish") "[0m[38;2;255;255;255m%{buildroot}[0m[38;2;248;248;242m/[0m[38;2;255;255;255m%{_datadir}[0m[38;2;248;248;242m/fish/vendor_completions.d/[0m[38;2;255;255;255m%{name}[0m[38;2;248;248;242m.fish"[0m
[38;2;248;248;242minstall -D -m 0644 $(find target/release/build -name "[0m[38;2;255;255;255m%{name}[0m[38;2;248;248;242m.zsh")  "[0m[38;2;255;255;255m%{buildroot}[0m[38;2;248;248;242m/[0m[38;2;255;255;255m%{_datadir}[0m[38;2;248;248;242m/zsh/site-functions/_[0m[38;2;255;255;255m%{name}[0m[38;2;248;248;242m"[0m

[38;2;249;38;114m%if[0m[38;2;248;248;242m [0m[38;2;255;255;255m%{with check}[0m
[38;2;249;38;114m%check[0m
[38;2;255;255;255m%{cargo_test}[0m
[38;2;249;38;114m%endif[0m

[38;2;249;38;114m%files[0m
[38;2;255;255;255m%doc[0m[38;2;248;248;242m README.md CONTRIBUTING.md CHANGELOG.md[0m
[38;2;255;255;255m%license[0m[38;2;248;248;242m LICENSE-MIT LICENSE-APACHE[0m
[38;2;255;255;255m%{_bindir}[0m[38;2;248;248;242m/[0m[38;2;255;255;255m%{name}[0m
[38;2;255;255;255m%{_mandir}[0m[38;2;248;248;242m/man1/[0m[38;2;255;255;255m%{name}[0m[38;2;248;248;242m.1[0m[38;2;255;255;255m%{ext_man}[0m

[38;2;249;38;114m%files[0m[38;2;248;248;242m bash-completion[0m
[38;2;255;255;255m%{_datadir}[0m[38;2;248;248;242m/bash-completion/completions/[0m[38;2;255;255;255m%{name}[0m

[38;2;249;38;114m%files[0m[38;2;248;248;242m fish-completion[0m
[38;2;255;255;255m%dir[0m[38;2;248;248;242m [0m[38;2;255;255;255m%{_datadir}[0m[38;2;248;248;242m/fish[0m
[38;2;255;255;255m%dir[0m[38;2;248;248;242m [0m[38;2;255;255;255m%{_datadir}[0m[38;2;248;248;242m/fish/vendor_completions.d[0m
[38;2;255;255;255m%{_datadir}[0m[38;2;248;248;242m/fish/vendor_completions.d/[0m[38;2;255;255;255m%{name}[0m[38;2;248;248;242m.fish[0m

[38;2;249;38;114m%files[0m[38;2;248;248;242m zsh-completion[0m
[38;2;255;255;255m%dir[0m[38;2;248;248;242m [0m[38;2;255;255;255m%{_datadir}[0m[38;2;248;248;242m/zsh[0m
[38;2;255;255;255m%dir[0m[38;2;248;248;242m [0m[38;2;255;255;255m%{_datadir}[0m[38;2;248;248;242m/zsh/site-functions[0m
[38;2;255;255;255m%{_datadir}[0m[38;2;248;248;242m/zsh/site-functions/_[0m[38;2;255;255;255m%{name}[0m

[38;2;249;38;114m%changelog[0m
