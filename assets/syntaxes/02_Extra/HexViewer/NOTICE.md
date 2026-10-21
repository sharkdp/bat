The Hex Viewer syntax is adapted from
[facelessuser/HexViewer](https://github.com/facelessuser/HexViewer/blob/21b29f1882cc31e861f8898be41c719ae334256f/HexViewer.sublime-syntax)
at commit `21b29f1882cc31e861f8898be41c719ae334256f`, under the MIT license in LICENSE.md.

The adaptation adds xxd and canonical hexdump layouts, 64-bit offsets, standard
numeric scopes for byte colors, and line-local error recovery. The upstream
HexViewer format and its `.hxv` extension remain supported. The `.xxd` and
`.hexdump` extensions select the same syntax; `.hex` is reserved for Intel HEX.
