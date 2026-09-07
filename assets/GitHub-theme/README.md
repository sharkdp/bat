`../patches/GitHub-modern-colors.patch` refreshes the existing `GitHub` theme with
the TextMate token rules and light palette from the official
[GitHub VS Code theme](https://github.com/primer/github-vscode-theme/tree/cd78e5e4e7bcf132a6f428ae0f32264bb1b729cf),
version 6.3.5. The palette is its pinned
[Primer Primitives 7.10.0](https://github.com/primer/primitives/tree/v7.10.0).
Both MIT licenses are included in this directory for the asset acknowledgements.

The source `src/theme.js` tokenColors array is evaluated for the light variant
using the palette in `data/colors/themes/light.ts` and the foreground override in
`src/colors.js`. Its 49 rules are converted to TextMate settings. Scope arrays are
joined with commas, and rules are reversed so syntect's first-match priority for
equal selectors preserves VS Code's last-match priority. Strikethrough is omitted
because syntect's FontStyle does not support it; bold, italic and underline remain.

The foreground is `#1f2328`, and `gutterForeground` is `#8c959f`, matching the
official light theme's `editorLineNumber.foreground` (`scale.gray[4]`). This also
colors bat's grid. The existing white background and bat's yellow line-highlight
and selection settings are retained. Rendering still depends on the Sublime
syntax scopes and the terminal's color capabilities.

The patch applies to the pinned `github-sublime-theme` revision
`59e525f638237dca56f728d7e5d38b9bb41c56d4`. It is applied and reversed by the usual
asset build, and no binary cache or additional theme name is introduced. When
updating the upstream sources, review the conversion and regenerate the four
GitHub_theme fixtures, including their number/grid decorations.
