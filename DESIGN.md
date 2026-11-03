# Interface design

The requested reference is [Claude](https://claude.com/) and [Anthropic](https://www.anthropic.com/). Their warm neutral palette, clear type hierarchy, and generous spacing inform this interface. The layout and component choices below are this project's interpretation for a data utility.

The public [Claude stylesheet](https://claude.com/_next/static/chunks/98bcbf1381ffeee3.css) inspected on November 2, 2026 defines the following color tokens. Its asset URL may change on future deployments.

| Role here | Color | Reference token |
| --- | --- | --- |
| Page | `#faf9f5` | `gray-050` |
| Navigation and subtle fills | `#f0eee6` | `gray-150` |
| Rules and control boundaries | `#dedcd1` | `gray-250` |
| Main text | `#141413` | `gray-950` |
| Muted text | `#73726c` | `gray-550` |
| Primary action | `#d97757` | `clay` |
| Action hover | `#c6613f` | `clay-hover` |

The near-white table surface and darker accent text are local adaptations for readable controls. Georgia supplies a system serif heading without a font download; it approximates the reference's editorial character and is not its proprietary typeface.

## Principles

- Let the dataset and its controls carry the page. Use concrete labels such as “Import data” and “Nested groups”; omit motivational headlines, decorative badges, and redundant status pills.
- Use one clay accent for the main import action. Use neutral ink for active navigation and selected tabs. Error color communicates an actual error.
- Keep a compact import column beside the data workspace. Separate sections with space and a few quiet rules. Reserve borders for editable controls and the data table.
- Present counts as a plain row of values, without repeated card frames, colored panels, or oversized numerals.
- Use a serif page title and a quiet system sans serif for controls, tabular data, and supporting text. Keep body and control text legible at ordinary browser zoom.
- Use natural, explicit controls: file selection, format options, sheet selection, import, search, hierarchy ordering, and export. Show format-specific controls only when relevant.
- At narrow widths, turn navigation into a horizontal row and stack import above exploration. Keep wide tables horizontally scrollable within their own region.
- Retain visible keyboard focus, meaningful error messages, and reduced-motion support. Load no external fonts or decorative assets.

These are practical guidelines for future changes, not a claim of affiliation with Anthropic.
