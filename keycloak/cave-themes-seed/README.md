# Cave Themes

Custom color themes for [Cave](/).

## How to use

1. Fork this repo to your own account
2. Add or edit `.toml` files — each file is a theme
3. Push — Cave automatically syncs your themes
4. Go to **Settings → Theme** to activate

## Theme format

Each `.toml` file defines CSS variables. See `midnight-blue.toml` for a documented example.

### Color keys

| Key | Description |
|-----|-------------|
| `bg` | Page background |
| `bg-warm` | Nav bar background |
| `surface` | Card/panel background |
| `surface-hover` | Hover state for surfaces |
| `border` | Primary border color |
| `border-subtle` | Subtle border color |
| `text` | Primary text |
| `text-secondary` | Secondary text |
| `text-muted` | Muted/disabled text |
| `accent` | Primary accent (brand color) |
| `accent-dim` | Dimmed accent (buttons) |
| `accent-bg` | Accent background tint |
| `link` | Link color |
| `link-hover` | Link hover color |
| `green` | Success/addition color |
| `green-bright` | Bright green |
| `green-bg` | Green background tint |
| `red` | Error/deletion color |
| `red-bg` | Red background tint |
| `yellow` | Warning color |
| `yellow-bg` | Yellow background tint |
| `blue` | Info/diff hunk color |
| `blue-bg` | Blue background tint |

### Font keys

| Key | Description |
|-----|-------------|
| `font-url` | Google Fonts (or other) CSS URL to @import |
| `font-mono` | Monospace font family (code, headings, nav) |
| `font-body` | Body text font family |

All colors must be valid CSS: `#hex`, `rgb()`, `rgba()`, or named colors.
Invalid values will be flagged as issues on your repo.
