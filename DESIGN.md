---
name: Ink on Parchment
source: design/v2/styles.css
colors:
  paper: '#F5EDE3'
  ink: '#2A1414'
  sepia: '#4A342F'
  muted: '#6B5750'
  oxblood: '#6E1E1E'
  rule: '#E1CEC5'
  rule-strong: '#C9A9A1'
  wash: '#EFE3DB'
  selection: '#E5D0C8'
  visited: '#5A2430'
  print-ink: '#000'
  print-paper: '#fff'
typography:
  display:
    fontFamily: Cormorant Garamond
    fontSize: clamp(40px, 1.55rem + 3.8vw, 68px)
    fontWeight: '400'
    lineHeight: 1.04
    letterSpacing: -0.015em
  h2:
    fontFamily: Cormorant Garamond
    fontSize: clamp(28px, 1.45rem + 1.2vw, 38px)
    fontWeight: '500'
    lineHeight: 1.12
    letterSpacing: -0.01em
  h3:
    fontFamily: Cormorant Garamond
    fontSize: clamp(21px, 1.2rem + 0.4vw, 24px)
    fontWeight: '600'
    lineHeight: 1.12
  wordmark:
    fontFamily: Cormorant Garamond
    fontSize: 24px
    fontWeight: '500'
  lead:
    fontFamily: EB Garamond
    fontSize: clamp(20px, 1.1rem + 0.6vw, 24px)
    fontWeight: '400'
    lineHeight: 1.55
  body:
    fontFamily: EB Garamond
    fontSize: clamp(18px, 1.05rem + 0.3vw, 20px)
    fontWeight: '400'
    lineHeight: 1.65
  small:
    fontFamily: EB Garamond
    fontSize: 15px
    fontWeight: '400'
  label:
    fontFamily: EB Garamond
    fontSize: 13px
    fontWeight: '500'
  asterism:
    fontFamily: EB Garamond
    fontSize: 17.6px
rounded:
  DEFAULT: 0
spacing:
  unit: 8px
  page-margin: clamp(24px, 5vw, 96px)
  section-gap: clamp(64px, 7vw, 136px)
  measure: 68ch
  measure-wide: 92ch
  shell: 78rem
  hairline: 1px
---

## Hvor systemet kommer fra

This system is documented from `design/v2/`, not from intentions. It was derived
from the two Google Stitch v1 screens that worked — the formula sheet and the
schedule — with their structural defects corrected: the type scale is real, the
accent is reserved again, the register is mixed case, and mobile is designed
rather than inherited.

The world: ink on parchment. A well-set textbook, not a website with a serif.

## Farger

Four colours carry the design; three tints of the oxblood do the structural work.

- **Paper `#F5EDE3`** — the one background on every page. There is no second
  surface, no card colour, no elevation tier.
- **Ink `#2A1414`** — headings and anything that must be read first. 14.98:1.
- **Sepia `#4A342F`** — body copy. 9.93:1.
- **Muted `#6B5750`** — metadata, captions, marginalia. 5.71:1, so it still
  clears AA at body size.
- **Oxblood `#6E1E1E`** — links, buttons, list marks, rules, and the marked row.
  Nothing else. If oxblood colours a heading, the accent has stopped meaning
  anything; that is the failure mode this system was rewritten to fix.

Tints, all oxblood over paper: `rule` (15%) for hairlines, `rule-strong` (32%)
for the rule under a table head and for link underlines, `wash` (6%) for the one
permitted fill.

No pure black and no pure white on screen. The print stylesheet is the single
exception, and it is deliberate.

## Typografi

Two faces from one family tradition, as the visual-design spec pinned them:

- **Cormorant Garamond** — display, h2, h3, wordmark. Weights 400–600. Its high
  contrast is the "fountain pen" half of the brief, and it only earns its
  delicacy above about 20px.
- **EB Garamond** — everything else. Weights 400–500. Sturdier, and legible at
  reading sizes where Cormorant thins out.

Rules that matter more than the sizes:

- **Mixed case.** All-caps letterspacing was tried in v1 and rejected: it reads
  as a brass plaque, not a fountain pen. There is no `text-transform: uppercase`
  anywhere in this system. Emphasis comes from size, weight, and italics.
- **No kickers.** A small label above a heading is not part of this system.
  Section identity comes from the heading itself.
- **Figures.** Prose uses old-style figures (`oldstyle-nums proportional-nums`)
  set on `body`. Anything a reader compares against another number — tables,
  glossaries, durations, times, key figures — opts into
  `lining-nums tabular-nums` via `.tnum`, `table`, or `.figures`. On a finance
  course this is functional, not decorative.
- **Italics** carry marginalia, captions, table column heads, and the quiz's
  answer marks, in the tradition of a printed textbook's side notes.
- **Measure** is 68ch for prose, 92ch for tables and formula grids.

## Rom og struktur

- Space separates; rules do not. Vertical rules are not in the system — v1 drew
  a 1000px oxblood line through empty space, and that is what a vertical rule
  will always eventually do.
- One horizontal hairline is the entire ornament budget, plus the asterism
  (`⁂`) as a section break on the formula sheet, which is a printer's mark with
  a job rather than a decoration.
- More space above a heading than below it: the heading belongs to what follows.
- `section-gap` scales 64→136px. Generous is the point; v1's 80px was not.
- **Corner radius is 0.** The Stitch export argued for rounded corners as a route
  to "approachable". Warmth here comes from the paper tone, the leading, and the
  italics instead.

## Flater vi ikke tegnet

Themed from the palette, because browser defaults belong to no design system:
text selection, caret, scrollbar track and thumb, link underline colour and
offset, and the focus ring — 2px solid oxblood at 3px offset, on every
interactive element. Focus was invisible in v1 and is now the most reliably
drawn state in the system.

## Komponenter

- **Cards: none.** No boxed, bordered or filled containers. The one exception on
  the whole site is the current week in the schedule: `wash` fill plus a 2px
  oxblood spine on the row's first cell. It earns the exception by marking one
  row on one page, and the table is pulled left by its own padding so the
  exception costs no alignment.
- **Tables** — horizontal hairlines only, no vertical rules, no zebra striping,
  italic column heads. Below 46rem each row becomes a block, with explicit ARIA
  roles in the markup so `display: block` does not strip the table semantics.
- **Video index** — typographic rows (number, title, subtitle, duration), never
  thumbnail cards. Hover tints the row with `wash`.
- **Math** — KaTeX, display mode. A formula wider than its column takes the full
  measure (`.formula--wide`) rather than scrolling sideways; a clipped equation
  is a wrong equation.
- **Quiz** — states are drawn, not assumed: correct (wash fill, oxblood key,
  italic "riktig"), wrong (muted, italic "ditt svar"), disabled after answering,
  with the explanation revealed either way. Score announced via `aria-live`.
- **Problem sets** — solutions in `<details>`, closed by default, so reading the
  answer is a deliberate act.
- **Portraits** — 4:5 crop, permanently muted (grayscale + slight sepia), never
  a circle and never a grayscale-to-colour hover trick.
- **Icons** — authored inline SVG on a single 1.25 stroke weight. No icon font.
- **Navigation** — text links, no pipes or separators. Below 46rem a `<details>`
  disclosure whose summary names the current page. Every page carries the same
  five items.

## Bilder

No decorative or stock photography, no hero image. Type, colour and whitespace
carry the design. Functional imagery only: instructor portraits as above,
lecture video embeds, and finance diagrams as spare oxblood linework in the
manner of a textbook figure.

## Ting som ikke skal skje

Each of these was a real defect in a prior round, not a hypothetical:

- A heading in oxblood.
- `text-transform: uppercase` anywhere.
- A vertical rule, or a rule that outlives its content.
- A second background colour, or a per-page config that drifts from this one.
- A page whose type scale silently fails to load.
- `px-page-margin` applied at phone widths.
- A navigation that exists only above the mobile breakpoint.
- A table marked up as divs.
- An equation set as running text with a slash for a fraction bar.
- Old-style figures inside a table of cash flows.
