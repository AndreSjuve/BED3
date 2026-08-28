# BED3 Website — Visual Design Style

## Context

This is the first of three sub-projects for the new BED3 course website
(capital budgeting & finance, NHH), per `IDEA_SKETCH.md`. It covers only the
visual design language — the "vibe" — used to brief Google Stitch. Information
architecture (what pages/content exist) and interactive learning features
(quizzes, problem sets) are separate sub-projects, brainstormed later.

Kept from the current site (https://andresjuve.github.io/BED3/): the
"Kursinnhold" content and the landing-page info/contact cards. Everything
else — including this visual design — is a clean redesign.

## Direction

Reference styles: **Light Academia** × **Luxury Typography** × **Neoclassical**,
tempered by **Japandi** restraint.

Target feeling: prestigious, reflecting the weight and history of finance as
a field — writing with a fountain pen on fine paper — without becoming heavy
or intimidating. Trustworthy and scholarly, not corporate; refined, not
ornate.

## Design tokens

**Color** — ivory/parchment base, oxblood accent, near-black text. Leather-bound
book / classic academia feel.

| Token | Hex | Use |
|---|---|---|
| `background` | `#F5EDE3` | page background |
| `text` | `#2A1414` | headings, primary text |
| `text-muted` | `#4A342F` | body copy |
| `accent` | `#6E1E1E` | links, buttons, eyebrow labels, rule lines |

**Typography** — one quiet type family pairing, no jarring contrast between
elements.

- Headings: **Cormorant Garamond** (weight 500–600) — delicate old-style serif
- Body / UI text: **EB Garamond** (weight 400–500) — same serif family, sized
  down for legibility
- No sans-serif, no display/grotesk faces anywhere

**Layout** — "Balanced" density:

- Left-aligned text (not centered/symmetric)
- Generous whitespace — the layout should breathe
- Thin single rule lines (1px, low-opacity accent color) as the *only*
  structural ornament — e.g. under an eyebrow label, between sections
- No borders, no boxed/framed cards, no decorative dividers beyond the plain
  rule line
- No drop shadows or heavy depth effects

**Imagery** — no decorative/stock photography and no standalone atmosphere
imagery (e.g. a hero photo). Type and whitespace carry the overall design.
Three exceptions, all functional rather than decorative:

- **Instructor photos** — the current site's instructor photos on the
  contact/about section carry over. Treat editorially: small, muted, cropped
  consistently — not glossy headshots.
- **Lecture index thumbnails** — the video lecture library needs some
  scannable visual unit per lecture. Prefer a typographic index card
  (topic, number, duration) over a video-still grid, so it stays in the
  established language rather than introducing photography. Exact treatment
  is decided in the information-architecture sub-project.
- **Finance diagrams** — NPV timelines, cash-flow diagrams, discount curves,
  and similar course content. Render as spare linework in the oxblood accent
  color (like a diagram in a well-typeset textbook), not full-color charts
  or stock illustration.

Still excluded: campus/stock photography, "students studying" imagery, and
any purely decorative illustration or hero photo. (Functional icons for
interactive features are out of scope for this document.)

## Stitch prompt

The block below is ready to paste into Google Stitch (https://stitch.withgoogle.com/)
as the style brief.

```
Design a website for an undergraduate university course in capital budgeting
and finance. The tone is prestigious and scholarly — it should feel like
writing with a fountain pen on fine paper, evoking the weight and history of
the field of finance, while staying warm and approachable rather than heavy
or intimidating. Style references: light academia, luxury typography,
neoclassical restraint, and Japandi minimalism.

Color palette: warm ivory/parchment background (#F5EDE3), near-black text
(#2A1414) for headings, a softer dark brown (#4A342F) for body copy, and a
single deep oxblood accent (#6E1E1E) used for links, buttons, small
uppercase eyebrow labels, and thin rule lines. No other accent colors.

Typography: an elegant old-style serif for headings (in the character of
Cormorant Garamond) and a classic, highly legible serif for body text and UI
elements (in the character of EB Garamond). Use only this one serif family
pairing throughout — no sans-serif or display/grotesk fonts anywhere.

Layout: left-aligned (not centered or symmetric), generous whitespace, calm
and uncluttered. The only structural ornament allowed is a thin single rule
line (hairline, low-opacity oxblood) used sparingly — for example under a
small uppercase eyebrow label, or between sections. No boxed/bordered cards,
no drop shadows, no decorative dividers or flourishes beyond that single
rule line.

Imagery: no decorative or stock photography, and no standalone atmosphere/
hero imagery — typography, color, and whitespace carry the overall design.
The only imagery allowed is functional: small, editorially-treated instructor
portrait photos (muted, consistently cropped, not glossy headshots), and
spare finance diagrams (NPV timelines, cash-flow diagrams, discount curves)
rendered as fine linework in the single oxblood accent color, in the style
of a diagram in a well-typeset textbook rather than a full-color chart or
illustration.
```

## Out of scope

- Page structure / navigation / information architecture (next sub-project)
- Quiz, problem-set, and other interactive features (separate sub-project)
- Functional iconography for interactive features
- Actual page build/implementation — this document only feeds the Stitch
  design-exploration step described in `IDEA_SKETCH.md`
