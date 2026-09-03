# Kapitalens kretsløp — interactive course-map on forsiden

**Date:** 2026-09-03
**Status:** Approved design, pending implementation plan

## Problem

BED3 spans capital budgeting, investments, and corporate finance, and students
struggle to see how the topics connect. The connective narrative exists in
prose (bakteppet: households save → the capital market prices capital → firms
invest it in the projects that create the most value), but it is locked in
linear text. The landing page should carry an engaging, explorable
visualization of that structure, built to the craft level of good commercial
product sites but in the site's own engraved-editorial visual language.

## Decisions made during brainstorming

1. **Organizing structure:** the capital-flow story (velstandshjulet) from
   bakteppet, not a concept graph or a central-question map.
2. **Interaction:** explore-on-hover/tap in a self-contained panel with
   ambient flow animation. No scrollytelling, no guided-tour mode.
3. **Placement:** a full-width section on `site/index.qmd` after
   «Der du begynner», before «Det du finner her».
4. **Coverage:** the full course. Async topics link to their module pages;
   plenary topics (porteføljeteori, KVM, kapitalstruktur, bærekraft) get the
   same explanation card but link to the relevant bakteppe section or
   timeplanen, marked «Undervises i plenum».

## Concept and content model

An interactive rendering of the velstandshjul narrative. Three stations on a
horizontal flow, return loop closing the circuit:

| Station | Role | Topics anchored there |
|---|---|---|
| Husholdningene | sparer | porteføljeteori, bærekraftige investeringer |
| Kapitalmarkedet | setter prisen | kapitalverdimodellen, rentemarkedet, valuta |
| Bedriftene | investerer | investeringsprosjekter, skatt og lån, inflasjon, risikovurderinger, låne- og betalingsalternativer, kapitalstruktur |

- **Opsjoner og terminer** sit on the paths between stations, labeled as the
  plumbing that moves risk.
- Return flow along the bottom: **avkastning** flowing from bedriftene back
  to husholdningene.

Each topic node carries:

- title
- a 2–3 sentence card: what question the topic answers and why it sits where
  it sits in the flow
- a link: module page plus duration for async topics; timeplanen or the
  relevant bakteppe section for plenary topics, with the label
  «Undervises i plenum»

All card content lives in one data structure (a plain JS object) so text can
be edited without touching layout code. Card prose follows the site's prose
conventions: no em dashes, no decorative bold, the established teaching
voice, mixed case only (no all-caps).

## Visual and motion

- Inline SVG in the site's design language: oxblood strokes on paper
  (`--oxblood` on `--paper`), Garamond labels, hairline rules from the
  existing token set in `design/v2/styles.css` / `theme/bed3.scss`.
- The life comes from motion, not color: a slow continuous flow animation
  along the capital paths (small dashes/particles drifting between stations,
  capital literally circulating). Calm enough to be ambient.
- Hover or tap on a topic highlights it and its paths and gently dims the
  rest. The explanation card renders in a fixed panel beside/below the
  diagram, never a floating tooltip, so nothing jumps and mobile tap behaves
  identically to hover.
- `prefers-reduced-motion` stops the ambient flow animation.
- On narrow screens the diagram reflows to a vertical stack; the flow runs
  top to bottom.

## Technical shape

No frameworks, no d3. The site ships zero JS libraries today and eleven
nodes do not warrant a dependency. One self-contained component:

- Raw HTML/SVG block in `site/index.qmd` (or a Quarto include) for the
  section.
- Styles appended to `theme/bed3.scss`, using existing tokens only.
- One small vanilla JS file in `site/assets/` holding the topic data and the
  highlight/card logic.

The Quarto build stays untouched and the component is testable in isolation.

## Placement and copy

New full-width section on forsiden after «Der du begynner», headed
**«Slik henger kurset sammen»**, with one intro sentence and a closing line
linking bakteppet as the written-out version of the same story. Existing
sections stay as they are.

## Error handling and degradation

- Without JS the SVG still renders as a complete, legible static diagram;
  topic nodes are plain anchors to their targets.
- Reduced motion: static diagram, interaction still works.
- Unknown/missing topic data: the node renders without a card rather than
  breaking the panel.

## Verification

- Build the site with Quarto.
- Drive the built page with Playwright: hover and tap states, card links
  resolve, reduced-motion honored.
- Screenshot review at desktop and mobile widths against the design system
  before calling it done.
