---
name: Scholastic Minimalist
colors:
  surface: '#fff8f2'
  surface-dim: '#e0d9cf'
  surface-bright: '#fff8f2'
  surface-container-lowest: '#ffffff'
  surface-container-low: '#fbf2e8'
  surface-container: '#f5ede3'
  surface-container-high: '#efe7dd'
  surface-container-highest: '#e9e1d8'
  on-surface: '#1e1b15'
  on-surface-variant: '#554241'
  inverse-surface: '#343029'
  inverse-on-surface: '#f8f0e6'
  outline: '#887270'
  outline-variant: '#dbc0be'
  surface-tint: '#9d413e'
  primary: '#50070b'
  on-primary: '#ffffff'
  primary-container: '#6e1e1e'
  on-primary-container: '#f4857f'
  inverse-primary: '#ffb3ae'
  secondary: '#765756'
  on-secondary: '#ffffff'
  secondary-container: '#ffd6d5'
  on-secondary-container: '#7a5b5a'
  tertiary: '#34211c'
  on-tertiary: '#ffffff'
  tertiary-container: '#4c3631'
  on-tertiary-container: '#be9f98'
  error: '#ba1a1a'
  on-error: '#ffffff'
  error-container: '#ffdad6'
  on-error-container: '#93000a'
  primary-fixed: '#ffdad7'
  primary-fixed-dim: '#ffb3ae'
  on-primary-fixed: '#410005'
  on-primary-fixed-variant: '#7e2a29'
  secondary-fixed: '#ffdad9'
  secondary-fixed-dim: '#e5bdbc'
  on-secondary-fixed: '#2c1515'
  on-secondary-fixed-variant: '#5c3f3f'
  tertiary-fixed: '#fedbd3'
  tertiary-fixed-dim: '#e0bfb8'
  on-tertiary-fixed: '#291713'
  on-tertiary-fixed-variant: '#59413c'
  background: '#fff8f2'
  on-background: '#1e1b15'
  surface-variant: '#e9e1d8'
typography:
  display-lg:
    fontFamily: EB Garamond
    fontSize: 48px
    fontWeight: '600'
    lineHeight: 56px
    letterSpacing: -0.02em
  display-lg-mobile:
    fontFamily: EB Garamond
    fontSize: 32px
    fontWeight: '600'
    lineHeight: 40px
    letterSpacing: -0.01em
  headline-md:
    fontFamily: EB Garamond
    fontSize: 30px
    fontWeight: '500'
    lineHeight: 38px
    letterSpacing: -0.01em
  body-lg:
    fontFamily: EB Garamond
    fontSize: 20px
    fontWeight: '400'
    lineHeight: 32px
  body-md:
    fontFamily: EB Garamond
    fontSize: 17px
    fontWeight: '400'
    lineHeight: 28px
  label-caps:
    fontFamily: EB Garamond
    fontSize: 14px
    fontWeight: '600'
    lineHeight: 20px
    letterSpacing: 0.1em
  caption:
    fontFamily: EB Garamond
    fontSize: 14px
    fontWeight: '400'
    lineHeight: 20px
rounded:
  sm: 0.25rem
  DEFAULT: 0.5rem
  md: 0.75rem
  lg: 1rem
  xl: 1.5rem
  full: 9999px
spacing:
  unit: 8px
  margin-page: 64px
  margin-mobile: 24px
  gutter: 32px
  section-gap: 80px
  hairline: 1px
---

## Brand & Style
The design system embodies a "Light Academia" aesthetic fused with "Japandi" minimalism, evoking the tactile sensation of a fountain pen gliding over heavy, high-quality parchment. It is designed for a prestigious university finance course where authority meets serenity.

The personality is intellectual, disciplined, and timeless. The user interface prioritizes clarity and the written word, moving away from hyper-modern trends while introducing subtle, softened edges for a more approachable scholarly feel. The emotional response should be one of focused study and quiet confidence. Visual structure is provided primarily through negative space and delicate hairline rules.

## Colors
The palette is grounded in natural, historical pigments.
- **Surface (Parchment):** #F5EDE3 serves as the universal background, providing a warm, non-glare surface for long-form reading.
- **Headings (Ink):** #2A1414 is used for primary headings and high-contrast titles, offering an almost-black depth.
- **Body (Sepia):** #4A342F provides a softer contrast for body text, reducing eye strain while maintaining a scholarly tone.
- **Accent (Oxblood):** #6E1E1E is the primary functional color. It is reserved for hairline dividers, interactive links, primary buttons, and critical emphasis.

Do not use pure black (#000000) or pure white (#FFFFFF) anywhere in the design system.

## Typography
This design system utilizes an all-serif approach to maintain a consistent scholarly atmosphere. **EB Garamond** is the sole typeface family, utilized for its classical proportions and exceptional legibility.

- **Headings:** Use medium weights with slight negative letter-spacing to create a "printed" feel.
- **Body Text:** Use regular weight with generous line heights (1.6x) to facilitate deep focus and reading.
- **Labels:** Small caps or uppercase with increased tracking should be used for metadata and utility labels to differentiate them from prose without switching typefaces.
- **Emphasis:** Use italics sparingly for emphasis or captions, as per traditional typesetting standards.

## Layout & Spacing
The layout follows a strict, left-aligned grid inspired by editorial manuscripts.

- **Grid:** A 12-column grid for desktop, but content typically occupies a centered 8-column "reading well" to prevent overly long line lengths.
- **Alignment:** Everything is strictly left-aligned. Avoid center alignment even for headlines.
- **Dividers:** Use 1px hairline rules in Oxblood (#6E1E1E) to separate logical sections. These rules should extend across the full width of the container.
- **Whitespace:** Emphasize generous vertical margins between sections to allow the content to "breathe." Avoid boxing content; use space and lines as the only organizational tools.

## Elevation & Depth
This design system is primarily two-dimensional, emphasizing the "ink on paper" aesthetic.
- **No Shadows:** Depth is never communicated through drop shadows or blurs.
- **Subtle Layering:** While the design is flat, the use of surface-container tiers can be used to separate supplementary content from the main "page."
- **Tonal Depth:** If a modal or overlay is required, it should use a solid Parchment (#F5EDE3) background with a 1px Oxblood hairline border, maintaining the flat, paper-like aesthetic.

## Shapes
The UI moves away from sharp corners to a "Rounded" profile, adding a modern touch to the classical aesthetic.
- **Radius:** Standard elements use a 0.5rem (8px) radius. Larger containers like `rounded-lg` use 1rem (16px).
- **Forms:** Buttons, input fields (if boxed), and selection indicators utilize these rounded corners to soften the architectural discipline of the academic subject matter, making the interface feel more contemporary and tactile.

## Components
Components are text-heavy but benefit from the updated shape language to feel like modern digital tools.

- **Buttons:** Primary buttons are solid Oxblood (#6E1E1E) with 0.5rem rounded corners and Parchment (#F5EDE3) text. Secondary buttons are text-only with a 1px hairline underline that appears on hover.
- **Input Fields:** While the system supports 1px hairline underlines, boxed inputs with a 0.5rem radius and a thin Oxblood border are preferred for modern clarity. Labels sit above the field in Small Caps.
- **Lists:** Unordered lists use a small Oxblood square or a simple dash. Ordered lists use traditional Roman numerals for a scholarly touch.
- **Checkboxes/Radios:** Small, rounded-corner squares (checkboxes) or circles (radios). When selected, they are filled with a solid Oxblood color.
- **Data Tables:** No vertical rules. Use only horizontal hairline rules in Oxblood to separate headers and rows. Row backgrounds do not stripe; use whitespace for legibility.
- **Navigation:** Simple text links separated by a vertical pipe `|` or generous horizontal spacing.
