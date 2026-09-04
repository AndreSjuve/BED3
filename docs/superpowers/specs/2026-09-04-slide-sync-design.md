# Slide sync — pulling course-content decks into the website

**Date:** 2026-09-04
**Status:** Implemented and verified in browser (slide sync + PDF download +
fullscreen viewer, all 9 decks)

**Addendum (2026-09-04):** Also added a "Vis i fullskjerm" button next to the
PDF download link, calling the native Fullscreen API on the `.slides` iframe.
Wired through one global include (`_includes/slides-fullscreen.html`, added
to `include-after-body`), matching the existing quiz/video script pattern —
self-guarding, no per-page opt-in needed. One nuance worth recording: Pandoc
renders a raw `<button>` tag as a block-level sibling of the preceding
`.slides` div, not wrapped in the same `<p>` as an adjacent markdown-syntax
link — so the button's DOM parent is not shared with the download link, and
the script locates its target via `btn.previousElementSibling`, not
`btn.parentElement.previousElementSibling`.

## Problem

Lecture slides used to be built in this repo: LaTeX Beamer sources under
`docs/Forelesninger/*.zip`, compiled locally by `tools/build-plansjer.py` into
`site/assets/plansjer/*.pdf`, linked as a bare PDF from each chapter's
"Forelesningsplansjer" section. `BED3-course-content` (sibling repo,
`https://github.com/AndreSjuve/BED3-course-content.git`) now owns that
source — decks are migrated to Quarto/RevealJS — and produces both an HTML
build (student-facing viewer) and a PDF (Canvas download) per deck via
`python scripts/build.py`. The website needs a repeatable way to pull those
two artifacts in and publish them, without ever holding or hand-editing
lecture source.

This is Phase 6 ("connect publishing") of `BED3/docs/migration-strategy.md`.

## Source of truth

`BED3-course-content/lectures/decks.yml` is the manifest: nine decks
(F01–F05, F13–F16; F06–F12 and F17 are a co-lecturer's decks with no
editable source, permanently out of scope). Each entry names `dir`
(source folder), `published_pdf` (filename contract — already linked from
9 live site pages, must not change) and `status` (only `migrated` decks are
synced). Build outputs (`_artifacts/pdf/*.pdf`, `lectures/**/*.html`,
`lectures/**/*_files/`) are gitignored in course-content — they exist only
in a locally built working tree, never in git history. So the sync step
reads from a local sibling checkout, not from a git clone/fetch.

## Decisions

1. **Replaces the old pipeline.** `docs/Forelesninger/*.zip` and
   `tools/build-plansjer.py` are deleted; course-content's `published_pdf`
   names are identical to what's already committed, so no site link breaks.
2. **Sync script, not a build.** `tools/sync-slides.py` copies already-built
   artifacts from a local course-content checkout; it does not invoke
   quarto/R/decktape itself. It refuses to run against a dirty course-content
   working tree (the built artifacts might not match any committed source),
   `--force` overrides.
3. **Dedup the RevealJS bundle.** Each deck's `_files/libs/revealjs` is an
   identical ~5.4MB copy of the same library. The sync script copies it once
   to `site/assets/plansjer-html/_shared/libs/revealjs/` and rewrites each
   deck's HTML to reference the shared copy, instead of committing nine
   duplicate copies (~49MB) to website git history.
4. **Commit the synced artifacts**, matching the existing convention for
   `site/assets/plansjer/*.pdf` (already tracked git source, no CI/deploy
   pipeline exists — publishing is a manual `quarto render`). `content-lock.yml`
   at the repo root is the audit trail: source repo, pinned commit SHA, sync
   timestamp, and per-deck record (id, title, filenames, byte sizes).
5. **Viewer is parchment register, not night.** `.video` (used for lecture
   video embeds) is night-register CSS (dark background, night-bleed
   variables) — slides are a reference/reading artifact, not one of the
   night-register elements (kretsløp, quiz, video, Buddy page). A new
   `.slides` class in `site/theme/bed3.scss` is a plain bordered box on the
   paper background, `aspect-ratio: 16/9` (matches the deck's 1600×900
   canvas), no night-bleed machinery.
6. **No shortcode/abstraction.** Each of the 9 chapter `index.qmd` files
   gets the iframe + download markup written out directly, matching how
   video/quiz markup is already repeated per page site-wide
   (`docs/video-page-conventions.md`).

## Layout

```
site/assets/plansjer/<published_pdf>                      # unchanged path
site/assets/plansjer-html/<dir>/<dir>.html                 # new
site/assets/plansjer-html/<dir>/<dir>_files/               # new, revealjs lib stripped
site/assets/plansjer-html/_shared/libs/revealjs/           # new, one copy
content-lock.yml                                           # new, repo root
```

Per chapter page (example, F01):

```markdown
## Forelesningsplansjer

::: {.slides}
<iframe src="../../../assets/plansjer-html/F01-investeringsprosjekter/F01-investeringsprosjekter.html"
        title="F01 · Investeringsprosjekter" loading="lazy"></iframe>
:::

[Last ned som PDF](../../../assets/plansjer/BED3_F01_investeringsprosjekter.pdf){.btn target="_blank"}
```

`site/_quarto.yml`'s `resources:` list gets `assets/plansjer-html` added
alongside the existing `assets/plansjer` entry.

## Testing

- Run `tools/sync-slides.py` against the current local course-content
  checkout (already built at commit `50b290d`); confirm all 9 decks copy,
  `content-lock.yml` records the commit.
- `quarto render` the site; open one chapter page, confirm the deck loads
  and navigates (arrow keys / click), and the PDF download link opens the
  correct file.
- Diff the 9 committed PDFs against the previous LaTeX-built versions to
  confirm real content changes, not a no-op copy.
