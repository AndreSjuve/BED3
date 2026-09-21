# BED3 — published site

This repository publishes <https://andresjuve.github.io/BED3/>. It holds **built output only**.

The source lives in **`AndreSjuve/bed3-course`** (private): lecture content, cases, problem
sets, exam material and the website project, in one repository.

Nothing here is edited by hand. The site is built from the source repository and pushed to the
`gh-pages` branch of this one, wholesale, on each publish. Anything committed here by hand will
be overwritten by the next publish and lost.

| branch | contents |
|---|---|
| `gh-pages` | the built site — what GitHub Pages serves |
| `master` | this note |

The repository name is part of the public URL, so it stays `BED3` even though the source
repository is `bed3-course`. Renaming it would break every link anyone holds.

## History

Until September 2026 this repository held the website *source*, and the built lecture slides
were committed into it by a sync script that copied them out of a separate content repository.
That history is still in this branch and in `bed3-course`, where the five BED3 repositories
were merged. It is no longer how anything is built.
