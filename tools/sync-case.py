"""Sync the EcoTech case (HTML + PDF) from BED3-teaching.

Usage:  C:\\Python314\\python.exe tools/sync-case.py [--force]
        BED3_TEACHING_PATH=... python tools/sync-case.py

BED3-teaching owns the case source (cases/ecotech/). It builds two
artifacts with its own `quarto render case.qmd --to <format> --profile
<edition>`: HTML (student-facing, spliced into the chapter page) and PDF
(Canvas download). Both are gitignored there -- they exist only in a
locally built working tree, never in git history -- so this script reads a
local sibling checkout rather than cloning or fetching, and does not
invoke quarto/R itself.

The synced HTML is post-processed before it lands in the website:
  - the PDF-only title block and Quarto's own <head>/<header> chrome are
    stripped, since the website page supplies its own title and script
    tags -- only the body content students read is kept.
  - each "Løsningsforslag ..." section is wrapped in the site's own
    <details class="solution"> reveal pattern (see
    docs/video-page-conventions.md), matching every other exercise
    solution on the site. BED3-teaching's own HTML ships these sections
    as plain, always-visible content; the collapsible reveal is a website
    presentation choice, not something the source needs to know about.

Refuses to run against a dirty BED3-teaching working tree -- the built
artifacts might not correspond to any committed source -- unless --force
is given. Writes its own `sources.teaching` key in content-lock.yml
(shared with tools/sync-slides.py's `sources.course_content` key; see
that script for the merge approach) as the audit trail.
"""

import argparse
import datetime
import os
import re
import subprocess
import sys
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[1]
SITE = ROOT / "site"
CASE_DIR = SITE / "kursmateriale" / "01-investeringsanalyse" / "03-skatt-og-laan"
HTML_DEST = CASE_DIR / "_ecotech-case.html"
PDF_OUT_DIR = SITE / "assets" / "cases" / "ecotech"
PDF_DEST_NAME = "BED3-EcoTech-Electronics-2026.pdf"
LOCK_FILE = ROOT / "content-lock.yml"

SOLUTION_SECTIONS = [
    'id="sec-lf"',
    'id="sec-lf-tax-financing"',
]


def git(args: list[str], cwd: Path) -> str:
    r = subprocess.run(["git", *args], cwd=cwd, capture_output=True, text=True)
    if r.returncode != 0:
        sys.exit(f"error: git {' '.join(args)} failed in {cwd}\n{r.stderr}")
    return r.stdout.strip()


def extract_body(html: str) -> str:
    body_match = re.search(r"<body>(.*)</body>", html, re.DOTALL)
    if not body_match:
        sys.exit("error: could not find <body>...</body> in the rendered case HTML")
    body = body_match.group(1)
    # Quarto's own title-block chrome (university name, case number,
    # author) -- the website page supplies its own title, not this.
    body = re.sub(
        r'<header id="title-block-header">.*?</header>', "", body, flags=re.DOTALL
    )
    return body.strip()


def wrap_solution(fragment: str) -> str:
    return (
        '<details class="solution">\n<summary>Vis løsningsforslag</summary>\n\n'
        '<div class="solution__body">\n\n'
        + fragment.strip() +
        '\n\n</div>\n\n</details>\n'
    )


def wrap_solutions(body: str) -> str:
    """Wrap each "Løsningsforslag ..." <h2> section (and everything up to
    the next one, or end of document) in the site's collapsible reveal
    pattern. Sections whose heading isn't present in this build (e.g. the
    show-notes-gated tasks 3-4 solution, when show-notes was false) are
    simply skipped -- there's nothing to wrap."""
    def find_h2_start(marker: str) -> int | None:
        idx = body.find(marker)
        if idx == -1:
            return None
        start = body.rfind("<h2", 0, idx)
        if start == -1:
            sys.exit(f"error: found {marker} but not its enclosing <h2 in the case HTML")
        return start

    bounds = [b for b in (find_h2_start(m) for m in SOLUTION_SECTIONS) if b is not None]
    if not bounds:
        return body

    out = body[: bounds[0]]
    for i, start in enumerate(bounds):
        end = bounds[i + 1] if i + 1 < len(bounds) else len(body)
        out += wrap_solution(body[start:end])
    return out


def sync(source: Path) -> dict:
    case_dir = source / "cases" / "ecotech"
    html_src = case_dir / "case.html"
    pdf_src = case_dir / "case.pdf"
    for p in (html_src, pdf_src):
        if not p.exists():
            sys.exit(f"error: {p} not found -- render both formats in {case_dir} "
                      f"first (quarto render case.qmd --to html --profile 2026 "
                      f"&& quarto render case.qmd --to course-pdf --profile 2026)")

    html = html_src.read_text(encoding="utf-8")
    body = extract_body(html)
    body = wrap_solutions(body)
    HTML_DEST.write_text(body, encoding="utf-8")

    PDF_OUT_DIR.mkdir(parents=True, exist_ok=True)
    pdf_dest = PDF_OUT_DIR / PDF_DEST_NAME
    pdf_dest.write_bytes(pdf_src.read_bytes())

    print(f"  -> {HTML_DEST.relative_to(ROOT)} ({HTML_DEST.stat().st_size / 1024:.0f} kB)")
    print(f"  -> {pdf_dest.relative_to(ROOT)} ({pdf_dest.stat().st_size / 1024:.0f} kB)")

    return {
        "html": str(HTML_DEST.relative_to(ROOT)).replace("\\", "/"),
        "html_bytes": HTML_DEST.stat().st_size,
        "pdf": str(pdf_dest.relative_to(ROOT)).replace("\\", "/"),
        "pdf_bytes": pdf_dest.stat().st_size,
        "solution_sections_found": len(
            [m for m in SOLUTION_SECTIONS if m.split('"')[1] in body]
        ),
    }


def write_lock(source: Path, commit: str, case_info: dict) -> None:
    doc = {}
    if LOCK_FILE.exists():
        doc = yaml.safe_load(LOCK_FILE.read_text(encoding="utf-8")) or {}
    doc["schema_version"] = 2
    remote = None
    try:
        remote = git(["remote", "get-url", "origin"], source)
    except SystemExit:
        remote = None
    doc.setdefault("sources", {})["teaching"] = {
        "repo": remote or f"local (no remote configured): {source}",
        "commit": commit,
        "synced_at": datetime.datetime.now().isoformat(timespec="seconds"),
        "case": {"id": "ecotech", **case_info},
    }
    header = (
        "# Records which producing-repo commit generated the assets currently\n"
        "# committed under site/assets/. One key per sync script under `sources:`\n"
        "# (tools/sync-slides.py -> course_content, tools/sync-case.py -> teaching).\n"
        "# Do not hand-edit; re-run the relevant sync script.\n"
    )
    LOCK_FILE.write_text(
        header + yaml.dump(doc, allow_unicode=True, sort_keys=False, default_flow_style=False),
        encoding="utf-8",
    )


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--source", type=Path,
                     default=Path(os.environ.get("BED3_TEACHING_PATH",
                                                   ROOT.parent / "BED3-teaching")))
    ap.add_argument("--force", action="store_true",
                     help="sync even if the source working tree is dirty")
    args = ap.parse_args()

    source = args.source.resolve()
    if not (source / ".git").exists():
        sys.exit(f"error: {source} is not a git repository "
                  f"(set --source or BED3_TEACHING_PATH)")

    # Scoped to the paths this script actually reads from, not the whole
    # repo -- BED3-teaching is a shared, actively-worked-in repo, and an
    # unrelated in-progress file elsewhere in it (another case, another
    # session) shouldn't block a sync that doesn't touch it.
    status = git(["status", "--porcelain", "--", "cases/ecotech", "pkg/bed3case"], source)
    if status and not args.force:
        sys.exit(f"error: {source}'s cases/ecotech or pkg/bed3case has "
                  f"uncommitted changes -- commit or stash them first (the "
                  f"built artifacts might not match any committed source), "
                  f"or pass --force to sync anyway:\n{status}")

    commit = git(["rev-parse", "HEAD"], source)
    print(f"-- ecotech case")
    info = sync(source)
    write_lock(source, commit, info)
    print(f"\nsynced ecotech case from {source} @ {commit[:12]}")
    print(f"solution sections wrapped: {info['solution_sections_found']}/{len(SOLUTION_SECTIONS)}")
    print(f"lock file: {LOCK_FILE.relative_to(ROOT)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
