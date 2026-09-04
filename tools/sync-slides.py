"""Sync built lecture decks (HTML + PDF) from BED3-course-content.

Usage:  C:\\Python314\\python.exe tools/sync-slides.py [--deck F04] [--force]
        BED3_COURSE_CONTENT_PATH=... python tools/sync-slides.py

BED3-course-content owns the lecture sources. It builds two artifacts per
deck with its own `python scripts/build.py`: HTML (RevealJS, the
student-facing viewer) and PDF (for Canvas downloads). Both are gitignored
there -- they exist only in a locally built working tree, never in git
history -- so this script reads a local sibling checkout rather than
cloning or fetching. It does not invoke quarto/R/decktape itself; if a
deck's artifacts are missing, run `python scripts/build.py --deck <ID>` in
course-content first.

Copies, per migrated deck (see lectures/decks.yml there):
    _artifacts/pdf/<published_pdf>   -> site/assets/plansjer/<published_pdf>
    lectures/<dir>/<dir>.html        -> site/assets/plansjer-html/<dir>/<dir>.html
    lectures/<dir>/<dir>_files/      -> site/assets/plansjer-html/<dir>/<dir>_files/
        (minus libs/revealjs, which is identical across every deck and is
        merged once into site/assets/plansjer-html/_shared/libs/revealjs/;
        the copied HTML is rewritten to reference that shared copy)
    lectures/<dir>/figures/          -> site/assets/plansjer-html/<dir>/figures/
        (R-generated chart SVGs the deck's slides reference directly, e.g.
        figures/fordeling-ks.svg; only copied when the source has one)

Refuses to run against a dirty course-content working tree -- the built
artifacts might not correspond to any committed source -- unless --force is
given. Writes content-lock.yml at the website repo root as the audit trail:
source commit, sync time, and what was copied.
"""

import argparse
import filecmp
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SITE = ROOT / "site"
PDF_OUT = SITE / "assets" / "plansjer"
HTML_OUT = SITE / "assets" / "plansjer-html"
SHARED_REVEALJS = HTML_OUT / "_shared" / "libs" / "revealjs"
LOCK_FILE = ROOT / "content-lock.yml"


def load_decks(source: Path) -> list[dict]:
    """Minimal reader for lectures/decks.yml (same approach as course-content's
    own scripts/build.py: the file is flat scalars plus one string list, so a
    YAML dependency buys nothing)."""
    text = (source / "lectures" / "decks.yml").read_text(encoding="utf-8")
    decks, cur = [], None
    for raw in text.splitlines():
        line = raw.rstrip()
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        if re.match(r"^  - id:", line):
            cur = {"id": line.split(":", 1)[1].strip()}
            decks.append(cur)
            continue
        if cur is None:
            continue
        m = re.match(r"^    (\w+):\s*(.*)$", line)
        if m and m.group(1) != "figures_r":
            cur[m.group(1)] = m.group(2).strip()
    return decks


def git(args: list[str], cwd: Path) -> str:
    r = subprocess.run(["git", *args], cwd=cwd, capture_output=True, text=True)
    if r.returncode != 0:
        sys.exit(f"error: git {' '.join(args)} failed in {cwd}\n{r.stderr}")
    return r.stdout.strip()


def merge_revealjs(deck_files: Path) -> None:
    """Copy files under <deck>_files/libs/revealjs into the shared folder,
    skipping any that already exist there. Warns instead of overwriting if a
    same-named file differs -- that would mean two decks were built against
    different RevealJS assets, which the shared-copy design assumes can't
    happen."""
    src = deck_files / "libs" / "revealjs"
    if not src.exists():
        sys.exit(f"error: {src} not found")
    for path in src.rglob("*"):
        if path.is_dir():
            continue
        rel = path.relative_to(src)
        dest = SHARED_REVEALJS / rel
        if dest.exists():
            if not filecmp.cmp(path, dest, shallow=False):
                print(f"  WARNING: {rel} differs from the shared copy already "
                      f"synced; keeping the shared copy (from an earlier deck)")
            continue
        dest.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(path, dest)


def sync_deck(source: Path, deck: dict) -> dict:
    deck_id, dir_, published_pdf = deck["id"], deck["dir"], deck["published_pdf"]
    print(f"-- {deck_id} ({deck.get('title', dir_)})")

    pdf_src = source / "_artifacts" / "pdf" / published_pdf
    html_src = source / "lectures" / dir_ / f"{dir_}.html"
    files_src = source / "lectures" / dir_ / f"{dir_}_files"
    figures_src = source / "lectures" / dir_ / "figures"
    for p in (pdf_src, html_src, files_src):
        if not p.exists():
            sys.exit(f"error: {p} not found -- run "
                      f"`python scripts/build.py --deck {deck_id}` in "
                      f"{source} first")

    PDF_OUT.mkdir(parents=True, exist_ok=True)
    pdf_dest = PDF_OUT / published_pdf
    shutil.copy2(pdf_src, pdf_dest)

    deck_out = HTML_OUT / dir_
    if deck_out.exists():
        shutil.rmtree(deck_out)
    deck_out.mkdir(parents=True)

    files_dest = deck_out / f"{dir_}_files"
    shutil.copytree(files_src, files_dest)
    shutil.rmtree(files_dest / "libs" / "revealjs")
    merge_revealjs(files_src)

    if figures_src.exists():
        shutil.copytree(figures_src, deck_out / "figures")

    html_text = html_src.read_text(encoding="utf-8")
    html_text = html_text.replace(f"{dir_}_files/libs/revealjs/",
                                   "../_shared/libs/revealjs/")
    html_dest = deck_out / f"{dir_}.html"
    html_dest.write_text(html_text, encoding="utf-8")

    print(f"  -> {pdf_dest.relative_to(ROOT)} "
          f"({pdf_dest.stat().st_size / 1024:.0f} kB)")
    print(f"  -> {html_dest.relative_to(ROOT)} "
          f"({html_dest.stat().st_size / 1024:.0f} kB)")

    return {
        "id": deck_id,
        "title": deck.get("title", dir_),
        "pdf": str(pdf_dest.relative_to(ROOT)).replace("\\", "/"),
        "pdf_bytes": pdf_dest.stat().st_size,
        "html": str(html_dest.relative_to(ROOT)).replace("\\", "/"),
        "html_bytes": html_dest.stat().st_size,
    }


def write_lock(source: Path, commit: str, synced: list[dict]) -> None:
    """content-lock.yml has one top-level key per sync script (course_content
    here, teaching from sync-case.py) under `sources:`, so each script only
    ever touches its own key and never clobbers the other's."""
    import datetime
    import yaml

    doc = {}
    if LOCK_FILE.exists():
        doc = yaml.safe_load(LOCK_FILE.read_text(encoding="utf-8")) or {}
    doc["schema_version"] = 2
    doc.setdefault("sources", {})["course_content"] = {
        "repo": git(["remote", "get-url", "origin"], source),
        "commit": commit,
        "synced_at": datetime.datetime.now().isoformat(timespec="seconds"),
        "decks": synced,
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
                     default=Path(os.environ.get("BED3_COURSE_CONTENT_PATH",
                                                   ROOT.parent / "BED3-course-content")))
    ap.add_argument("--deck", action="append", help="deck id, e.g. F04 (repeatable)")
    ap.add_argument("--force", action="store_true",
                     help="sync even if the source working tree is dirty")
    args = ap.parse_args()

    source = args.source.resolve()
    if not (source / ".git").exists():
        sys.exit(f"error: {source} is not a git repository "
                  f"(set --source or BED3_COURSE_CONTENT_PATH)")

    # Scoped to the paths this script actually reads from, not the whole
    # repo -- course-content is a shared, actively-worked-in repo, and an
    # unrelated in-progress file elsewhere in it shouldn't block a sync
    # that doesn't touch it.
    status = git(["status", "--porcelain", "--", "lectures", "_artifacts"], source)
    if status and not args.force:
        sys.exit(f"error: {source}'s lectures or _artifacts has uncommitted "
                  f"changes -- commit or "
                  f"stash them first (the built artifacts might not match "
                  f"any committed source), or pass --force to sync anyway:\n{status}")

    commit = git(["rev-parse", "HEAD"], source)

    decks = [d for d in load_decks(source) if d.get("status") == "migrated"]
    if args.deck:
        wanted = {d.upper() for d in args.deck}
        decks = [d for d in decks if d["id"].upper() in wanted]
        missing = wanted - {d["id"].upper() for d in decks}
        if missing:
            sys.exit(f"error: unknown or non-migrated deck(s): {', '.join(sorted(missing))}")
    if not decks:
        sys.exit("error: no decks to sync")

    synced = [sync_deck(source, deck) for deck in decks]
    write_lock(source, commit, synced)
    print(f"\nsynced {len(synced)} deck(s) from {source} @ {commit[:12]}")
    print(f"lock file: {LOCK_FILE.relative_to(ROOT)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
