r"""Build site/assets/formelark.pdf from site/formelark.qmd.

Single-sourcing mechanism
-------------------------
site/formelark.qmd is the only place formulas are edited. It holds the
formulas as HTML "cards" (:::: {.formula} blocks with an ### name, a $$ math
block, and web-only glossaries/notes). This script parses that file and
generates a temporary print .qmd — the compact exam-attachment layout
("Vedlegg til eksamen i BED3 Investering og finans": sections as headings,
"**Name:** formula" list items, no cards, no glossaries) — renders it to a
Typst PDF with Quarto, and copies the result to site/assets/formelark.pdf.

What the parser relies on in site/formelark.qmd:
  * "## Heading"            -> a PDF section heading
  * ":::: {.formula ...}"   -> starts a card
  * "### Name"              -> the card's formula name (inline $math$ allowed)
  * "$$ ... $$"             -> the card's formula (first math block per card)
Everything else (glossaries, notes, grid/asterism fences) is web-only chrome
and is dropped from the PDF.

Why Typst, and why the .typ post-processing:
  * LaTeX display math inside list items renders airy (~7 pages); the target
    exam attachment is a compact ~3-pager with each formula on its name's
    line. Typst inline math gives exactly that.
  * Pandoc only emits Typst *block* math for $$..$$ ("$ x $"), so after
    `quarto render --to typst` (with keep-typ) the script rewrites the .typ:
      - "$ x $" -> "$x$"  (inline math: formula sits on the name's line)
      - "^Tdot.op" -> "^T dot.op"  (pandoc texmath bug: the space is dropped
        after a superscript before \cdot, which breaks Typst compilation)
    and then compiles with Quarto's embedded Typst (`quarto typst compile`).
    The initial render's own compile step may fail on that texmath bug; the
    script only needs the .typ it leaves behind.

Run:  python tools/build-formelark-pdf.py
Temp render files go to the session scratchpad (or %TEMP%\formelark-build).
"""

import os
import re
import shutil
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
SITE_QMD = ROOT / "site" / "formelark.qmd"
OUT_PDF = ROOT / "site" / "assets" / "formelark.pdf"

QUARTO = Path(
    r"C:\Users\s12600\AppData\Local\Programs\Positron"
    r"\resources\app\quarto\bin\quarto.cmd"
)

PRINT_HEADER = """\
---
title: "Vedlegg til eksamen i BED3 Investering og finans"
format:
  typst:
    number-sections: false
    fontsize: 10pt
    margin:
      x: 2cm
      y: 2cm
    keep-typ: true
---
"""


def parse_cards(text: str):
    """Yield ('section', title) and ('formula', name, math) in page order."""
    lines = text.split("\n")
    i = 0
    while i < len(lines):
        line = lines[i]
        if line.startswith("## "):
            yield ("section", line[3:].strip())
        elif re.match(r"^:{4}\s*\{\.formula", line):
            name, math = None, None
            i += 1
            while i < len(lines) and not re.match(r"^:{4}\s*$", lines[i]):
                if lines[i].startswith("### ") and name is None:
                    name = lines[i][4:].strip()
                elif lines[i].strip() == "$$" and math is None:
                    j = i + 1
                    body = []
                    while j < len(lines) and lines[j].strip() != "$$":
                        body.append(lines[j])
                        j += 1
                    math = "\n".join(body)
                    i = j
                i += 1
            if name is None or math is None:
                raise SystemExit(f"card missing name or math near line {i}")
            yield ("formula", name, math)
        i += 1


def build_print_qmd(text: str) -> str:
    parts = [PRINT_HEADER]
    for item in parse_cards(text):
        if item[0] == "section":
            parts.append(f"\n## {item[1]}\n")
        else:
            _, name, math = item
            parts.append(f"- **{name}:**\n  $$\n{math}\n  $$\n")
    return "\n".join(parts)


def main() -> int:
    scratch = os.environ.get("CLAUDE_SCRATCHPAD")
    if scratch:
        build_dir = Path(scratch) / "formelark-build"
    else:
        build_dir = Path(os.environ.get("TEMP", ".")) / "formelark-build"
    build_dir.mkdir(parents=True, exist_ok=True)

    src = SITE_QMD.read_text(encoding="utf-8")
    print_qmd = build_dir / "formelark-print.qmd"
    print_qmd.write_text(build_print_qmd(src), encoding="utf-8", newline="\n")
    print(f"wrote {print_qmd}")

    # Step 1: qmd -> .typ. The render's own Typst compile may fail on the
    # texmath "^Tdot.op" bug; we only need the .typ it leaves behind.
    typ = build_dir / "formelark-print.typ"
    typ.unlink(missing_ok=True)
    result = subprocess.run(
        [str(QUARTO), "render", str(print_qmd), "--to", "typst"],
        cwd=build_dir,
        capture_output=True,
        text=True,
        encoding="utf-8",
        errors="replace",
    )
    if not typ.exists():
        sys.stdout.write(result.stdout or "")
        sys.stderr.write(result.stderr or "")
        print("quarto render produced no .typ file", file=sys.stderr)
        return result.returncode or 1

    # Step 2: post-process the .typ (see module docstring).
    text = typ.read_text(encoding="utf-8")
    text, n_bug = re.subn(r"\^([A-Za-z0-9]+)dot\.op", r"^\1 dot.op", text)
    text, n_inline = re.subn(
        r"\$ (.*?) \$", lambda m: "$" + m.group(1) + "$", text, flags=re.DOTALL
    )
    typ.write_text(text, encoding="utf-8", newline="\n")
    print(f"post-processed .typ: {n_inline} math blocks inlined, "
          f"{n_bug} texmath dot.op fixes")

    # Step 3: compile with Quarto's embedded Typst.
    result = subprocess.run(
        [str(QUARTO), "typst", "compile", str(typ)],
        cwd=build_dir,
        capture_output=True,
        text=True,
        encoding="utf-8",
        errors="replace",
    )
    sys.stdout.write(result.stdout or "")
    sys.stderr.write(result.stderr or "")
    if result.returncode != 0:
        return result.returncode

    rendered = build_dir / "formelark-print.pdf"
    OUT_PDF.parent.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(rendered, OUT_PDF)
    print(f"copied -> {OUT_PDF}")
    return 0


if __name__ == "__main__":
    sys.stdout.reconfigure(encoding="utf-8")
    sys.exit(main())
