"""Build the lecture-slide PDFs from the LaTeX sources in docs/Forelesninger.

Usage:  C:\\Python314\\python.exe tools/build-plansjer.py [--only F02]

Pipeline (established 2026-09-01):
  1. Each deck ships as docs/Forelesninger/BED3_FNN_*.zip containing main.tex,
     bed3forelesning.sty and figures. Decks use the beamer *metropolis* theme
     and fontspec, so they MUST be compiled with xelatex, not pdflatex.
  2. TinyTeX (TeX Live 2025) needs these extra packages, installed once from
     the frozen 2025 archive because CTAN has rolled to 2026 and tlmgr refuses
     cross-release installs:
         tlmgr --repository {REPO} install \
             beamertheme-metropolis pgfopts fira media9 sourcesanspro
     (pdfbase.sty lives inside media9 — searching for a "pdfbase" package
     finds nothing.)
  3. xelatex runs twice per deck (TOC/refs), output lands in
     site/assets/plansjer/<ascii-name>.pdf. Norwegian characters are kept in
     the PDF content but not in the file names.

The chapter index pages link these files; _quarto.yml lists site/assets as a
resource so Quarto copies them into _site.
"""

import argparse
import io
import shutil
import subprocess
import sys
import tempfile
import zipfile
from pathlib import Path

REPO_URL = "https://ftp.math.utah.edu/pub/tex/historic/systems/texlive/2025/tlnet-final"
ROOT = Path(__file__).resolve().parents[1]
SRC = ROOT / "docs" / "Forelesninger"
OUT = ROOT / "site" / "assets" / "plansjer"

# zip stem -> published file stem (ascii, url-safe)
NAMES = {
    "BED3_F01_Investeringsprosjekter": "BED3_F01_investeringsprosjekter",
    "BED3_F02__Effekter_av_skatt_og_lån": "BED3_F02_effekter-av-skatt-og-laan",
    "BED3_F03__Inflasjon": "BED3_F03_inflasjon",
    "BED3_F04_Risikovurderinger": "BED3_F04_risikovurderinger",
    "BED3_F05_Låne__og_betalingsalternativer": "BED3_F05_laane-og-betalingsalternativer",
    "BED3_F13_Rentemarkedet": "BED3_F13_rentemarkedet",
    "BED3_F14_Opsjonskontrakter": "BED3_F14_opsjonskontrakter",
    "BED3_F15_Terminkontrakter": "BED3_F15_terminkontrakter",
    "BED3_F16_Internasjonal_finans": "BED3_F16_internasjonal-finans",
}


def build(stem, outname, workroot):
    zpath = SRC / f"{stem}.zip"
    if not zpath.exists():
        print(f"SKIP  {stem}: {zpath} not found")
        return False
    work = workroot / stem
    with zipfile.ZipFile(zpath) as z:
        z.extractall(work)
    for i in (1, 2):
        r = subprocess.run(
            ["xelatex", "-interaction=nonstopmode", "main.tex"],
            cwd=work, capture_output=True,
        )
    pdf = work / "main.pdf"
    if r.returncode != 0 or not pdf.exists():
        log = (work / "main.log")
        tail = log.read_text(errors="replace")[-1500:] if log.exists() else "(no log)"
        print(f"FAIL  {stem}\n{tail}")
        return False
    OUT.mkdir(parents=True, exist_ok=True)
    shutil.copy(pdf, OUT / f"{outname}.pdf")
    print(f"OK    {stem} -> {outname}.pdf ({pdf.stat().st_size // 1024} kB)")
    return True


def main():
    sys.stdout.reconfigure(encoding="utf-8")
    ap = argparse.ArgumentParser()
    ap.add_argument("--only", help="build a single deck, e.g. F02")
    args = ap.parse_args()

    todo = {k: v for k, v in NAMES.items() if not args.only or f"_{args.only}_" in f"_{k}_" or args.only in k}
    if not todo:
        sys.exit(f"nothing matches --only {args.only}")
    failures = 0
    with tempfile.TemporaryDirectory(prefix="plansjer-") as tmp:
        for stem, outname in sorted(todo.items()):
            if not build(stem, outname, Path(tmp)):
                failures += 1
    if failures:
        sys.exit(f"{failures} deck(s) failed — if a package is missing, install it from\n  {REPO_URL}\n(see module docstring).")
    print("All decks built into", OUT)


if __name__ == "__main__":
    main()
