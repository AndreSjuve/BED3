"""Verify that site/formelark.qmd contains every formula from docs/formelark.qmd.

Extracts all display-math ($$ ... $$) blocks from both files, normalises
whitespace, and reports source formulas missing from the site page.

Known deliberate corrections of LaTeX slips in the source are listed in
KNOWN_CORRECTIONS (source form -> corrected form, whitespace-normalised);
a source formula also counts as present when its corrected form is found.

Run:  python tools/verify_formelark.py   (exit code 0 = nothing missing)
"""

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
SOURCE = ROOT / "docs" / "formelark.qmd"
SITE = ROOT / "site" / "formelark.qmd"

# Whitespace-normalised source formula -> whitespace-normalised corrected form.
KNOWN_CORRECTIONS = {
    # Black-Scholes parameters: source has "\quad_2 = d_1 - ..." — a dropped
    # "d"; the site sheet writes "\quad d_2 = d_1 - ...".
    r"d_1=\frac{\ln\left(\frac{S_0}{K}\right)+r_f\cdotT}{\sigma\sqrt{T}}"
    r"+\frac{1}{2}\cdot\sigma\cdot\sqrt{T},\quad_2=d_1-\sigma\sqrt{T}":
    r"d_1=\frac{\ln\left(\frac{S_0}{K}\right)+r_f\cdotT}{\sigma\sqrt{T}}"
    r"+\frac{1}{2}\cdot\sigma\cdot\sqrt{T},\quadd_2=d_1-\sigma\sqrt{T}",
}


def display_math(path: Path) -> list[str]:
    text = path.read_text(encoding="utf-8")
    return [m.group(1) for m in re.finditer(r"\$\$(.+?)\$\$", text, re.DOTALL)]


def normalise(formula: str) -> str:
    return re.sub(r"\s+", "", formula)


def main() -> int:
    source = display_math(SOURCE)
    site = {normalise(f) for f in display_math(SITE)}

    missing = []
    for formula in source:
        key = normalise(formula)
        key = KNOWN_CORRECTIONS.get(key, key)
        if key not in site:
            missing.append(formula.strip())

    print(f"source formulas: {len(source)}")
    print(f"site formulas:   {len(site)}")
    if missing:
        print(f"\nMISSING from site/formelark.qmd ({len(missing)}):")
        for f in missing:
            print("  " + " ".join(f.split()))
        return 1
    print("missing:         0")
    return 0


if __name__ == "__main__":
    sys.stdout.reconfigure(encoding="utf-8")
    sys.exit(main())
