"""Check that no quiz answer key is given away by the page's intro text.

Usage:  python tools/check_overlap.py <chapter-folder> [n-gram size, default 4]

For every .qmd in the folder with a "Før du ser videoen" section and a quiz,
extract the intro prose and each question's correct-answer text, and report any
shared word n-gram. The intro must orient, not answer: a verbatim echo lets the
student pass by lexical match instead of retrieval. Exit code 1 on any hit.

This catches verbatim leakage only. A one-word key ("Internrenten") or a
paraphrase can still leak — that stays a human check in review.
"""

import glob
import io
import os
import re
import sys

sys.stdout.reconfigure(encoding="utf-8")

STOP_GRAMS = set()  # add known-benign grams here if they ever appear


def word_ngrams(text, n):
    words = re.sub(r"[^a-zæøåéA-ZÆØÅÉ ]", " ", text.lower()).split()
    return {" ".join(words[i : i + n]) for i in range(len(words) - n + 1)}


def check_file(path, n):
    text = io.open(path, encoding="utf-8").read()
    if "## Før du ser videoen" not in text or "quiz__q" not in text:
        return []
    intro = text.split("## Før du ser videoen")[1].split(":::::")[0]
    intro_grams = word_ngrams(intro, n)
    hits = []
    for qi, block in enumerate(text.split("quiz__q")[1:], 1):
        m = re.search(r"data-answer=.([abc])", block)
        if not m:
            continue
        key = m.group(1)
        km = re.search(
            r"data-key=.%s.><span class=.choice__key.>[ABC]</span><span>(.*?)</span>" % key,
            block,
        )
        if not km:
            continue
        shared = (word_ngrams(km.group(1), n) & intro_grams) - STOP_GRAMS
        if shared:
            hits.append((qi, sorted(shared)))
    return hits


def main():
    folder = sys.argv[1] if len(sys.argv) > 1 else "."
    n = int(sys.argv[2]) if len(sys.argv) > 2 else 4
    total = 0
    for path in sorted(glob.glob(os.path.join(folder, "*.qmd"))):
        hits = check_file(path, n)
        for qi, grams in hits:
            total += 1
            print(f"LEAK  {os.path.basename(path)}  Q{qi}: intro echoes key: {grams[0]!r}")
    if total:
        print(f"\n{total} leak(s). Reword the intro to pose, not answer.")
        sys.exit(1)
    print("OK: no intro/answer-key overlap.")


if __name__ == "__main__":
    main()
