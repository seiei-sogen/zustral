#!/usr/bin/env python3
"""Lists the source lines that no program of the corpus executes, and checks
that each has a reason on record.

    unexecuted-lines.py COBERTURA.xml ROOT            print them: file, line, definition, which, text
    unexecuted-lines.py --check COBERTURA.xml ROOT    compare with expected/unexecuted-lines.tsv

coverage.sh --unexecuted calls it. The generated files (the parser, the lexer,
the built-in modules) are left out.

The line report of bisect_ppx gives a line the lowest count of the points on
it, so a line counts as executed only when every point on it is. Two things
follow. A line of or-pattern alternatives is unexecuted while one alternative
on it is. And where a call that raises (`type_error` and the like) is not the
last thing of its arm, the tool puts a point after the call, which nothing
reaches: such a line is always reported, and its row on the record names the
test that runs the arm.

The record, expected/unexecuted-lines.tsv, has one row per line that no
program can execute: file, the top-level definition the line is in, which of
the lines with that text in the definition it is (1 for the first), the text
of the line without its indentation, and the reason. It is written by hand:
a reason is a claim about the compiler, and nobody can generate it. A row is
found by file, definition, which and text, not by line number, so it survives
edits elsewhere in the file. --check fails for a line that is never executed and has
no row (write a test that executes it, or a row that says why none can), and
for a row whose line is executed or gone (delete the row).
"""
import collections
import pathlib
import re
import sys
import xml.etree.ElementTree as ET

GENERATED = re.compile(r"lib/(BuiltInModules|Parser|Lexer)\.ml$")
DEFINITION = re.compile(r"^(?:let|and)(?: rec)? (?:\(?)([A-Za-z_][A-Za-z0-9_']*)")
HERE = pathlib.Path(__file__).resolve().parent


def unexecuted(report: str, root: pathlib.Path) -> list[tuple[str, int, str, int, str]]:
    rows = []
    for cls in ET.parse(report).iter("class"):
        name = cls.get("filename", "")
        if GENERATED.search(name) or not name.startswith(("lib/", "cli/")):
            continue
        source = (root / name).read_text().splitlines()
        hits = {int(line.get("number")): int(line.get("hits")) for line in cls.iter("line")}
        # The definition every line is in, and which line with its text it is there.
        definition, seen, place = "-", collections.Counter(), {}
        for number, text in enumerate(source, start=1):
            found = DEFINITION.match(text)
            if found:
                definition, seen = found.group(1), collections.Counter()
            seen[text.strip()] += 1
            place[number] = (definition, seen[text.strip()])
        for number in sorted(n for n, h in hits.items() if h == 0):
            rows.append((name, number, *place[number], source[number - 1].strip()))
    return rows


def main() -> int:
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    rows = unexecuted(args[0], pathlib.Path(args[1]))
    if "--check" not in sys.argv:
        for name, number, definition, which, text in rows:
            print(f"{name}\t{number}\t{definition}\t{which}\t{text}")
        return 0
    found = {(name, definition, str(which), text) for name, _, definition, which, text in rows}
    recorded = set()
    for line in (HERE / "expected" / "unexecuted-lines.tsv").read_text().splitlines():
        if not line or line.startswith("#"):
            continue
        name, definition, which, text, reason = line.split("\t")
        if not reason.strip():
            print(f"no reason: {name}\t{definition}\t{which}\t{text}")
            return 1
        if (name, definition, which, text) in recorded:
            print(f"two rows for one line: {name}\t{definition}\t{which}\t{text}")
            return 1
        recorded.add((name, definition, which, text))
    no_reason, stale = sorted(found - recorded), sorted(recorded - found)
    for row in no_reason:
        print("never executed, and no row says why: " + "\t".join(row))
    for row in stale:
        print("on the record, but executed or gone: " + "\t".join(row))
    print(f"unexecuted lines {len(found)}, with a reason {len(found & recorded)}, "
          f"without {len(no_reason)}, stale rows {len(stale)}")
    return 1 if no_reason or stale else 0


sys.exit(main())
