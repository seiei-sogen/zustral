#!/usr/bin/env python3
"""Lists the source lines that no program of the corpus executes, and checks
that each has a reason on record.

    unexecuted-lines.py COBERTURA.xml ROOT            print them: file, line, definition, text
    unexecuted-lines.py --check COBERTURA.xml ROOT    compare with expected/unexecuted-lines.tsv

coverage.sh --unexecuted calls it. A line counts as executed when any of its
points is. The generated files (the parser, the lexer, the built-in modules)
are left out.

The record, expected/unexecuted-lines.tsv, has one row per line that no
program can execute: file, the top-level definition the line is in, the text
of the line without its indentation, and the reason. It is written by hand:
a reason is a claim about the compiler, and nobody can generate it. A row is
found by file, definition and text, not by line number, so it survives edits
elsewhere in the file. --check fails for a line that is never executed and has
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


def unexecuted(report: str, root: pathlib.Path) -> list[tuple[str, int, str, str]]:
    rows = []
    for cls in ET.parse(report).iter("class"):
        name = cls.get("filename", "")
        if GENERATED.search(name) or not name.startswith(("lib/", "cli/")):
            continue
        source = (root / name).read_text().splitlines()
        hits: dict[int, int] = {}
        for line in cls.iter("line"):
            number = int(line.get("number"))
            hits[number] = max(hits.get(number, 0), int(line.get("hits")))
        for number in sorted(n for n, h in hits.items() if h == 0):
            definition = "-"
            for above in range(number - 1, -1, -1):
                found = DEFINITION.match(source[above])
                if found:
                    definition = found.group(1)
                    break
            rows.append((name, number, definition, source[number - 1].strip()))
    return rows


def main() -> int:
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    rows = unexecuted(args[0], pathlib.Path(args[1]))
    if "--check" not in sys.argv:
        for name, number, definition, text in rows:
            print(f"{name}\t{number}\t{definition}\t{text}")
        return 0
    found = collections.Counter((name, definition, text) for name, _, definition, text in rows)
    recorded: collections.Counter = collections.Counter()
    for line in (HERE / "expected" / "unexecuted-lines.tsv").read_text().splitlines():
        if not line or line.startswith("#"):
            continue
        name, definition, text, reason = line.split("\t")
        if not reason.strip():
            print(f"no reason: {name}\t{definition}\t{text}")
            return 1
        recorded[(name, definition, text)] += 1
    no_reason = found - recorded
    stale = recorded - found
    for (name, definition, text), count in sorted(no_reason.items()):
        print(f"never executed, and no row says why ({count}): {name}\t{definition}\t{text}")
    for (name, definition, text), count in sorted(stale.items()):
        print(f"on the record, but executed or gone ({count}): {name}\t{definition}\t{text}")
    print(f"unexecuted lines {sum(found.values())}, with a reason {sum((found & recorded).values())}, "
          f"without {sum(no_reason.values())}, stale rows {sum(stale.values())}")
    return 1 if no_reason or stale else 0


sys.exit(main())
