#!/usr/bin/env python3
"""Counts the source lines that no program of the corpus executes, by kind.

    unexecuted-kinds.py COBERTURA.xml ROOT [--files]

coverage.sh --kinds calls it. The kinds are told apart by the text of the line
and of the three lines after it, so the counts are rough: they say where the
unexecuted code is, not exactly what it is. A line counts as executed when
every point on it is (see unexecuted-lines.py).
"""
import collections
import pathlib
import re
import sys
import xml.etree.ElementTree as ET

GENERATED = re.compile(r"lib/(BuiltInModules|Parser|Lexer)\.ml$")
CONSTANT = r"(false|true|None|\[\]|\(\))"
USER_ERROR = r"(type|declaration|generic|linearity|parse|cli|entrypoint)_error"


def kind_of(name: str, text: str, after: str) -> str:
    if name.endswith("Errors.ml"):
        return "wording of an error (lib/*Errors.ml)"
    if "internal_err" in text or "internal_err" in after[:80]:
        return "an arm that raises an internal error"
    if text.startswith("|") and "->" not in text:
        return "a line of alternatives of an or-pattern"
    if re.search(rf"->\s*{CONSTANT}\s*$", text) or re.fullmatch(CONSTANT, text):
        return "an arm that returns a constant"
    if re.search(USER_ERROR, text) or re.search(USER_ERROR, after[:120]):
        return "an arm that raises an error for the user"
    return "other code"


def main() -> None:
    report, root = sys.argv[1], pathlib.Path(sys.argv[2])
    kinds: collections.Counter = collections.Counter()
    per_file: dict = collections.defaultdict(int)
    with_point = 0
    for cls in ET.parse(report).iter("class"):
        name = cls.get("filename", "")
        if GENERATED.search(name) or not name.startswith(("lib/", "cli/")):
            continue
        source = (root / name).read_text().splitlines()
        hits = {int(line.get("number")): int(line.get("hits")) for line in cls.iter("line")}
        with_point += len(hits)
        for number in sorted(n for n, h in hits.items() if h == 0):
            text = source[number - 1].strip()
            after = " ".join(s.strip() for s in source[number:number + 3])
            kinds[kind_of(name, text, after)] += 1
            per_file[name] += 1
    missed = sum(kinds.values())
    print(f"lines-with-a-point\t{with_point}")
    print(f"never-executed\t{missed}\t{100 * (with_point - missed) / with_point:.2f}% executed")
    for kind, count in kinds.most_common():
        print(f"{count}\t{kind}")
    if "--files" in sys.argv:
        for name, count in sorted(per_file.items(), key=lambda item: -item[1]):
            print(f"{count}\t{name}")


main()
