#!/usr/bin/env python3
"""Lists the source lines that no program of the corpus executes in full, and
checks that each has a reason on record.

    unexecuted-lines.py POINTS ROOT            print them: file, line, definition, which, points, text
    unexecuted-lines.py --check POINTS ROOT    compare with expected/unexecuted-lines.tsv

coverage.sh --unexecuted calls it with the file `bisect-ppx-report merge`
writes: for every source file, the offset of each point and how often it was
hit. The generated files (the parser, the lexer, the built-in modules) are
left out.

A line is listed when one of the points on it was never hit (the line report
of bisect_ppx counts lines the same way). The column `points` says whether
that is all of them (`none` was hit) or not (`some` were). A line of
or-pattern alternatives is listed while one alternative on it is unexecuted.

One kind of point is left out. For a call that is not the last thing of its
arm, bisect_ppx adds a point that is hit when the call returns, and records
it at the last character of the function's name. The functions that raise an
error (`type_error` and the others of lib/Error.mli, `internal_err`,
`report_and_exit`, `exit`) never return, so that point of theirs is hit by
nothing, whatever the tests do. The points of the call's own arm stay.

The record, expected/unexecuted-lines.tsv, has one row per listed line: file,
the top-level definition the line is in (`name#2` for the second definition of
a name in the file), which of the lines with that text in the definition it is
(1 for the first), `points`, the text of the line without its indentation, and
the reason why no program can execute the rest of it. The reason is written by
hand: it is a claim about the compiler, and nobody can generate it. A row is
found by file, definition, which and text, not by line number, so it survives
edits elsewhere in the file.

--check fails for a listed line without a row (write a test that executes it,
or a row that says why none can), for a row whose line is not listed any more
(delete the row), and for a row whose `points` differs from what was measured:
`some` on the record and `none` measured means that the test which executed
part of the line is gone.
"""
import collections
import pathlib
import re
import sys

GENERATED = re.compile(r"lib/(BuiltInModules|Parser|Lexer)\.ml$")
DEFINITION = re.compile(r"^(?:let|and)(?: rec)? (?:\(?)([A-Za-z_][A-Za-z0-9_']*)")
# The functions whose type says that they do not return (lib/Error.mli,
# cli/CliEngine.mli), and `exit`.
NEVER_RETURNS = {
    "parse_error", "cli_error", "type_error", "linearity_error", "declaration_error",
    "entrypoint_error", "generic_error", "internal_err", "report_and_exit", "exit",
}
NAME_AT_END = re.compile(r"[A-Za-z_][A-Za-z0-9_']*$")
HERE = pathlib.Path(__file__).resolve().parent
Row = tuple[str, int, str, int, str, str]


def load_points(coverage_file: str) -> dict[str, list[tuple[int, int]]]:
    """Per source file, its points: (offset, hits)."""
    words = iter(pathlib.Path(coverage_file).read_bytes().split(b" "))
    if next(words) != b"BISECT-COVERAGE-4":
        sys.exit(f"{coverage_file}: not a file written by bisect-ppx-report merge")
    points = {}
    for _ in range(int(next(words))):
        next(words)  # the length of the name; no source file has a space in its name
        name = next(words).decode()
        offsets = [int(next(words)) for _ in range(int(next(words)))]
        counts = [int(next(words)) for _ in range(int(next(words)))]
        points[name] = list(zip(offsets, counts))
    return points


def unexecuted(coverage_file: str, root: pathlib.Path) -> tuple[list[Row], int]:
    rows, with_point = [], 0
    for name, points in sorted(load_points(coverage_file).items()):
        if GENERATED.search(name) or not name.startswith(("lib/", "cli/")):
            continue
        # One character per byte: the points are byte offsets.
        text = (root / name).read_text(encoding="latin-1")
        hits = collections.defaultdict(list)
        for offset, count in points:
            name_here = NAME_AT_END.search(text, max(0, offset - 80), offset + 1)
            after_name = offset + 1 == len(text) or not (text[offset + 1].isalnum() or text[offset + 1] in "_'")
            if count == 0 and name_here and after_name and name_here.group(0) in NEVER_RETURNS:
                continue
            hits[text.count("\n", 0, offset) + 1].append(count)
        with_point += len(hits)
        # The definition every line is in, and which line with its text it is there.
        definition, names, seen, place = "-", collections.Counter(), collections.Counter(), {}
        source = text.splitlines()
        for number, line in enumerate(source, start=1):
            found = DEFINITION.match(line)
            if found:
                names[found.group(1)] += 1
                again = names[found.group(1)]
                definition = found.group(1) if again == 1 else f"{found.group(1)}#{again}"
                seen = collections.Counter()
            seen[line.strip()] += 1
            place[number] = (definition, seen[line.strip()])
        for number in sorted(n for n, counts in hits.items() if min(counts) == 0):
            some = "some" if max(hits[number]) > 0 else "none"
            rows.append((name, number, *place[number], some, source[number - 1].strip()))
    return rows, with_point


def main() -> int:
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    rows, with_point = unexecuted(args[0], pathlib.Path(args[1]))
    if "--check" not in sys.argv:
        for name, number, definition, which, some, text in rows:
            print(f"{name}\t{number}\t{definition}\t{which}\t{some}\t{text}")
        return 0
    found = {(name, definition, str(which), text): some for name, _, definition, which, some, text in rows}
    if len(found) != len(rows):
        print("two unexecuted lines have the same file, definition, which and text: the key does not tell them apart")
        return 1
    recorded = {}
    for line in (HERE / "expected" / "unexecuted-lines.tsv").read_text().splitlines():
        if not line or line.startswith("#"):
            continue
        name, definition, which, some, text, reason = line.split("\t")
        if not reason.strip():
            print(f"no reason: {name}\t{definition}\t{which}\t{text}")
            return 1
        if (name, definition, which, text) in recorded:
            print(f"two rows for one line: {name}\t{definition}\t{which}\t{text}")
            return 1
        recorded[(name, definition, which, text)] = some
    no_reason = sorted(set(found) - set(recorded))
    stale = sorted(set(recorded) - set(found))
    moved = sorted(key for key in set(found) & set(recorded) if found[key] != recorded[key])
    for key in no_reason:
        print("not executed, and no row says why: " + "\t".join(key))
    for key in stale:
        print("on the record, but executed or gone: " + "\t".join(key))
    for key in moved:
        print(f"the record says `{recorded[key]}` of the line's points are executed, measured `{found[key]}`: " + "\t".join(key))
    print(f"lines with a point {with_point}, not executed in full {len(found)} "
          f"({100 * (with_point - len(found)) / with_point:.2f}% executed), with a reason {len(set(found) & set(recorded))}, "
          f"without {len(no_reason)}, stale rows {len(stale)}, rows whose points moved {len(moved)}")
    return 1 if no_reason or stale or moved else 0


sys.exit(main())
