#!/usr/bin/env python3
"""Lists what no program executed of the errors to the user: the places that
raise one, or the lines of their wording.

  unexecuted-errors.py COVERAGE_FILE                       the places
  unexecuted-errors.py --messages COVERAGE_FILE            the wording, compared with
                                                           expected/unexecuted-message-lines.txt
  unexecuted-errors.py --messages --record COVERAGE_FILE   rewrite that file

Called by coverage.sh --errors and coverage.sh --messages with the file
`bisect-ppx-report merge` writes: for every source file, the offset of each
point and how often it was hit.

The places

A place is a call of one of the functions that raise an error of a category
(`type_error`, `cli_error`, and so on; lib/Error.mli lists them).
`internal_err` is left out: it marks a state the code believes cannot happen.

The nearest point at or before the start of the call, in the same top-level
definition, decides whether the place was executed. Where the call is all of
an `if` branch or of a function body, that is a point on the call itself.
Where it is all of a match arm, it is the arm's point, which the tool puts on
the pattern. Points after the start of the call say nothing: the tool puts
one at the end of the name of a function it expects to return, and no program
gets past a call that raises.

The places of the lexer are in lib/Lexer.mll, but the points are in the file
ocamllex generates from it. The generated file (in the instrumented build,
_build_coverage) says which of its lines come from which line of the .mll,
and the report names the line of the .mll.

Prints one line per unexecuted place: file, line, the enclosing top-level
definition and the constructor of the error. The last line gives the totals.

The wording

All of it is in lib/*Errors.ml: the `render` function of each category, and
the functions `render` calls for the words of an enumeration. That code runs
only when an error is printed, so a line of it that no program executed is
wording that no test shows.

A line that carries points is executed if every one of them was hit: both
branches of an `if` written on one line have to run. A line without a point
(most wording is a literal in a list) goes with the nearest earlier line that
carries one and is indented less, in the same top-level definition: the match
arm it belongs to. Lines outside any arm or function body (types, `open`) are
not wording and are left out. So is a line that calls a function that never
returns: the point after such a call is never hit, and what the line says is
not wording.

Prints the unexecuted lines as file, tab, the line without its indentation.
Line numbers shift, the text of a line does not. The same text can occur
several times in a file (`| None ->`), so the comparison counts: a line that
is unexecuted more times than the record says is new. Issue #124 gives, for
each recorded line, why no program reaches it.
"""
import bisect
import collections
import pathlib
import re
import sys

HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE.parents[1]
RECORDED = HERE / "expected" / "unexecuted-message-lines.txt"
BUILD = ROOT / "_build_coverage" / "default"
# A definition at the top of an OCaml file. A lexer also defines its rules.
DEFINITION = re.compile(r"^(?:let rec|let|and|rule) ([a-z_][A-Za-z0-9_']*)", re.M)
PLACE = re.compile(r"(?<![_a-zA-Z])%s\b" % r"(?:parse|cli|type|linearity|declaration|entrypoint|generic)_error(?:_at)?")
NEVER_RETURNS = re.compile(r"(?<![_a-zA-Z])(?:raise|failwith|invalid_arg|internal_err|exit|assert false)\b")
# The constructor of the error: the first capitalised word that is not a module's name.
CONSTRUCTOR = re.compile(r"\b[A-Z][A-Za-z0-9_]*\b(?!\.)")
# ocamllex writes `# 159 "lib/Lexer.mll"` before the lines it copies from there.
LINE_DIRECTIVE = re.compile(r'^# (\d+) "([^"]+)"')


def load_points(coverage_file: str) -> dict[str, list[tuple[int, int]]]:
    """Per source file, its points in the order of the text: (offset, hits)."""
    words = iter(pathlib.Path(coverage_file).read_bytes().split(b" "))
    if next(words) != b"BISECT-COVERAGE-4":
        sys.exit(f"{coverage_file}: not a file written by bisect-ppx-report merge")
    points = {}
    for _ in range(int(next(words))):
        next(words)  # the length of the name; no source file has a space in its name
        name = next(words).decode()
        offsets = [int(next(words)) for _ in range(int(next(words)))]
        counts = [int(next(words)) for _ in range(int(next(words)))]
        points[name] = sorted(zip(offsets, counts))
    return points


def read(path: pathlib.Path) -> str:
    """The text of a source file, one character per byte: the points are byte offsets."""
    return path.read_text(encoding="latin-1")


def without_comments(text: str) -> str:
    """The text with comments blanked out. Every character keeps its offset."""
    chars, depth, i = list(text), 0, 0
    while i < len(chars):
        if text.startswith("(*", i):
            depth += 1
            chars[i] = chars[i + 1] = " "
            i += 2
        elif depth and text.startswith("*)", i):
            depth -= 1
            chars[i] = chars[i + 1] = " "
            i += 2
        elif text[i] == '"' and not depth:
            i += 1
            while text[i] != '"':
                i += 2 if text[i] == "\\" else 1
            i += 1
        else:
            if depth and chars[i] != "\n":
                chars[i] = " "
            i += 1
    return "".join(chars)


def line_of(text: str, offset: int) -> int:
    return text.count("\n", 0, offset) + 1


def definitions(text: str) -> list[tuple[int, str]]:
    """The top-level definitions of a file: (offset, name)."""
    return [(m.start(), m.group(1)) for m in DEFINITION.finditer(text)]


def enclosing(starts: list[tuple[int, str]], offset: int) -> tuple[int, str]:
    before = [start for start in starts if start[0] <= offset]
    return before[-1] if before else (0, "?")


def executed(points: list[tuple[int, int]], call: int, definition: int) -> bool:
    """Whether the call at this offset ran. Its definition starts at `definition`."""
    nearest = bisect.bisect_right(points, (call, float("inf"))) - 1
    return nearest >= 0 and points[nearest][0] >= definition and points[nearest][1] > 0


def origins(generated: str, source: str) -> dict[int, int]:
    """For a generated file: its lines that are copied from `source`, with the line there."""
    origin: dict[int, int] = {}
    current = None
    for number, line in enumerate(generated.split("\n"), start=1):
        directive = LINE_DIRECTIVE.match(line)
        if directive:
            current = int(directive.group(1)) if directive.group(2) == source else None
        elif current is not None:
            origin[number] = current
            current += 1
    return origin


def places(points: dict[str, list[tuple[int, int]]]):
    """Every place: (file, line, definition, the text after the function's name, whether it ran)."""
    for path in sorted([*(ROOT / "lib").glob("*.ml"), *(ROOT / "cli").glob("*.ml")]):
        if path.name in ("BuiltInModules.ml", "Error.ml"):  # Error.ml defines the functions
            continue
        name = str(path.relative_to(ROOT))
        text = read(path)
        code = without_comments(text)
        starts = definitions(code)
        for place in PLACE.finditer(code):
            first, definition = enclosing(starts, place.start())
            ran = executed(points.get(name, []), place.start(), first)
            yield name, line_of(text, place.start()), definition, text[place.end():place.end() + 200], ran
    for path in sorted((ROOT / "lib").glob("*.mll")):
        name = str(path.relative_to(ROOT))
        generated_path = BUILD / "lib" / (path.stem + ".ml")
        if not generated_path.exists():
            sys.exit(f"{generated_path}: not built; coverage.sh builds it")
        generated = read(generated_path)
        origin = origins(generated, name)
        code, source = without_comments(generated), without_comments(read(path))
        starts, named = definitions(code), definitions(source)
        line_starts = [0] + [m.end() for m in re.finditer("\n", source)]
        for place in PLACE.finditer(code):
            line = origin.get(line_of(generated, place.start()))
            if line is None:
                continue  # not copied from the .mll
            first, _ = enclosing(starts, place.start())
            _, definition = enclosing(named, line_starts[line - 1])
            ran = executed(points.get(str(generated_path.relative_to(BUILD)), []), place.start(), first)
            yield name, line, definition, generated[place.end():place.end() + 200], ran


def report_places(points: dict[str, list[tuple[int, int]]]) -> int:
    total = unexecuted = 0
    for name, line, definition, rest, ran in places(points):
        total += 1
        if not ran:
            unexecuted += 1
            constructor = CONSTRUCTOR.search(rest)
            print(f"{name}\t{line}\t{definition}\t{constructor.group(0) if constructor else ''}")
    print(f"error places {total}, unexecuted {unexecuted}")
    return 0


def indentation(line: str) -> int:
    return len(line) - len(line.lstrip())


def message_lines(points: dict[str, list[tuple[int, int]]]):
    """Every line of wording: (file, the line without its indentation, whether it ran)."""
    for path in sorted((ROOT / "lib").glob("*Errors.ml")):
        name = str(path.relative_to(ROOT))
        text = read(path)
        original, code = text.split("\n"), without_comments(text).split("\n")
        hits: list[list[int]] = [[] for _ in code]  # per line, the hits of its points
        for offset, count in points.get(name, []):
            hits[line_of(text, offset) - 1].append(count)
        for index, line in enumerate(code):
            if not re.search(r"\w", line) or PLACE.search(line) or NEVER_RETURNS.search(line):
                continue
            if hits[index]:
                ran = all(hits[index])
            else:
                ran = None
                for earlier in range(index - 1, -1, -1):
                    if hits[earlier] and indentation(code[earlier]) < indentation(line):
                        ran = any(hits[earlier])
                        break
                    if code[earlier].strip() and indentation(code[earlier]) == 0:
                        break  # the first line of the top-level definition
            if ran is not None:
                yield name, original[index].strip().encode("latin-1").decode(), ran


def report_messages(points: dict[str, list[tuple[int, int]]], record: bool) -> int:
    rows = list(message_lines(points))
    unexecuted = [f"{name}\t{line}" for name, line, ran in rows if not ran]
    summary = f"message lines {len(rows)}, unexecuted {len(unexecuted)}"
    if record:
        RECORDED.write_text("".join(row + "\n" for row in unexecuted))
        print(f"recorded {RECORDED.name}: {summary}")
        return 0
    now = collections.Counter(unexecuted)
    recorded = collections.Counter(RECORDED.read_text().splitlines()) if RECORDED.exists() else collections.Counter()
    new, gone = now - recorded, recorded - now
    for row in new.elements():
        print(f"NEW   {row}")
    for row in gone.elements():
        print(f"GONE  {row}")
    if new:
        print(f"{sum(new.values())} lines of wording are executed by no test. Write a test, or record why none can exist.")
        return 1
    if gone:
        print("lines on the record are now executed or deleted; run coverage.sh --messages --record")
        return 1
    print(f"match {RECORDED.name}: {summary}")
    return 0


arguments = [argument for argument in sys.argv[1:] if not argument.startswith("--")]
if len(arguments) != 1:
    sys.exit(__doc__)
if "--messages" in sys.argv:
    sys.exit(report_messages(load_points(arguments[0]), "--record" in sys.argv))
sys.exit(report_places(load_points(arguments[0])))
