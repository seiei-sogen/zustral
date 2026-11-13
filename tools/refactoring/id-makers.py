#!/usr/bin/env python3
"""Checks that only the environment makes IDs.

  id-makers.py              check lib/, cli/, bin/ and test/
  id-makers.py --self-test  only check the search against the seeded files of
                            samples/makers/ (every other run does this first,
                            and stops if a sample disagrees)

An ID hides its number (lib/Id.mli), but whoever holds a supply can make one:
`Id.fresh_function_id Supply.initial` makes an ID whose number the environment
of the compilation has already given out, and a lookup with it finds another
declaration. Inside one dune library OCaml cannot restrict a function to one
client module, so this check does. Only lib/Env.ml may name `Supply.initial`
or a function `fresh_<kind>_id`, apart from the modules that define them
(lib/Id.ml, lib/Supply.ml and their interfaces). A unit test goes through
`Env` too.

It reads the sources as text, without comments, string literals and character
literals. A name counts as a word whatever module path is written before it,
so `I.fresh_decl_id` after `module I = Id`, a bare `fresh_mono_id` after
`open Id` and `let make = Id.fresh_decl_id` are all seen, in the file that
writes them.

What it does not see (samples/makers/lib/NotSeen.ml): `initial` written
without `Supply.` before it, after `open Supply` or in `Supply.(initial)`.
The word alone is too common to look for. An ID made from that supply still
needs a `fresh_<kind>_id`, which is seen.

Exits 1 if a file outside the environment names a maker, 2 if a sample of
samples/makers/ disagrees with the search.
"""
import importlib.util
import pathlib
import re
import sys

HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE.parents[1]
SAMPLES = HERE / "samples" / "makers"
DIRECTORIES = ("lib", "cli", "bin", "test")
SUFFIXES = (".ml", ".mli", ".mll", ".mly")
ALLOWED = {"lib/Env.ml", "lib/Id.ml", "lib/Id.mli", "lib/Supply.ml", "lib/Supply.mli"}
# `fresh_` and `_id` around any kind, so that a kind added later is covered.
# `fresh_ident` (a temporary's name) and a variable `fresh_id` are not makers.
MAKER = re.compile(r"(?<![A-Za-z0-9_'])(Supply\s*\.\s*initial|fresh_[A-Za-z0-9_']+_id)(?![A-Za-z0-9_'])")
VERDICTS = ("flagged", "clear", "missed")

# The same blanking as the escape check, so that both read a file alike.
_spec = importlib.util.spec_from_file_location("escape_check", HERE / "escape-check.py")
_escape = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_escape)
code_only = _escape.code_only


def makers(root: pathlib.Path) -> dict[str, list[str]]:
    """The files under `root` that name a maker and may not, with the names."""
    found: dict[str, list[str]] = {}
    for directory in DIRECTORIES:
        for path in sorted((root / directory).rglob("*")):
            relative = path.relative_to(root).as_posix()
            if path.suffix not in SUFFIXES or relative in ALLOWED:
                continue
            names = {re.sub(r"\s+", "", m) for m in MAKER.findall(code_only(path.read_text()))}
            if names:
                found[relative] = sorted(names)
    return found


def self_test() -> tuple[bool, str]:
    """Whether the search says of samples/makers/ what its files say of
    themselves. The directory is a small tree laid out like the repository.
    The first comment of each file holds one verdict: `flagged:` and the names
    the search must report, `clear:` (the file may name them, or names none)
    or `missed:` (the file makes an ID and the search does not see it: the
    limit the text above lists, kept here so that it stays true)."""
    found = makers(SAMPLES)
    files = sorted(p for d in DIRECTORIES for p in (SAMPLES / d).rglob("*") if p.suffix in SUFFIXES)
    lines = []
    for path in files:
        relative = path.relative_to(SAMPLES).as_posix()
        header = re.match(r"\(\*\s*(\w+):([^*]*)\*\)", path.read_text())
        if not header or header[1] not in VERDICTS:
            lines.append(f"{relative}: the first comment must be one of {', '.join(f'`{v}:`' for v in VERDICTS)}")
            continue
        want = header[2].split() if header[1] == "flagged" else []
        got = found.get(relative, [])
        if sorted(want) != got:
            lines.append(f"{relative}: expected {header[1]} {' '.join(want)}, the search says: {' '.join(got) or 'clear'}")
    if not files:
        lines.append(f"no samples in {SAMPLES}")
    if lines:
        return False, "\n".join([*lines, "the search disagrees with samples/makers/"])
    return True, f"self-test: {len(files)} samples, all as expected"


def main() -> int:
    passed, message = self_test()
    if sys.argv[1:] == ["--self-test"] or not passed:
        print(message)
        return 0 if passed else 2
    found = makers(ROOT)
    for relative, names in found.items():
        print(f"{relative}: {', '.join(names)}")
    if found:
        print(f"{len(found)} files make IDs; only lib/Env.ml may (it holds the supply of the compilation)")
        return 1
    print("only lib/Env.ml makes IDs")
    return 0


if __name__ == "__main__":
    sys.exit(main())
