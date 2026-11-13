#!/usr/bin/env python3
"""Checks that no error leaves a stage of the compiler as an exception.

  escape-check.py STAGE...   check the named stage modules (lib/STAGE.ml)
  escape-check.py            check every module the driver names, and the driver

A stage reports its errors as a value (lib/Outcome.ml). Inside a stage an
error travels as the exception `Austral_error`, and the functions the driver
(lib/Compiler.ml) calls are where `Outcome.catch` turns it into a value. No
type says that such a function catches everything, so this check looks.

A function can raise if its text has a `raise`, or names a function that can
raise, outside the parentheses of an `Outcome.catch (...)`. Every `raise` in
the compiler raises `Austral_error`; the functions that build an error
(`type_error`, `internal_err`, ...) are found through it. Names are matched as
words. A name stands for the top-level functions of that name in the same file
and in every module the file mentions, whether or not a local variable hides
them: the answer is an upper estimate, and a function it clears cannot raise
`Austral_error`. The lexer and the parser are not OCaml files, so a function
that mentions one of those two modules counts as raising if anything in their
sources does.

Two things follow for the code. A local function that raises has to be
defined inside the `Outcome.catch (...)` that calls it, or at the top level.
And the argument of `Outcome.catch` has to be written in parentheses.

Other exceptions (`Failure`, `Not_found`, ...) are not this check's concern:
nothing in the compiler catches them (issue #85).

Exits 1 if an error can escape, 2 if the question makes no sense (a stage that
the driver does not call).
"""
import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
DRIVER = "Compiler"
RAISE = re.compile(r"(?<![A-Za-z0-9_'])raise(?![A-Za-z0-9_'])")
# Sources that ocamllex and menhir turn into modules of the same name.
GENERATED = ("Lexer.mll", "Parser.mly")
# Made from lib/builtin/ by concat_builtins.py: string constants only.
CONSTANTS = "BuiltInModules"
DEFINITION = re.compile(r"^(?:let rec|let|and) ([a-z_][A-Za-z0-9_']*)[^=\n]*=", re.M)
CATCH = re.compile(r"\bOutcome\.catch\s*\(")
CHAR_LITERAL = re.compile(r"'(?:[^\\'\n]|\\(?:[\\\"'ntbr ]|[0-9]{3}|x[0-9a-fA-F]{2}|o[0-3][0-7]{2}))'")


def code_only(text: str) -> str:
    """The text with comments, string literals and character literals blanked
    out. Everything else keeps its place."""
    chars, i, depth = list(text), 0, 0
    def blank(start: int, end: int) -> None:
        for k in range(start, end):
            if chars[k] != "\n":
                chars[k] = " "
    while i < len(text):
        if text.startswith("(*", i):
            start, depth, i = i, 1, i + 2
            while i < len(text) and depth:
                if text.startswith("(*", i):
                    depth, i = depth + 1, i + 2
                elif text.startswith("*)", i):
                    depth, i = depth - 1, i + 2
                else:
                    i += 1
            blank(start, i)
        elif text[i] == '"':
            start, i = i, i + 1
            while i < len(text) and text[i] != '"':
                i += 2 if text[i] == "\\" else 1
            i += 1
            blank(start, i)
        elif text[i] == "'" and (m := CHAR_LITERAL.match(text, i)):
            blank(m.start(), m.end())
            i = m.end()
        else:
            i += 1
    return "".join(chars)


def outside_catch(body: str) -> str:
    """The body without the arguments of its calls to `Outcome.catch`."""
    kept, at = [], 0
    while m := CATCH.search(body, at):
        kept.append(body[at:m.start()])
        depth, i = 1, m.end()
        while i < len(body) and depth:
            depth += {"(": 1, ")": -1}.get(body[i], 0)
            i += 1
        at = i
    kept.append(body[at:])
    return " ".join(kept)


Key = tuple[str, str]  # module, function


def word(names) -> re.Pattern:
    alternatives = "|".join(sorted(map(re.escape, names), key=len, reverse=True))
    return re.compile(r"(?<![A-Za-z0-9_'.])(?:[A-Z][A-Za-z0-9_]*\.)*(" + alternatives + r")(?![A-Za-z0-9_'])")


def sources() -> dict[str, str]:
    """The code of every module of the compiler that is written by hand."""
    files = [*(ROOT / "lib").glob("*.ml"), *(ROOT / "cli").glob("*.ml")]
    code = {path.stem: code_only(path.read_text()) for path in sorted(files) if path.stem != CONSTANTS}
    # The rules of the lexer and the parser are not OCaml: taken as they are.
    code.update({file.split(".")[0]: (ROOT / "lib" / file).read_text() for file in GENERATED})
    return code


def definitions(code: dict[str, str]) -> dict[Key, str]:
    """The part of every top-level function that an exception can leave through."""
    bodies = {}
    for module, text in code.items():
        starts = [(m.start(), m.end(), m.group(1)) for m in DEFINITION.finditer(text)]
        for (_, body_start, name), (end, _, _) in zip(starts, starts[1:] + [(len(text), 0, "")]):
            bodies[(module, name)] = outside_catch(text[body_start:end])
    return bodies


def raising(code: dict[str, str], bodies: dict[Key, str]) -> dict[Key, Key | None]:
    """Every function that can raise, with the function it names that makes it
    so, or None if it has a `raise` itself. A generated module is a function
    of no name."""
    # The modules whose functions a file can call: itself and those it mentions.
    mentions = {module: {module} | (set(re.findall(r"\b[A-Z][A-Za-z0-9_]*\b", text)) & code.keys()) for module, text in code.items()}
    generated = [file.split(".")[0] for file in GENERATED]
    texts = dict(bodies) | {(module, ""): code[module] for module in generated}
    why: dict[Key, Key | None] = {key: None for key, body in bodies.items() if RAISE.search(body)}
    changed = True
    while changed:
        changed = False
        pattern = word({name for _, name in why if name})
        for key, text in texts.items():
            if key in why:
                continue
            functions = ((module, m.group(1)) for m in pattern.finditer(text) for module in mentions[key[0]])
            rules = ((module, "") for module in generated if re.search(rf"\b{module}\.", text))
            if reason := next((k for k in (*functions, *rules) if k in why and k != key), None):
                why[key] = reason
                changed = True
    return why


def chain(key: Key, why: dict[Key, Key | None]) -> str:
    """How the function comes to raise, as far as the names tell."""
    steps = [key[1]]
    while (key := why[key]) is not None:
        steps.append(".".join(filter(None, key)))
    return " -> ".join([*steps, "raise"])


def exported(module: str) -> list[str]:
    interface = ROOT / "lib" / f"{module}.mli"
    return re.findall(r"^val ([a-z_][A-Za-z0-9_']*)", interface.read_text(), re.M) if interface.exists() else []


def main() -> int:
    driver = code_only((ROOT / "lib" / f"{DRIVER}.ml").read_text())
    stages = sys.argv[1:]
    whole = not stages
    if whole:
        # Every module the driver opens or names, in the order of its first mention.
        named = re.findall(r"\b([A-Z][A-Za-z0-9_]*)\b", driver)
        stages = [m for m in dict.fromkeys(named) if m not in (DRIVER, CONSTANTS) and (ROOT / "lib" / f"{m}.ml").exists()]
    code = sources()
    bodies = definitions(code)
    why = raising(code, bodies)
    report: list[tuple[str, list[str]]] = []
    for module in stages:
        called = [name for name in exported(module) if word([name]).search(driver)]
        if not called:
            if whole:
                continue
            print(f"{module}: the driver calls none of its exported functions")
            return 2
        report.append((module, called))
    if whole:
        report.append((DRIVER, exported(DRIVER)))
    escaping = 0
    for module, names in report:
        # A function this check cannot find would pass without being looked at.
        if missing := [name for name in names if (module, name) not in bodies]:
            print(f"{module}: no definition found for {', '.join(missing)}")
            return 2
        escapes = [name for name in names if (module, name) in why]
        escaping += len(escapes)
        print(f"{module}: {', '.join(names)}: " + ("an error can escape" if escapes else "no error escapes"))
        for name in escapes:
            print(f"  {chain((module, name), why)}")
    print(f"modules {len(report)}, functions {sum(len(names) for _, names in report)}, escaping {escaping}")
    return 1 if escaping else 0


if __name__ == "__main__":
    sys.exit(main())
