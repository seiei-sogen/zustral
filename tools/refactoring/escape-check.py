#!/usr/bin/env python3
"""Checks that no error leaves a stage of the compiler as an exception.

  escape-check.py STAGE...     check the named stage modules (lib/STAGE.ml)
  escape-check.py              check every module the driver names, and the driver
  escape-check.py --self-test  only check the analysis against the seeded
                               escapes of samples/escape/ (every other run does
                               this first, and stops if a sample disagrees)

A stage reports its errors as a value (lib/Outcome.ml). Inside a stage an
error travels as the exception `Austral_error`, and the functions the driver
(lib/Compiler.ml) calls are where `Outcome.catch` turns it into a value. No
type says that such a function catches everything, so this check looks. It
reads the sources as text: it does not parse them, and it knows no types.

What it sees:
- A definition is a `let name`, `let rec name` or `and name` that no `in`
  closes, at any indentation and inside any `struct ... end` of the file. Its
  text runs to the next definition, so its header may take several lines.
  The definitions of one name in one file count as one function. A module
  written inside a function (under a local `let`, or packed as a value) is
  part of that function's text.
- A function can raise if its text has `raise`, `raise_notrace` or
  `raise_with_backtrace`, or names a function that can raise, outside the
  parentheses of an `Outcome.catch (fun ...)`. Every `raise` in the compiler
  raises `Austral_error`; the functions that build an error (`type_error`,
  `internal_err`, ...) are found through it.
- Names are matched as words, whatever module path is written before them. A
  name stands for the functions of that name in the same file and in every
  module the file mentions, whether or not a local variable hides them.
- The lexer and the parser are not OCaml files, so a function that names one
  of those two modules (`Lexer.token`) counts as raising if anything in their
  sources does.
Apart from the limits below, the answer is an upper estimate: a function it
clears does not raise `Austral_error`, and one it reports may be harmless.

What it does not see (samples/escape/NotSeen.ml has one of each, and nothing
checks that the compiler has none):
- A function that is called through a value: a closure that an
  `Outcome.catch` returns, a field of a record, the content of a reference, a
  module given to a functor. The call names the value, not the function.
- A name that is not bound by `let name`, `let rec name` or `and name`: an
  operator (`let ( +! ) ...`), the names of a pattern (`let (check, report)
  = ...`), a `let` with an attribute. Its text counts for the definition
  before it, and its callers are not followed.
- A function reached through another module's `include` or module alias: the
  calling file does not mention the module that defines it.

Two things follow for the code. A local function that raises has to be
defined inside the `Outcome.catch (...)` that calls it, or at the top level.
And the argument of `Outcome.catch` has to be a `fun` written in parentheses:
any other argument is run before `catch` is called, so its text counts as
outside.

Other exceptions (`Failure`, `Not_found`, ...) are not this check's concern:
nothing in the compiler catches them (issue #85).

Exits 1 if an error can escape, 2 if the check cannot answer: a stage that
the driver does not call, a function with no definition, or a sample of
samples/escape/ that the analysis gets wrong.
"""
import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
DRIVER = "Compiler"
SAMPLES = pathlib.Path(__file__).resolve().parent / "samples" / "escape"
# The three ways the standard library raises. raise_error and the like are
# functions of the compiler: found through the `raise` in their text.
RAISE = re.compile(r"(?<![A-Za-z0-9_'])raise(?:_notrace|_with_backtrace)?(?![A-Za-z0-9_'])")
# Sources that ocamllex and menhir turn into modules of the same name.
GENERATED = ("Lexer.mll", "Parser.mly")
# Made from lib/builtin/ by concat_builtins.py: string constants only.
CONSTANTS = "BuiltInModules"
BINDING_WORD = re.compile(r"(?<![A-Za-z0-9_'])(let|and|in|struct|sig|object|begin|end)(?![A-Za-z0-9_'])")
# `( let* )` names an operator: the `let` in it opens nothing.
OPERATOR_NAME = re.compile(r"[!$%&*+\-./:<=>?@^|~]+\s*\)")
# A module as a value, `(module struct ... end)`: the only `struct` that can
# stand in a function with no local `let` around it.
PACKED = re.compile(r"\(\s*module\s+$")
BOUND_NAME = re.compile(r"\s+(?:rec\s+)?(?!(?:type|module)\b)([a-z_][A-Za-z0-9_']*)")
# Only a `fun` is run by `catch`: any other argument is run before the call.
CATCH = re.compile(r"\bOutcome\.catch\s*\((?=\s*fun\b)")
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


def definition_starts(text: str) -> list[tuple[int, int, str]]:
    """Where each definition of a name starts, where its name ends, and the
    name. A `let` that an `in` closes is local; one that no `in` closes is a
    definition, at whatever depth of `struct ... end` it stands. An `and`
    belongs to the nearest `let` still open. A module written under a local
    `let`, or packed as a value, is part of the function around it."""
    words: list[tuple[re.Match, int | None]] = []  # a `let`, or an `and` with the index of its `let`
    closes: dict[int, int] = {}  # the place of the `in` of every local `let`
    packed: list[tuple[int, int]] = []  # from the `struct` to the `end` of every packed module
    open_lets: list[int | None] = []  # None is where a `struct ... end` (or the like) begins
    open_structs: list[int | None] = []  # for each None above: where it begins, if it is packed
    for m in BINDING_WORD.finditer(text):
        keyword = m.group(1)
        if OPERATOR_NAME.match(text, m.end()):
            continue
        if keyword == "let":
            open_lets.append(len(words))
            words.append((m, None))
        elif keyword == "and":
            words.append((m, open_lets[-1] if open_lets else None))
        elif keyword == "in":
            if open_lets and open_lets[-1] is not None:
                closes[open_lets.pop()] = m.start()
        elif keyword == "end":
            # What is still open in a `struct` when it ends is a definition of that module.
            while open_lets:
                if open_lets.pop() is None:
                    if (start := open_structs.pop()) is not None:
                        packed.append((start, m.start()))
                    break
        else:
            open_lets.append(None)
            open_structs.append(m.start() if keyword == "struct" and PACKED.search(text, max(0, m.start() - 80), m.start()) else None)
    packed.sort(reverse=True)
    starts, local_until = [], 0
    for index, (m, owner) in enumerate(words):
        while packed and packed[-1][0] < m.start():
            local_until = max(local_until, packed.pop()[1])
        if index in closes:
            local_until = max(local_until, closes[index])
        elif owner not in closes and m.start() >= local_until and (name := BOUND_NAME.match(text, m.end())):
            starts.append((m.start(), name.end(), name.group(1)))
    return starts


def definitions(code: dict[str, str]) -> dict[Key, str]:
    """The part of every function that an exception can leave through."""
    bodies: dict[Key, str] = {}
    for module, text in code.items():
        starts = definition_starts(text)
        for (_, body_start, name), (end, _, _) in zip(starts, starts[1:] + [(len(text), 0, "")]):
            # Two definitions of one name are one function: either may be the one called.
            bodies[(module, name)] = bodies.get((module, name), "") + " " + outside_catch(text[body_start:end])
    return bodies


def raising(code: dict[str, str], bodies: dict[Key, str]) -> dict[Key, Key | None]:
    """Every function that can raise, with the function it names that makes it
    so, or None if it has a `raise` itself. A generated module is a function
    of no name."""
    # The modules whose functions a file can call: itself and those it mentions.
    mentions = {module: {module} | (set(re.findall(r"\b[A-Z][A-Za-z0-9_]*\b", text)) & code.keys()) for module, text in code.items()}
    generated = [module for file in GENERATED if (module := file.split(".")[0]) in code]
    texts = dict(bodies) | {(module, ""): code[module] for module in generated}
    why: dict[Key, Key | None] = {key: None for key, text in texts.items() if RAISE.search(text)}
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


VERDICTS = ("escapes", "clear", "missed")


def self_test() -> tuple[bool, str]:
    """Whether the analysis says of samples/escape/ what its files say of
    themselves, and the lines to print. The files are OCaml that is never
    compiled, analysed together the way lib/ and cli/ are. The first comment
    of each names its functions under a verdict: `escapes` (can raise, and the
    check must say so), `clear` (cannot, and the check must not say so) and
    `missed` (can raise, and the check does not see it: the limits the text
    above lists, kept here so that it stays true)."""
    files = sorted(SAMPLES.glob("*.ml"))
    code = {path.stem: code_only(path.read_text()) for path in files}
    bodies = definitions(code)
    why = raising(code, bodies)
    lines, functions = [], 0
    for path in files:
        header = path.read_text().split("*)")[0]
        expected = {verdict: re.findall(r"[a-z_][A-Za-z0-9_']*", names) for verdict, names in re.findall(r"\b(\w+):([^:\n]*)$", header, re.M)}
        if not expected or expected.keys() - set(VERDICTS):
            lines.append(f"{path.name}: the first comment must list functions after {', '.join(f'`{v}:`' for v in VERDICTS)}")
        for verdict, names in expected.items():
            for name in names:
                functions += 1
                key = (path.stem, name)
                got = "no definition found" if key not in bodies else "escapes" if key in why else "clear"
                # A limit that is lifted has to leave the list of limits too.
                want = "clear" if verdict == "missed" else verdict
                if got != want:
                    lines.append(f"{path.name}: {name}: expected {verdict}, the check says: {got}")
    if not files:
        lines.append(f"no samples in {SAMPLES}")
    if lines:
        return False, "\n".join([*lines, "the analysis disagrees with samples/escape/"])
    return True, f"self-test: {len(files)} samples, {functions} functions, all as expected"


def main() -> int:
    passed, message = self_test()
    if sys.argv[1:] == ["--self-test"] or not passed:
        print(message)
        return 0 if passed else 2
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
