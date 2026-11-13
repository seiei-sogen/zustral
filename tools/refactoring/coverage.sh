#!/usr/bin/env bash
# Measures how much of the compiler the corpus of equivalence.py executes.
# A line the corpus never runs is a line a refactoring can break unnoticed.
#   coverage.sh              compare the per-file summary with expected/coverage.txt
#   coverage.sh --record     rewrite expected/coverage.txt
#   coverage.sh --lines F    print the line numbers of lib/F that no program executes
#   coverage.sh --errors     list the places that raise an error to the user and that no
#                            program executes
#   coverage.sh --errors --only DIR
#                            the same, running only the programs under DIR
#                            (a directory of test-programs/suites)
#   coverage.sh --messages   list the lines of error wording (lib/*Errors.ml) that no
#                            program executes, and fail if one is not on the record,
#                            expected/unexecuted-message-lines.txt
#   coverage.sh --messages --record
#                            rewrite that record
#   coverage.sh --unexecuted list every line that no program executes: file, line,
#                            definition, text
#   coverage.sh --unexecuted --check
#                            fail if one of them has no reason on the record,
#                            expected/unexecuted-lines.tsv, or if a row of the
#                            record is executed or gone
#   coverage.sh --unexecuted --only DIR
#                            the list when only the programs under DIR run (to see
#                            which lines a new test executes)
#   coverage.sh --kinds      count the lines that no program executes, by kind (an
#                            arm that raises an internal error, a line of or-pattern
#                            alternatives, ...); --kinds --files adds the count of
#                            each file. The kinds are read off the text: rough
# Environment:
#   ZUSTRAL_NIX_FLAGS  extra flags for `nix develop`
#
# The instrumented compiler is built in _build_coverage, beside the normal build.
set -uo pipefail

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
ROOT=$(cd "$HERE/../.." && pwd)
read -r -a NIX_FLAGS <<< "${ZUSTRAL_NIX_FLAGS:-}"
MODE=compare FILE=
ONLY=
RECORD_MESSAGES=()
case ${1:-} in
  --record) MODE=record ;;
  --lines) MODE=lines; FILE=${2:?usage: coverage.sh --lines File.ml} ;;
  --errors) MODE=errors; [[ ${2:-} == --only ]] && ONLY=${3:?usage: coverage.sh --errors --only DIR} ;;
  --messages) MODE=messages; [[ ${2:-} == --record ]] && RECORD_MESSAGES=(--record) ;;
  --kinds) MODE=kinds; KINDS_FILES=(); [[ ${2:-} == --files ]] && KINDS_FILES=(--files) ;;
  --unexecuted)
    MODE=unexecuted; UNEXECUTED_CHECK=()
    [[ ${2:-} == --check ]] && UNEXECUTED_CHECK=(--check)
    [[ ${2:-} == --only ]] && ONLY=${3:?usage: coverage.sh --unexecuted --only DIR} ;;
esac

T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT
mkdir -p "$T/cov"
cd "$ROOT" || exit 2

# Inside a nix shell already (CI runs under nix-shell), run the command as it is.
in_shell() {
  if [[ -n ${IN_NIX_SHELL:-} ]]; then bash -c "$1"; else nix develop "$ROOT" "${NIX_FLAGS[@]}" --command bash -c "$1"; fi
}

in_shell 'python3 concat_builtins.py && dune build --root . --build-dir _build_coverage --instrument-with bisect_ppx ./bin/austral.exe 2>&1' > "$T/build.log" 2>&1 \
  || { echo "instrumented build failed: $(tail -n 8 "$T/build.log")" >&2; exit 2; }

ZUSTRAL_EQUIVALENCE_ONLY="$ONLY" BISECT_FILE="$T/cov/bisect" python3 "$HERE/equivalence.py" snapshot "$ROOT/_build_coverage/default/bin/austral.exe" "$T/snapshot" > "$T/snapshot.log" 2>&1 \
  || { echo "corpus run failed: $(tail -n 5 "$T/snapshot.log")" >&2; exit 2; }

# The hits of every line, as XML. The report names the generated files too
# (the parser, the lexer), which are in the instrumented build.
line_report() {
  in_shell "bisect-ppx-report cobertura --source-path _build_coverage/default --coverage-path '$T/cov' '$T/cobertura.xml'" > /dev/null 2> "$T/report.err" \
    || { echo "report failed: $(tail -n 5 "$T/report.err")" >&2; exit 2; }
}

# Every point of every file with its hits, in one file. A line can hold several
# points, and the line report keeps one number per line.
point_report() {
  in_shell "bisect-ppx-report merge --coverage-path '$T/cov' '$T/points.coverage'" > /dev/null 2> "$T/report.err" \
    || { echo "report failed: $(tail -n 5 "$T/report.err")" >&2; exit 2; }
}

if [[ $MODE == errors ]]; then
  point_report
  python3 "$HERE/unexecuted-errors.py" "$T/points.coverage"
  exit $?
fi

if [[ $MODE == messages ]]; then
  point_report
  python3 "$HERE/unexecuted-errors.py" --messages "${RECORD_MESSAGES[@]}" "$T/points.coverage"
  exit $?
fi

if [[ $MODE == unexecuted ]]; then
  line_report
  python3 "$HERE/unexecuted-lines.py" "${UNEXECUTED_CHECK[@]}" "$T/cobertura.xml" "$ROOT"
  exit $?
fi

if [[ $MODE == kinds ]]; then
  line_report
  python3 "$HERE/unexecuted-kinds.py" "$T/cobertura.xml" "$ROOT" "${KINDS_FILES[@]}"
  exit $?
fi

if [[ $MODE == lines ]]; then
  line_report
  python3 - "$T/cobertura.xml" "$FILE" <<'PY'
import sys, xml.etree.ElementTree as ET
tree, wanted = ET.parse(sys.argv[1]), sys.argv[2]
for cls in tree.iter("class"):
    if cls.get("filename", "").endswith("/" + wanted) or cls.get("filename") == wanted:
        missed = [int(l.get("number")) for l in cls.iter("line") if l.get("hits") == "0"]
        print(" ".join(map(str, sorted(set(missed)))))
        break
else:
    sys.exit(f"{wanted}: not in the coverage report")
PY
  exit $?
fi

in_shell "bisect-ppx-report summary --per-file --coverage-path '$T/cov'" > "$T/report.txt" 2> "$T/report.err" \
  || { echo "report failed: $(tail -n 5 "$T/report.err")" >&2; exit 2; }

# The report's own total counts the generated parser and built-in modules too.
# Add a total over the hand-written files, which is the number the plan tracks.
summarize() {
  awk -F'\t' '
    $1 == "Project" { printf "total-with-generated\t%s\t%s\n", $2, $3; next }
    { split($3, a, "/"); hit += a[1]; all += a[2]; print }
    END { printf "total-hand-written\t%.2f\t%d/%d\n", 100 * hit / all, hit, all }'
}

# Lines look like " 87.50 %   14/16   lib/Foo.ml"; the generated modules are left out.
grep -E '%' "$T/report.txt" | grep -v -E 'lib/(BuiltInModules|Parser|Lexer)\.ml$' \
  | awk '{ pct = $1; frac = $3; file = $4; if (file == "") file = "total"; printf "%s\t%s\t%s\n", file, pct, frac }' \
  | sort | summarize > "$T/coverage.txt"

if [[ $MODE == record ]]; then
  cp "$T/coverage.txt" "$HERE/expected/coverage.txt"; echo "recorded coverage.txt: $(tail -n 1 "$T/coverage.txt")"
elif diff -u "$HERE/expected/coverage.txt" "$T/coverage.txt"; then
  echo "match coverage.txt: $(tail -n 1 "$T/coverage.txt")"
else
  echo "MISMATCH coverage.txt"; exit 1
fi
