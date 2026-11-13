#!/usr/bin/env bash
# Counts, per source file of the compiler, the things the refactoring plan
# wants to shrink. See docs/research/functional-domain-refactoring/README.md.
#   measure.sh            compare with expected/
#   measure.sh --record   rewrite expected/ from the current tree
#   measure.sh --fragile  also count fragile matches (needs a compiler build)
#   measure.sh --check-fragile  only the fragile matches: fail unless the totals
#                         equal expected/fragile-totals.txt and none is on a
#                         type of lib/Type.ml or on MonoType.mono_ty
#   measure.sh --self-test
#                         check the patterns of the columns io, names and state against
#                         the sample lines in samples/ (every other mode does
#                         this first, and stops if a sample disagrees)
#   measure.sh --tree DIR print the totals of the tree at DIR; nothing is compared
#                         or recorded
#
# The numbers of the time the plan was made (baseline/totals.txt) are counted
# with --tree, on the tree of commit befc0ad. That tree has no cli/: the edge
# was lib/Cli*.ml, so move those files to cli/ first, for coreio to mean the same.
#   git archive befc0ad lib bin | tar -x -C "$D"
#   mkdir "$D/cli" && mv "$D"/lib/Cli*.ml "$D/cli/"
#   measure.sh --tree "$D"
# Environment for --fragile:
#   ZUSTRAL_NIX_FLAGS      extra flags for `nix develop`
#   ZUSTRAL_BUILD_TIMEOUT  seconds allowed for the build (default 900)
set -uo pipefail

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
ROOT=$(cd "$HERE/../.." && pwd)
RECORD=0 FRAGILE=0 CHECK_FRAGILE=0 SELF_TEST=0 TREE=
while (($#)); do
  case $1 in
    --record) RECORD=1 ;;
    --fragile) FRAGILE=1 ;;
    --check-fragile) CHECK_FRAGILE=1 ;;
    --self-test) SELF_TEST=1 ;;
    --tree) TREE=${2:?usage: measure.sh --tree DIR}; shift ;;
    *) echo "unknown option: $1" >&2; exit 2 ;;
  esac
  shift
done
# The table of built-in names is always this checkout's, also when another tree is measured.
NAMES_TABLE=$ROOT/lib/BuiltInNames.ml
if [[ -n $TREE ]]; then ROOT=$(cd "$TREE" && pwd) || exit 2; fi

T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT

# The core is lib/, the command line is cli/. BuiltInModules.ml is generated
# from lib/builtin/ by concat_builtins.py.
sources() { (cd "$ROOT" && ls lib/*.ml lib/*.mll lib/*.mly cli/*.ml 2> /dev/null | grep -v '^lib/BuiltInModules\.ml$'); }

count() { # regex file
  grep -cE -- "$1" "$ROOT/$2" || true
}

# Lines inside a type or exception definition that use string as a field or a
# constructor argument. A definition starts at `type` or `exception` and ends
# at the next item that is not a type (`and` continues a type definition).
count_type_strings() { # file
  awk '
    /^[ \t]*(type|exception)[ \t]/ { in_type = 1 }
    /^[ \t]*(let|val|external|open|include|module|end|class)([ \t]|$)/ { in_type = 0 }
    in_type && /(of|\*|:) string( |;|$|\))/ { n++ }
    END { print n + 0 }
  ' "$ROOT/$1"
}

# Input, output and process control. Printing into a string or a buffer
# (Printf.sprintf, Format.asprintf, Printf.bprintf) is neither, and a printer
# that writes to the formatter it is given (Format.fprintf) does what its
# caller decides.
IO_PATTERN='open_out|open_in'
IO_PATTERN+='|(^|[^_a-zA-Z])(print_(string|endline|newline|int|char|float|bytes)|prerr_[a-z]+|read_line|input_(line|char|value|byte)|really_input[a-z_]*|output_(string|char|bytes|value|byte)|close_(in|out))'
IO_PATTERN+='|Printf\.k?[ef]?printf|Format\.(e?printf|(std|err)_formatter|formatter_of_out_channel|print_[a-z_]+)'
IO_PATTERN+='|(^|[^_a-zA-Z.])(Stdlib\.)?(exit|at_exit)( |$|\()|(^|[^_a-zA-Z])flush( |$|\()'
IO_PATTERN+='|In_channel|Out_channel|Lexing\.from_channel|Sys\.|(^|[^_a-zA-Z])Unix($|[^_a-zA-Z])|Filename\.'
IO_PATTERN+='|write_string_to_file|read_file_to_string|run_command'

# Lines where a string literal names something built in, outside the module
# that holds those names. Two kinds of literal count.
#   A name of the table (every word-like literal of lib/BuiltInNames.ml:
#   "Unit", "abort", "Foreign_Import") counts wherever it stands: compared,
#   matched on, listed, or the result of a match arm that maps a constructor
#   to its name.
#   Any other capitalised word ("Buffer") counts where a string is tested
#   against it, because that is how a name not yet in the table is spelled:
#     | "X" ->, | "X" | "Y" ->       a match arm on the literal
#     = "X", <> "X"                  a comparison or a binding
#     String.equal x "X"             the same, with the function
#     List.mem x ["X"; ...]          a lookup in a list of names
# Also counted, whatever the literal:
#     make_ident "x", make_mod_name "x"   a literal becomes a name
#     (ident_string i) = "x"              the string of a name is compared with a literal
#     "camelCase" alone on a line         a match arm whose result is a method name
# Not counted: the wording of a message (the literal after Text or Code, which
# name_lines removes before matching), a token of the lexer ("Span" { ... }),
# and every other lowercase word ("plain" and "json" are words of the command
# line). A lowercase name that is not in the table is missed, so the count is
# a lower bound.
NAMES_MODULE=lib/BuiltInNames.ml
table_names=$(grep -oE '"[A-Za-z][A-Za-z0-9_.]*"' "$NAMES_TABLE" | tr -d '"' | sort -u | sed 's/\./\\./g' | paste -sd '|')
[[ -n $table_names ]] || { echo "no names found in $NAMES_TABLE" >&2; exit 2; }
CAPITALISED='"[A-Z][A-Za-z0-9_.]*"'
NAMES_PATTERN='make_(ident|mod_name) "'
NAMES_PATTERN+="|\"($table_names)\" *(\$|[^ {])"
NAMES_PATTERN+="|\\| *$CAPITALISED *(->|\\|)|(=|<>) *$CAPITALISED"
NAMES_PATTERN+="|String\\.equal[^\"]*$CAPITALISED|List\\.mem[^\"]*\\[[^]]*$CAPITALISED"
NAMES_PATTERN+='|_string [a-z_]+\) *= *"|^\s*"[a-z]+[A-Z][A-Za-z0-9]*"\s*$'
name_lines() { # reads OCaml, prints the lines that count
  sed -E 's/(Text|Code) "([^"\\]|\\.)*"/\1/g' | grep -E -- "$NAMES_PATTERN"
}
count_names() { # file
  if [[ $1 == "$NAMES_MODULE" ]]; then echo 0; return; fi
  name_lines < "$ROOT/$1" | wc -l
}

# Mutable state: creating a ref cell, a Hashtbl or a Buffer, declaring a
# mutable field, assigning a field or an element (<-), changing an Array or
# Bytes in place, incr and decr, a lazy value, a Queue or a Stack. Each counts
# whatever its scope: a local cell is as mutable as a global one. Reading
# state (!r, Buffer.contents) and assigning a ref cell (:=, after the ref
# that made it counted) do not count.
STATE_PATTERN='= *ref ([a-z0-9("\[]|\[\])|^\s*ref \(|Hashtbl\.(create|add|replace)|Buffer\.create|^\s*mutable [a-z_]+ *:|<-'
STATE_PATTERN+='|(Array|Bytes)\.(unsafe_)?(set|fill|blit)|(^|[^_a-zA-Z0-9])(incr|decr|lazy)( |$|\()|Lazy\.'
STATE_PATTERN+='|(^|[^_a-zA-Z0-9])(Queue|Stack)\.'

# samples/COLUMN.tsv holds one line of OCaml per row, after the verdict the
# column must give it: count or skip.
self_test() {
  local column verdict line got bad=0 n=0
  for column in io names state; do
    while IFS=$'\t' read -r verdict line; do
      [[ -z $verdict || $verdict == \#* ]] && continue
      case $column in
        io) got=$(grep -cE -- "$IO_PATTERN" <<< "$line") ;;
        names) got=$(name_lines <<< "$line" | wc -l) ;;
        state) got=$(grep -cE -- "$STATE_PATTERN" <<< "$line") ;;
      esac
      if ((got)); then got=count; else got=skip; fi
      n=$((n + 1))
      [[ $got == "$verdict" ]] || { printf '%s: expected %s, got %s: %s\n' "$column" "$verdict" "$got" "$line"; bad=1; }
    done < "$HERE/samples/$column.tsv"
  done
  ((bad)) && return 1
  echo "self-test: $n sample lines, all as expected"
}

# One row per file. Columns:
#   raise     places that raise: raise_error, type_error and the other helpers of a category, internal_err,
#             ok_or_raise (the error of a result), failwith, raise, assert false
#   internal  a state the code believes cannot happen: internal_err, or the same error built as a
#             value, Error (Internal ...)
#   adhoc     errors with no name: err with a bare string. R11 removed err, so this stays at 0
#   catch     places that catch or re-label: try, adorn_error_with_*, with_errors
#   state     mutable state, created or changed (STATE_PATTERN above)
#   io        input, output and process control: opening, closing and using channels, printing,
#             anything in Sys, Unix or Filename, exit and at_exit, and the compiler's own file
#             and process helpers
#   wildcard  match arms that catch everything: | _ ->
#   strings   string used as a field or constructor argument in a type definition
#   names     string literals that name something built in, outside lib/BuiltInNames.ml
metrics() {
  printf 'file\tlines\traise\tcatch\tstate\tio\twildcard\tstrings\tinternal\tadhoc\tnames\n'
  sources | while read -r f; do
    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$f" \
      "$(wc -l < "$ROOT/$f")" \
      "$(count 'raise_error|ok_or_raise|(^|[^_a-zA-Z])(parse|cli|type|linearity|declaration|entrypoint|generic)_error(_at)? |internal_err|failwith|(^|[^_a-zA-Z])raise |assert false' "$f")" \
      "$(count '(^|[^_a-zA-Z])try($|[^_a-zA-Z])|adorn_error_with_[a-z_]+|with_errors' "$f")" \
      "$(count "$STATE_PATTERN" "$f")" \
      "$(count "$IO_PATTERN" "$f")" \
      "$(count '^\s*\| _ ->' "$f")" \
      "$(count_type_strings "$f")" \
      "$(count 'internal_err|Error \(Internal ' "$f")" \
      "$(count '(^|[^_a-zA-Z])err [("]' "$f")" \
      "$(count_names "$f")"
  done
}

totals() { # metrics-file
  # coreio is the part of io that is in the core (lib/). The plan wants it at 0.
  awk -F'\t' 'NR > 1 { for (i = 2; i <= NF; i++) s[i] += $i; n++; if ($1 ~ /^lib\//) coreio += $6 }
    END { printf "files %d\nlines %d\nraise %d\ncatch %d\nstate %d\nio %d\nwildcard %d\nstrings %d\ninternal %d\nadhoc %d\ncoreio %d\nnames %d\n", n, s[2], s[3], s[4], s[5], s[6], s[7], s[8], s[9], s[10], coreio, s[11] }' "$1"
}

# Fragile matches: a match that a new constructor would fall into without a
# compile error. The OCaml compiler reports each one as warning 4.
fragile() {
  local w="$T/tree"
  mkdir -p "$w"
  # Untracked files are copied too: the gate runs before a new file is committed.
  (cd "$ROOT" && git ls-files -z --cached --others --exclude-standard lib cli bin dune-project concat_builtins.py austral.opam | xargs -0 tar -cf - ) | tar -xf - -C "$w"
  read -r -a nix_flags <<< "${ZUSTRAL_NIX_FLAGS:-}"
  local build='python3 concat_builtins.py && dune build --root . --profile release ./lib ./cli 2>&1'
  if [[ -n ${IN_NIX_SHELL:-} ]]; then
    (cd "$w" && OCAMLPARAM="_,w=+4,warn-error=-a" timeout "${ZUSTRAL_BUILD_TIMEOUT:-900}" bash -c "$build") > "$T/build.log"
  else
    (cd "$w" && OCAMLPARAM="_,w=+4,warn-error=-a" timeout "${ZUSTRAL_BUILD_TIMEOUT:-900}" \
      nix develop "$ROOT" "${nix_flags[@]}" --command bash -c "$build") > "$T/build.log"
  fi
  local rc=$?
  ((rc == 0)) || { echo "build failed ($rc): $(tail -n 3 "$T/build.log")" >&2; return 1; }
  awk '
    /^File "/ { split($0, a, "\""); file = a[2]; next }
    /^Warning 4 / { want = 1; next }
    want && /constructors are added to type/ {
      t = $0; sub(/.*to type /, "", t); sub(/\.$/, "", t); sub(/^Austral_(core|cli)__/, "", t)
      want = 0
      # A type named without its module is defined in the file being compiled.
      if (t !~ /\./) { m = file; sub(/.*\//, "", m); sub(/\.ml$/, "", m); t = m "." t }
      print file "\t" t
    }' "$T/build.log" | LC_ALL=C sort | uniq -c | awk '{ print $2 "\t" $3 "\t" $1 }'
}

# type-fragile is the part on the types of lib/Type.ml and on
# MonoType.mono_ty. R20 brought it to 0; a new type constructor must stop
# the build at every match on these types, so it stays at 0.
fragile_totals() { # fragile-file
  awk -F'\t' '{ s += $3; f[$1] = 1; t[$2] += $3; if ($2 ~ /^(Type|MonoType)\./) ty += $3 }
    END { print "fragile-matches " s + 0; print "files " length(f); print "type-fragile " ty + 0; for (k in t) print "type " k " " t[k] }' "$1" | LC_ALL=C sort
}

check() { # name
  if ((RECORD)); then
    cp "$T/$1" "$HERE/expected/$1"; echo "recorded $1"
  elif diff -u "$HERE/expected/$1" "$T/$1"; then
    echo "match $1"
  else
    echo "MISMATCH $1"; failed=1
  fi
}

if ((SELF_TEST)); then self_test; exit $?; fi
# A count means what the samples say it means. Every run checks that first.
self_test > "$T/self-test.log" || { cat "$T/self-test.log"; echo "the patterns disagree with samples/"; exit 1; }
if ((CHECK_FRAGILE)); then
  fragile > "$T/fragile.tsv" || exit 1
  fragile_totals "$T/fragile.tsv" > "$T/fragile-totals.txt"
  # Recording a larger number must not hide a new catch-all on these types.
  if ! grep -qx 'type-fragile 0' "$T/fragile-totals.txt"; then
    echo "a match on a type of lib/Type.ml or on MonoType.mono_ty has a catch-all arm:"
    awk -F'\t' '$2 ~ /^(Type|MonoType)\./' "$T/fragile.tsv"
    exit 1
  fi
  if ! diff -u "$HERE/expected/fragile-totals.txt" "$T/fragile-totals.txt"; then
    echo "MISMATCH fragile-totals.txt (if the counts went down: measure.sh --fragile --record)"
    exit 1
  fi
  echo "match fragile-totals.txt: $(grep -E '^(fragile-matches|type-fragile) ' "$T/fragile-totals.txt" | paste -sd, - | sed 's/,/, /')"
  exit 0
fi

failed=0
metrics > "$T/metrics.tsv"
totals "$T/metrics.tsv" > "$T/totals.txt"
if [[ -n $TREE ]]; then cat "$T/totals.txt"; exit 0; fi
check metrics.tsv
check totals.txt
if ((FRAGILE)); then
  if fragile > "$T/fragile.tsv"; then
    fragile_totals "$T/fragile.tsv" > "$T/fragile-totals.txt"
    check fragile.tsv
    check fragile-totals.txt
  else
    failed=1
  fi
fi
exit "$failed"
