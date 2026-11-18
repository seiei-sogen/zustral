#!/usr/bin/env bash
# Runs, in order, every check that .github/workflows/build-and-test.yml runs, and prints one
# line per check: its exit status and its time. Exits 1 if any check failed.
#
# Run it inside the nix shell, from the root of the repository:
#   nix-shell --command 'tools/refactoring/run-every-ci-check-locally.sh'
# The output of each check is kept in $ZUSTRAL_CI_CHECK_LOG_DIR (default: a new temporary directory).
#
# The equivalence check with the base branch is not here: it compares two commits, and a bug
# fix differs on purpose.

set -uo pipefail
cd "$(dirname "$0")/../.."

LOG_DIR=${ZUSTRAL_CI_CHECK_LOG_DIR:-$(mktemp -d)}
mkdir -p "$LOG_DIR"
failed=0

run_one_check() {
  local name=$1; shift
  local started=$SECONDS
  if "$@" > "$LOG_DIR/$name.log" 2>&1; then
    echo "ok    $name ($((SECONDS - started))s)"
  else
    echo "FAIL  $name ($((SECONDS - started))s)  $LOG_DIR/$name.log"
    failed=1
  fi
}

run_one_check make                       make
run_one_check make-test                  make test
run_one_check end-to-end                 python3 test-programs/runner.py
run_one_check command-line               python3 test-programs/cli-runner.py
run_one_check metrics-equal-the-record   tools/refactoring/measure.sh
run_one_check no-unused-exports          tools/refactoring/unused-exports.sh
run_one_check every-message-has-a-test   python3 tools/refactoring/unpinned-messages.py
run_one_check no-error-escapes-a-stage   python3 tools/refactoring/escape-check.py
run_one_check only-environment-makes-ids python3 tools/refactoring/id-makers.py
run_one_check every-wording-is-shown     tools/refactoring/coverage.sh --messages
run_one_check every-line-is-executed     tools/refactoring/coverage.sh --unexecuted --check
run_one_check examples                   bash run-examples.sh
run_one_check stdlib-build               bash -c 'make -C standard clean && make -C standard'
run_one_check stdlib-test                ./standard/test_bin

exit "$failed"
