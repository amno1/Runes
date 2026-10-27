#!/bin/sh
# A/B of decompress compiled by two SBCLs (e.g. master and master + the pass).
#   ./ab.sh /path/to/baseline/run-sbcl.sh /path/to/patched/run-sbcl.sh [files...]
# Compiles decompress once per compiler into its own fasl directory, then runs
# run.lisp under perf stat, alternating A and B, twice per file.  A load-only
# run (BENCH unset) gives the baseline to subtract.
# Columns: label file MB/s instructions cycles.
set -e
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../../.." && pwd)
A=$1; B=$2; shift 2
files=${*:-"SourceCodePro-Regular.woff meslo.z9.z src8m.z6.z src1m.q9.br RedHatText-Regular.br"}
out=${OUT:-/tmp/mov-retarget-ab}
mkdir -p "$out/A" "$out/B"
build() { ASDF_OUTPUT_TRANSLATIONS="$root/:$out/$1/" "$2" --non-interactive \
  --eval '(require :asdf)' --eval "(push #p\"$root/\" asdf:*central-registry*)" \
  --eval '(handler-bind ((warning (function muffle-warning))) (asdf:load-system "decompress" :force t))' >/dev/null 2>&1; }
build A "$A"; build B "$B"
run() { printf '%s %s ' "$1" "$3"
  ASDF_OUTPUT_TRANSLATIONS="$root/:$out/$1/" FILE=$3 BENCH=$4 \
    perf stat -x, -e instructions:u,cycles:u "$2" --script "$here/run.lisp" 2>&1 \
    | grep -v '^;' | cut -d, -f1 | tr '\n' ' '; echo; }
run A "$A" load-only ""; run B "$B" load-only ""
for f in $files; do for i in 1 2; do run A "$A" "$f" 1; run B "$B" "$f" 1; done; done
