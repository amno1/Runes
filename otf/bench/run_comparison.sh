#!/usr/bin/env bash
# run_comparison.sh — one run of FreeType's hinted loading against runes/otf's.
# Prints two sections of "NAME OPERATIONS TIME ms" rows, as bench3x3.py reads.
set -euo pipefail

BENCH_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FONTS_DIR="${BENCH_DIR}/../tests/fonts"

if [ ! -f "${BENCH_DIR}/ft_hint_bench" ] || [ "${BENCH_DIR}/ft_hint_bench.c" -nt "${BENCH_DIR}/ft_hint_bench" ]; then
    cc -O3 -march=native $(pkg-config --cflags freetype2) \
        "${BENCH_DIR}/ft_hint_bench.c" $(pkg-config --libs freetype2) \
        -o "${BENCH_DIR}/ft_hint_bench"
fi

echo "RUNNING FREETYPE C"
"${BENCH_DIR}/ft_hint_bench" "${DEJAVU:-/usr/share/fonts/TTF/DejaVuSans.ttf}" "${FONTS_DIR}/LiberationSans-Regular.ttf"

SBCL_BIN="${SBCL_BIN:-/home/arthur/repos/sb-simd-512/run-sbcl.sh}"
if [ ! -x "${SBCL_BIN}" ]; then
    SBCL_BIN="sbcl"
fi

echo "RUNNING RUNES/OTF (${SBCL_BIN})"
"${SBCL_BIN}" --noinform --dynamic-space-size 4096 --disable-debugger \
     --load "${BENCH_DIR}/hint_bench.lisp" --eval '(sb-ext:exit)'
