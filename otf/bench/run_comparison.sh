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

# DejaVu Sans is not among the test fonts: DEJAVU names it, else the places
# distributions put it.  The Lisp side looks in the same ones.
if [ -z "${DEJAVU:-}" ]; then
    for candidate in /usr/share/fonts/TTF/DejaVuSans.ttf \
                     /usr/share/fonts/truetype/dejavu/DejaVuSans.ttf \
                     /usr/share/fonts/dejavu/DejaVuSans.ttf \
                     /usr/share/fonts/dejavu-sans-fonts/DejaVuSans.ttf \
                     "${FONTS_DIR}/DejaVuSans.ttf"; do
        if [ -f "${candidate}" ]; then DEJAVU="${candidate}"; break; fi
    done
fi
if [ -z "${DEJAVU:-}" ] || [ ! -f "${DEJAVU}" ]; then
    echo "DejaVuSans.ttf not found: install DejaVu Sans or set DEJAVU to its path" >&2
    exit 1
fi
export DEJAVU

echo "RUNNING FREETYPE C"
"${BENCH_DIR}/ft_hint_bench" "${DEJAVU}" "${FONTS_DIR}/LiberationSans-Regular.ttf"

SBCL_BIN="${SBCL_BIN:-/home/arthur/repos/sb-simd-512/run-sbcl.sh}"
if [ ! -x "${SBCL_BIN}" ]; then
    SBCL_BIN="sbcl"
fi

echo "RUNNING RUNES/OTF (${SBCL_BIN})"
"${SBCL_BIN}" --noinform --dynamic-space-size 4096 --disable-debugger \
     --load "${BENCH_DIR}/hint_bench.lisp" --eval '(sb-ext:exit)'
