#!/bin/sh
# tests/bench/make-data.sh — build the benchmark streams in tests/bench/data/.
#
# Needs: brotli (CLI), woff2_compress, sbcl, libz; the fonts below
# (Arch/CachyOS package paths) and an SBCL source tree (SBCL_SRC, default
# ~/repos/sbcl).
#
#   WOFF2 fonts: one real web font (Red Hat Text, shipped with gegl's docs) and
#     three system fonts converted with woff2_compress; their Brotli streams
#     and decompressed sizes are extracted by woff2-streams.lisp.
#   src8m: the first 8 MB of SBCL's src/code + src/compiler, brotli -q 9 / 11.
#   src1m: the first 1 MB of SBCL's src/code, brotli -q 9 / 5.
#   WOFF 1.0: two real web fonts (Red Hat Text, Source Code Pro); every
#     compressed table in them is a zlib stream (bench.lisp reads the .woff).
#   zlib (.z): src8m at levels 6 and 9, and the Meslo TTF at level 9, made
#     with libz by zlib-streams.lisp.
set -e
here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
data="$here/data"
SBCL=${SBCL:-sbcl}
SBCL_SRC=${SBCL_SRC:-$HOME/repos/sbcl}
mkdir -p "$data"

cp /usr/share/doc/gegl-0.4/RedHatText-Regular.woff2 "$data/"
cp /usr/share/doc/gegl-0.4/RedHatText-Regular.woff "$data/"
cp /usr/share/doc/gegl-0.4/SourceCodePro-Regular.ttf.woff "$data/SourceCodePro-Regular.woff"
cp /usr/share/fonts/TTF/MesloLGLNerdFont-Bold.ttf "$data/meslo.ttf"
(cd "$here" && "$SBCL" --script zlib-streams.lisp 9 data/meslo.z9.z data/meslo.ttf)
rm "$data/meslo.ttf"
for f in /usr/share/fonts/TTF/MesloLGLNerdFont-Bold.ttf \
         /usr/share/fonts/noto/NotoSerifTangut-Regular.ttf \
         /usr/share/fonts/noto/NotoSansSignWriting-Regular.ttf; do
  cp "$f" "$data/"
  woff2_compress "$data/$(basename "$f")" >/dev/null
  rm "$data/$(basename "$f")"
done
(cd "$here" && "$SBCL" --script woff2-streams.lisp data/*.woff2)

cat "$SBCL_SRC"/src/code/*.lisp "$SBCL_SRC"/src/compiler/*.lisp | head -c 8000000 > "$data/src8m"
cat "$SBCL_SRC"/src/code/*.lisp | head -c 1000000 > "$data/src1m"
brotli -q 9  -fo "$data/src8m.q9.br"  "$data/src8m"
brotli -q 11 -fo "$data/src8m.q11.br" "$data/src8m"
brotli -q 9  -fo "$data/src1m.q9.br"  "$data/src1m"
brotli -q 5  -fo "$data/src1m.q5.br"  "$data/src1m"
(cd "$here" && "$SBCL" --script zlib-streams.lisp 6 data/src8m.z6.z data/src8m \
             && "$SBCL" --script zlib-streams.lisp 9 data/src8m.z9.z data/src8m)
# decompressed sizes, as the WOFF2 streams have them
for q in q9 q11; do wc -c < "$data/src8m" > "$data/src8m.$q.size"; done
for q in q9 q5;  do wc -c < "$data/src1m" > "$data/src1m.$q.size"; done
rm "$data/src8m" "$data/src1m"
ls -l "$data"
