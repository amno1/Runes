## Design

Both decoders are structured after C decoders: libbrotli for Brotli, zlib-ng and
libdeflate for deflate. They are tuned for SBCL, and share some parts, but not
all. Some of the important ideas:

**Bit reader** is a 64-bit bit buffer in a raw word, into which we load the
input with one unaligned 8-byte load (56–63 bits after a refill). The input
is read in place and the last 8 bytes of the input are copied and zero-padded,
so no load leaves the array. Buffer, bit count, and position are declared as
untagged words, and (hopefully) kept in registers. On AVX-512 builds, the VOPs
keep them in registers for their whole loop.

**Prefix codes** are decoded through two-level tables: a root indexed by the
next 8 bits (10 for deflate’s literal/length code), and second-level tables for
longer codes. Both formats use canonical prefix codes read LSB-first. Brotli’s
tables (huffman.lisp) are built from only the symbols that have a code, without
scanning the whole alphabet; deflate’s are built by their own builder, described
below.

**Brotli** decodes each meta-block with a command loop, written as stage macros
over locals. It is compiled twice: once for the common case of a window that
covers all output, and once for a window smaller than the output, which needs an
extra test per back-reference. If you compile with AVX-512 (sb-simd-512), the
common commands run as one VOP (`brotli-vop.lisp`, BMI2 shifts): the command
and its lengths, the literal run, the distance and the copy, with the bit
buffer, positions and last two output bytes in registers. Block switches,
static dictionary words, the meta-block's end and errors exit the VOP with a
status, and the Lisp loop finishes that command.

**Deflate** — built after zlib-ng's fast loop: 10-bit roots for the
literal/length tables (font data has many literal codes longer than 8 bits),
table entries that carry a match's base and extra-bit count (no second lookup),
up to three literals per refill, and the next root lookup issued before the
refill so their loads overlap. From libdeflate: a length entry's bit count
includes its extra bits, so one shift consumes both, and the decode tables are
built by doubling the root (no strided stores and no bit-reversal pass), in
place, in arrays reused across blocks and streams. The loop contains no calls
(errors leave it and are signalled afterwards), so SBCL keeps more of its state
in registers. With AVX-512 (sb-simd-512), the loop runs as one VOP
(`inflate-vop.lisp`, BMI2 shifts) while at least 16 input bytes remain and the
output is not near its end, with everything in registers; the Lisp loop
finishes the block. On other builds, the Lisp loop decodes the whole block. The
algorithm is the same, but the Lisp loop is about 7–12% slower, because SBCL
does not allocate registers as well as we can by hand in a VOP. The zlib
Adler-32 runs 64 bytes per step with AVX-512 (`vpsadbw` and `vpmaddubsw`), or
32 with AVX2.

**Output** is a buffer with write-ahead slack, so back-references can copy whole
words (8 to 24 bytes per step, 64 for long copies with AVX-512) and may
overshoot into the slack. It is shrunk in place at the end.

## Correctness

Comparisons are in `tests/tests.lisp` and use libbrotli and libz as oracles
(on the test machine libz is zlib-ng's zlib-compatible build), called through
`sb-alien`:

The 44 Brotli reference vectors in `tests/vectors/` decode to their
originals.
  
Inputs compressed by libbrotlienc with random quality (0–11), window (10–24)
and mode (generic, text, font) decode to the input, with and without
`:size-hint`;

Inputs compressed by libz with random level (0–9), window (9–15), memory level
(1–9) and strategy (default, filtered, Huffman only, RLE, fixed codes), as zlib
streams and as raw deflate, decode to the input;

Corrupted streams (1–3 bit flips, or truncation) are compared with libbrotlidec
for Brotli, `uncompress` for zlib and `inflate` for raw deflate: where the C
library rejects a stream we signal an error, and where it still decodes one we
produce the same bytes.

The default run is 18,044 checks with a fixed seed.

```sh
sbcl --script tests/run.lisp
```

## The Project Layout

```
src/
  package.lisp   package, types, conditions, AVX-512 feature detection
  tables.lisp    RFC 7932 tables, word transforms, the static dictionary
  reader.lisp    bit reader (shared)
  huffman.lisp   Brotli prefix codes and decode tables
  decoder.lisp   Brotli: meta-blocks, the command loop, DECOMPRESS
  brotli-vop.lisp   the Brotli command loop as one VOP (AVX-512 builds)
  inflate-vop.lisp  the deflate symbol loop as one VOP (AVX-512 builds)
  inflate.lisp   deflate and zlib: INFLATE, ZLIB-DECOMPRESS, Adler-32
data/dictionary.bin
doc/optimizations.md   every optimisation tried, with measured results
sbcl-experiments/      SBCL code-generation experiments: disassemblies, an SBCL patch
tests/
  tests.lisp     libbrotli and libz oracle tests;  run.lisp runs them
  vectors/       Brotli reference streams (.br) and originals (.raw)
  bench/         make-data.sh, woff2-streams.lisp, zlib-streams.lisp,
                 bench.lisp, bench-c.lisp, profile.lisp,
                 perf-compare.lisp, literal-pairs.lisp
```
