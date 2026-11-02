# decompress

**Brotli** ([RFC 7932](https://datatracker.ietf.org/doc/html/rfc7932 "RFC
7932")) and **deflate/zlib** ([RFC
1951/1950](https://datatracker.ietf.org/doc/html/rfc1951 "RFC 1951/1950"))
decoders in Common Lisp for SBCL — for WOFF2 and WOFF 1.0 fonts,
`Content-Encoding: br` and `deflate`. No FFI: both decoders are written in
Common Lisp, with optional AVX-512 code through sb-simd. The 122,784-byte Brotli
static dictionary, which every Brotli decoder needs, is the Brotli project's
copy, in `data/dictionary.bin`. It is read the first time a stream uses it.
RFC 7932 prints the same bytes as a hex dump in Appendix A, so the file can be
re-created from the standard rather than taken from libbrotli.

```lisp
(asdf:load-system "decompress")

;; Brotli (WOFF2: pass the decoded size, which WOFF2 records)
(decompress:decompress octets)                     ; => decoded octets
(decompress:decompress octets :size-hint n)

;; zlib (WOFF 1.0: one stream per table; pass the table's origLength)
(decompress:zlib-decompress octets :size-hint n)   ; => decoded octets
;; raw deflate
(decompress:inflate octets :size-hint n)
```

`:size-hint` is optional; with it the output is allocated once. Malformed
input signals `decompress:brotli-error` or `decompress:deflate-error`, both
subclasses of `decompress:decompression-error`.

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

## Performance

MB/s of decoded output, best of 5 rounds after warm-up, both sides decoding into
a fresh buffer of the known size, on an AVX-512 machine with SBCL's sb-simd-512
branch; each figure is the mean of two runs in the same session.  C is called
through `sb-alien`. A WOFF 1.0 row decodes every compressed table of the font.

| Brotli stream               | Size¹               | decompress  | libbrotli² | ratio         |
|-----------------------------|---------------------|-------------|------------|---------------|
| Red Hat Text WOFF2          | 28 KB → 69 KB       | 343         | 343        | 1.00×         |
| Meslo Nerd Font WOFF2       | 1.18 → 2.42 MB      | 316         | 284        | 1.11×         |
| Noto Serif Tangut WOFF2     | 1.02 → 2.40 MB      | 409         | 352        | 1.16×         |
| Noto Sans SignWriting WOFF2 | 0.87 → 4.02 MB      | 544         | 501        | 1.09×         |
| SBCL source q9 / q11        | 1.58 / 1.42 → 8 MB  | 1131 / 1021 | 862 / 892  | 1.31× / 1.14× |
| SBCL source 1 MB q9 / q5    | 208 / 218 KB → 1 MB | 1026 / 926  | 867 / 805  | 1.18× / 1.15× |

| zlib stream              | Size¹              | decompress  | zlib-ng³    | libdeflate⁴ | vs zlib-ng / libdeflate |
|--------------------------|--------------------|-------------|-------------|-------------|-------------------------|
| Red Hat Text WOFF        | 36 KB → 73 KB      | 878         | 877         | 1008        | 1.00× / 0.87×           |
| Source Code Pro WOFF     | 68 KB → 138 KB     | 693         | 569         | 781         | 1.22× / 0.89×           |
| Meslo TTF, level 9       | 1.60 → 2.98 MB     | 613         | 573         | 731         | 1.07× / 0.84×           |
| SBCL source, level 6 / 9 | 1.94 / 1.92 → 8 MB | 1325 / 1407 | 1274 / 1349 | 1599 / 1703 | 1.04× / 0.83×           |

1. Compressed → decoded.
2. `BrotliDecoderDecompress`, libbrotli 1.2.1's one-shot decoder (C), as
   CachyOS ships it (built with BMI2). A `-march=native` build was 1–4%
   faster and left every ratio above 1.0×.
3. libz's `uncompress`; here libz is zlib-ng 2.3's zlib-compatible build, a
   SIMD-tuned zlib.
4. `libdeflate_zlib_decompress` (`ZLIB=libdeflate` in `bench-c.lisp`), the
   fastest whole-buffer deflate decoder I know of.

Small streams are dominated by per-stream setup (reading prefix codes and
building their tables); large ones by the symbol loop. Both symbol loops run
as VOPs on AVX-512 builds. The deflate decoder matches or beats zlib-ng and
is 0.83–0.89× libdeflate; the gap to libdeflate is in the loop itself
(`doc/optimizations.md`, section 18). `doc/optimizations.md` records every
optimisation tried, with measured results.

```sh
cd tests/bench
SBCL=sbcl ./make-data.sh            # builds data/: fonts, SBCL source, zlib streams
sbcl --script bench.lisp            # decompress
sbcl --script bench-c.lisp          # libbrotli and libz
ZLIB=libdeflate sbcl --script bench-c.lisp   # zlib streams with libdeflate
ONLY=.woff sbcl --script bench.lisp # only files whose name contains ".woff"
sbcl --script profile.lisp data/RedHatText-Regular.br
sbcl --script profile.lisp data/SourceCodePro-Regular.woff   # zlib streams too
sbcl --script perf-compare.lisp     # perf counters per byte, against libdeflate
sbcl --script literal-pairs.lisp    # literal pairs a multi-symbol table could cover
```

To run `make-data.sh` you will need: `brotli`, `woff2_compress`, libz, the fonts
it names, and an SBCL source tree (`SBCL_SRC`, default `~/repos/sbcl`).

## Requirements

**SBCL** on a 64-bit little-endian CPU, such as x86-64 or 64-bit Arm
(developed and tested on x86-64; other CPUs untested).  The bit reader
loads 8 input bytes as one native word, which matches the bit order of
Brotli and deflate only on little-endian CPUs; on any other the system
signals an error when it is compiled.  The decoders also use SAPs for
unaligned loads and stores, raw-word structure slots,
`sb-ext:define-load-time-global` and `sb-kernel:%shrink-vector`.

**AVX-512 (optional)**: when SBCL has sb-simd with AVX512BW (the sb-simd-512
branch) *and* the compiling CPU supports AVX512BW, the Brotli command loop and
the deflate symbol loop run as VOPs (which also use BMI2), long back-reference
copies move 64 bytes at a time, deflate tables are sorted and counted with
64-byte compares, and Adler-32 runs 64 bytes per step. This is a compile
time option, so a fasl built that way must only be loaded on AVX-512 CPUs.
Without it, stock sb-simd's AVX2 runs Adler-32 32 bytes per step when the
compiling CPU has AVX2 (same compile-time rule).

The check is quiet: stock SBCL's sb-simd, and older builds of sb-simd-512,
lack operations the AVX-512 code needs (`u8.64-sad`, `s16.32-maddubs`), so
with them the plain Lisp loops are compiled instead, with no warning. The
speeds quoted above need a current sb-simd-512 build; the Lisp loops are
slower (measured with Brotli: about 25% on the large streams, 5–15% on the
fonts). To check a build, see whether `:decompress-avx512` is in `*features*`
after loading the system.

**ASDF at run time**: the dictionary file is found with
`asdf:system-relative-pathname`, so an image that uses Brotli dictionary words
needs ASDF and the `data/` directory, or the dictionary loaded before the image
is saved.

**Tests and benchmarks**: libbrotlienc, libbrotlidec (the `brotli` package) and
libz; libdeflate and `perf` for `ZLIB=libdeflate` and `perf-compare.lisp`.
`LIBBROTLIENC`, `LIBBROTLIDEC`, `LIBZ` and `LIBDEFLATE` override the library
paths.

## Layout

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

## License

MIT, Copyright (c) 2026 Arthur Miller (see `LICENSE`). The test vectors in
`tests/vectors/` are taken from ynniv's decompress (MIT); the static dictionary
is from the Brotli project (MIT).
