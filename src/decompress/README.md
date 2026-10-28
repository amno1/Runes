# decompress

**Brotli** (RFC 7932) and **deflate/zlib** (RFC 1951/1950) decoders in Common
Lisp for SBCL — for WOFF2 and WOFF 1.0 fonts, `Content-Encoding: br` and
`deflate`. No FFI: both decoders are written Lisp, with optional AVX-512 code
through sb-simd. The 122,784-byte Brotli static dictionary is vendored in
`data/`.

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

Both decoders follow the structure of the fast C decoders, written for SBCL,
and share their parts:

- **Bit reader** — a 64-bit bit buffer in a raw word, refilled with one
  unaligned 8-byte load (56–63 bits after a refill); the input is read in
  place (only its last 8 bytes are copied, zero-padded, so no load leaves the
  array). Buffer, bit count and position stay untagged words.
- **Prefix codes** — two-level decode tables (8-bit root, second level for
  longer codes), built from the used symbols only, never by scanning the whole
  alphabet. Brotli and deflate use the same canonical, LSB-first codes.
- **Output** — one buffer with write-ahead slack, so back-references copy 16
  bytes per step (64 with AVX-512) and may overshoot; it is shrunk in place at
  the end, not copied.
- **Brotli** — one command-loop function per meta-block, written as stage
  macros over locals, with a second copy for the common case of a window
  covering all output. Dictionary words are transformed straight into the
  output.
- **Deflate** — built after zlib-ng's fast loop: 10-bit roots for the
  literal/length tables (font data has many literal codes longer than 8
  bits), table entries that carry a match's base and extra-bit count (no
  second lookup), up to three literals per refill, and the next root lookup
  issued before the refill so their loads overlap. From libdeflate: a length
  entry's bit count includes its extra bits, so one shift consumes both, and
  the decode tables are built by doubling the root (no strided stores, no
  bit-reversal pass). The loop contains no calls (errors leave it and are
  signalled afterwards), so SBCL keeps more of its state in registers. The
  zlib Adler-32 runs 64 bytes per step with AVX-512 (`vpsadbw` and
  `vpmaddubsw`), or 32 with AVX2.

## Correctness

Comparisons are in `tests/tests.lisp` and use libbrotli and libz-ng as oracles,
through `sb-alien`:

- the 44 Brotli reference vectors in `tests/vectors/` decode to their
  originals;
- inputs compressed by libbrotlienc at every quality (0–11), window (10–24)
  and mode decode to the input, with and without `:size-hint`;
- inputs compressed by libz at every level (0–9), window (9–15), memory level
  and strategy (default, filtered, Huffman only, RLE, fixed codes), as zlib
  streams and as raw deflate, decode to the input;
- corrupted streams (bit flips, truncation) — Brotli compares to libbrotlidec,
  zlib to `uncompress`, raw deflate to `inflate`: we produce the same bytes on
  correct input, and signal an error on the same errors as those.

The default run is 18,044 checks with a fixed seed; runs of 40,000–86,000
checks with other seeds also pass.

```sh
sbcl --script tests/run.lisp
```

## Performance

MB/s of decoded output, best of 5 rounds after warm-up, both sides decoding
into a fresh buffer of the known size, on an AVX-512 machine with SBCL's
sb-simd-512 branch. C is libbrotli's `BrotliDecoderDecompress` and libz's
`uncompress` (zlib-ng 1.3.1 here, a SIMD-tuned zlib), called through
`sb-alien`. A WOFF 1.0 row decodes every compressed table of the font.

| Stream                               | Compressed → output   | decompress | C           | ratio         |
|--------------------------------------|-----------------------|-------------|-------------|---------------|
| **Brotli**                           |                       |             |             |               |
| Red Hat Text (web font, WOFF2)       | 27.9 KB → 69 KB       | 319         | 347         | 0.92×         |
| Meslo Nerd Font Bold (WOFF2)         | 1.18 MB → 2.42 MB     | 310         | 286         | 1.08×         |
| Noto Serif Tangut (WOFF2)            | 1.02 MB → 2.40 MB     | 406         | 351         | 1.16×         |
| Noto Sans SignWriting (WOFF2)        | 0.87 MB → 4.02 MB     | 524         | 501         | 1.05×         |
| SBCL source, 8 MB, quality 9 / 11    | 1.58 / 1.42 MB → 8 MB | 885 / 839   | 892 / 903   | 0.99× / 0.93× |
| SBCL source, 1 MB, quality 9 / 5     | 208 / 218 KB → 1 MB   | 823 / 768   | 872 / 810   | 0.94× / 0.95× |
| **zlib / deflate**                   |                       |             |             |               |
| Red Hat Text (web font, WOFF 1.0)    | 36 KB → 73 KB         | 756         | 874         | 0.86×         |
| Source Code Pro (web font, WOFF 1.0) | 68 KB → 138 KB        | 473         | 584         | 0.81×         |
| Meslo Nerd Font Bold TTF, level 9    | 1.60 MB → 2.98 MB     | 526         | 579         | 0.91×         |
| SBCL source, 8 MB, level 6 / 9       | 1.94 / 1.92 MB → 8 MB | 1111 / 1168 | 1273 / 1332 | 0.87× / 0.88× |

Small streams are dominated by per-stream setup (reading prefix codes and
building their tables); large ones by the symbol loop, where the remaining gap
is mostly SBCL's register allocation in one large loop body. zlib-ng's inflate
is heavily SIMD-tuned. `doc/optimizations.md` records every optimisation tried,
with measured results.

```sh
cd tests/bench
SBCL=sbcl ./make-data.sh            # builds data/: fonts, SBCL source, zlib streams
sbcl --script bench.lisp            # decompress
sbcl --script bench-c.lisp          # libbrotli and libz
ONLY=.woff sbcl --script bench.lisp # only files whose name contains ".woff"
sbcl --script profile.lisp data/RedHatText-Regular.br
```

`make-data.sh` needs `brotli`, `woff2_compress`, libz, the fonts it names and
an SBCL source tree (`SBCL_SRC`, default `~/repos/sbcl`).

## Requirements

- **SBCL**, little-endian (developed and tested on x86-64): the decoders use
  SAPs for unaligned loads and stores, raw-word structure slots,
  `sb-ext:define-load-time-global` and `sb-kernel:%shrink-vector`.

- **AVX-512 (optional)**: when SBCL has sb-simd with AVX512BW (the sb-simd-512
  branch) *and* the compiling CPU supports AVX512BW, long back-reference copies
  move 64 bytes at a time and Adler-32 runs 64 bytes per step. This is a compile
  time option, so a fasl built that way must only be loaded on AVX-512 CPUs.
  Without it, stock sb-simd's AVX2 runs Adler-32 32 bytes per step when the
  compiling CPU has AVX2 (same compile-time rule).

- **Tests and benchmarks**: libbrotlienc, libbrotlidec (the `brotli` package)
  and libz; `LIBBROTLIENC`, `LIBBROTLIDEC` and `LIBZ` override their paths.

## Layout

```
src/
  package.lisp   package, types, conditions, AVX-512 feature detection
  tables.lisp    RFC 7932 tables, word transforms, the static dictionary
  reader.lisp    bit reader (shared)
  huffman.lisp   prefix codes and decode tables (shared)
  decoder.lisp   Brotli: meta-blocks, the command loop, DECOMPRESS
  inflate.lisp   deflate and zlib: INFLATE, ZLIB-DECOMPRESS, Adler-32
data/dictionary.bin
doc/optimizations.md   every optimisation tried, with measured results
tests/
  tests.lisp     libbrotli and libz oracle tests;  run.lisp runs them
  vectors/       Brotli reference streams (.br) and originals (.raw)
  bench/         make-data.sh, woff2-streams.lisp, zlib-streams.lisp,
                 bench.lisp, bench-c.lisp, profile.lisp
```

## License

MIT, Copyright (c) 2026 Arthur Miller (see `LICENSE`). The test vectors in
`tests/vectors/` are taken from decompress (MIT, ynniv); the static dictionary
is from the Brotli project (MIT).
