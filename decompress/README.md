# decompress

**Brotli** ([RFC 7932](https://datatracker.ietf.org/doc/html/rfc7932 "RFC
7932")) and **deflate/zlib** ([RFC
1951/1950](https://datatracker.ietf.org/doc/html/rfc1951 "RFC 1951/1950"))
decoders in Common Lisp for SBCL — for WOFF2 and WOFF 1.0 fonts,
`Content-Encoding: br` and `deflate`. No FFI: both decoders are written in
Common Lisp, with optional AVX-512 code through sb-simd. The 122,784-byte Brotli
static dictionary, needed by Brotli decoder, is taken from the Brotli project
directly; found in `data/dictionary.bin`. It is loaded the first time a stream
uses it. It could be re-created from the RFC 7932 directly (Appendix A) rather
than taken from libbrotli, but I don't do it.

# Quick example:

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
it uses, and an SBCL source tree (`SBCL_SRC`, default `~/repos/sbcl`).

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

## License

MIT, Copyright (c) 2026 Arthur Miller (see `LICENSE`). The test vectors in
`tests/vectors/` are taken from ynniv's decompress (MIT); the static dictionary
is from the Brotli project (MIT).
