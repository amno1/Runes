# runes/otf

runes/otf is the font loader of the Runes project. It started as a fork of [zpb-ttf](https://github.com/xach/zpb-ttf) by Zachary Beane and has since grown into a broad OpenType loader: TrueType and PostScript outlines (CFF and CFF2), variable fonts, font collections and web fonts (WOFF and WOFF2). It is written entirely in Common Lisp, with a little assembly, including the two decompressors that WOFF and WOFF2 need (zlib and Brotli). Only SBCL is supported: the code relies on SBCL internals (system-area pointers, `sb-kernel`, VOPs), so a port to other implementations would mean rewriting those parts. With my AVX-512 fork of SBCL (sb-simd-512), some loops use AVX-512 for extra speed; stock SBCL runs everything else unchanged.

You need a font loader if you want to load a font file or a font collection and get its metrics, names, character mapping, kerning and glyph outlines. Two layers are provided:

- `zpb-ttf` is the familiar high-level interface, with the API zpb-ttf programs already use, so existing programs should work unchanged when recompiled on SBCL, and can then open many more fonts. `zpb-ttf.asd` is kept for exactly that.
- `runes/otf` (package `runes/otf`, nickname `otf`) is the low-level layer underneath.

A font is loaded into memory once and its tables are read straight out of the byte buffer, without copying. Every read is bounds-checked, so a truncated or malicious file signals a `font-out-of-bounds-error` rather than reading garbage. The table accessors are generated: each OpenType table is described as an s-expression schema in `schemas/` (derived from the table definitions in the Fontations project), and the code generator in `codegen/` turns those schemas into the Lisp readers in `src/tables/`. They cover the core TrueType tables as well as the layout, variation, color, bitmap and Apple tables. A few layouts whose shape depends on the data itself are decoded by hand in `src/extensions.lisp`. Don't edit the generated files directly; change the schema or the emitter and run `make codegen`.

A typical use looks like this:

```lisp
(zpb-ttf:with-font-loader (font "LiberationSans-Regular.ttf")
  (let ((glyph (zpb-ttf:find-glyph #\A font)))
    (values (zpb-ttf:family-name font)
            (zpb-ttf:advance-width glyph)
            (zpb-ttf:bounding-box glyph))))
```

Fonts with PostScript outlines, in a `CFF ` or `CFF2` table, are drawn by the `runes/otf/charstring` system, a port of Fontations' charstring interpreter together with its CFF DICT parser and font structures. CFF curves are cubic, while zpb-ttf contours describe quadratic curves, so `contours` approximates each cubic with quadratic pieces within a quarter of a font unit; existing programs draw these fonts without changes. `cubic-contours` gives the exact outline of any glyph, TrueType or CFF, as move, line and cubic curve commands. A variable font, CFF2 or TrueType, can be set to an instance by axis tag and user value, and `variation-axes` lists the axes it has; outlines, advances, side bearings and font-wide metrics then follow that instance:

```lisp
(zpb-ttf:with-font-loader (font "Cantarell-VF.otf")
  (setf (zpb-ttf:variation-coordinates font) '(("wght" . 700)))
  (let ((glyph (zpb-ttf:find-glyph #\o font)))
    (values (zpb-ttf:advance-width glyph)
            (zpb-ttf:cubic-contours glyph))))
```

Note: CFF support is still a work in progress. For example, glyphs built with the old `seac` operator signal an error, and `avar` 2.0 mappings are not applied; `doc/font-loader-details.md` lists what is not supported.

WOFF and WOFF2 are web font formats: an OpenType font with its tables compressed, with zlib in WOFF and with Brotli in WOFF2 (WOFF2 also transforms the glyf, loca and hmtx tables to compress better). We only decompress. `open-font-loader` and `with-font-loader` recognise both formats and open them like any other font; `collection-index` picks a font of a WOFF2 collection. `otf:decode-woff1` and `otf:decode-woff2` unpack a file to a plain sfnt in memory (a WOFF2 collection becomes a TTC), and `otf:decode-woff1-metadata` and `otf:woff1-private-data` return a WOFF file's extended metadata and private data blocks:

```lisp
(zpb-ttf:with-font-loader (font "Inter-Regular.woff2")
  (zpb-ttf:family-name font))
```

The Brotli and zlib decoders are the `decompress` system, in `decompress/` at the top of the Runes repository. It has no dependencies and works on its own for any Brotli, zlib or raw deflate data held in memory; it has no streaming interface and does not compress. Its tests compare it with libbrotli and libz on round trips and on corrupted input. With the AVX-512 build of SBCL, the Brotli decoder runs at about 0.98–1.16× the speed of libbrotli (the reference C library) on the test fonts, and 1.1–1.2× on large streams; the zlib decoder at about 1.0–1.2× [zlib-ng](https://github.com/zlib-ng/zlib-ng). The fastest deflate decoder, [libdeflate](https://github.com/ebiggers/libdeflate), is still ahead: we are at roughly 0.7–0.9× of its speed. `decompress/README.md` has the measurements, and `decompress/doc/optimizations.md` what was tried.

The hottest loops of both decoders are written as SBCL VOPs, in assembly, because SBCL's register allocation was the limit for plain Lisp code: SBCL keeps a few registers for its own use, so more values are spilled to the stack than in C, and tagged integers cost extra shifts. I also tested a small SBCL patch that removes some of the needless register-to-register moves SBCL emits (about 300 for temporaries, against about 20 from GCC); the CPU eliminates most of those moves itself, and the shorter instruction stream did not help. Anyway, fonts are rarely loaded in a hot path, and they are mostly small files, so this is more than fast enough.

TrueType outlines and their glyph variations (`gvar`) are checked point by point against FreeType's (the industry baseline) at several instances of the test fonts. `doc/font-loader-details.md` has notes on what the loader does, how it differs from the original zpb-ttf, and what is not supported.

All systems load with ASDF, and `zpb-ttf` pulls in `runes/otf` automatically. The Makefile in this directory has targets for the everyday tasks: `make test` runs the test suite against the fonts in `tests/fonts/`, `make test-decompress` runs the Brotli and zlib decoder tests, `make bench` runs the benchmarks, `make codegen` regenerates the table code from the schemas, and `make install` links the Runes repository into Quicklisp's `local-projects`. The original zpb-ttf reference manual is in `doc/index.html`.

## Runes

runes/otf is part of the Runes project, which aims to bring everything needed to manage and load fonts to Common Lisp, in pure Lisp.

## License

The zpb-ttf part is distributed under the BSD 2-Clause license and the runes/otf part under the MIT license; `LICENSE` has both notices. `decompress` is MIT licensed, with its own `LICENSE`. The test fonts come with their own licenses, described in `tests/fonts/README.md`.
