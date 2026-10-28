# decompress: what we tried, and what it gained

A record of the work that took the decoder from about 26 MB/s to libbrotli's
speed, in the order it was done. Every idea is listed, including the ones that
lost, with the measurement that decided it.

All numbers are MB/s of *decoded output* on the same machine (AVX-512 CPU,
SBCL with the sb-simd-512 branch), best of 5 rounds after warm-up. Changes were
judged by same-session A/B runs, because run-to-run noise is about ±1–3% (and
up to ±10% on battery). "Kept" means the change is in the final decoder.

## Test data

| Name                  | Compressed → output   | Notes                                                   |
|-----------------------|-----------------------|---------------------------------------------------------|
| Red Hat Text          | 27.9 KB → 69 KB       | a real web font (WOFF2); the case that matters most     |
| Meslo Nerd Font Bold  | 1.18 MB → 2.42 MB     | converted with `woff2_compress`                         |
| Noto Serif Tangut     | 1.02 MB → 2.40 MB     | converted with `woff2_compress`                         |
| Noto Sans SignWriting | 0.87 MB → 4.02 MB     | converted with `woff2_compress`                         |
| src8m q9 / q11        | 1.58 / 1.42 MB → 8 MB | SBCL source, `brotli -q 9` / `-q 11`                    |
| src1m q9 / q5         | 208 / 218 KB → 1 MB   | first 1 MB of SBCL's `src/code`, `brotli -q 9` / `-q 5` |

`src1m` was called "the word list" for a long time: the command that built it
fell back to SBCL source because `/usr/share/dict/words` does not exist on the
machine. Its numbers are valid; only the name was wrong.

Data shape matters. The two SBCL source files are only 2–3% literals (the rest
is back-references and dictionary words); the fonts are 14–36% literals, all
context-modelled, and every stream uses a single context mode (Signed for all
four fonts, UTF8 for the source files).

## 1. Starting point: the original decoder

A straightforward RFC 7932 decoder: one function call per input bit, Huffman
codes walked bit by bit (zlib "puff" style), generic arithmetic, untyped
vectors, an adjustable output vector with a fill pointer.

|                  | src1m q9 | src8m q9 / q11 |
|------------------|----------|----------------|
| Original decoder | 21.6–27  | 30.9 / 32.7    |

The profile was generic `ASH`, `TWO-ARG-IOR`, hairy (non-simple) array access
and `READ-HUFF`.

## 2. Assessment: can AVX-512 speed it up?

Before writing code we checked what 512-bit vectors could do for Brotli.
sb-simd-512 offers AVX512F/BW/DQ/CD/VL/VBMI/VBMI2/IFMA/VNNI/BITALG/VPOPCNTDQ/FP16
and AVX10; the CPU has F/BW/CD/DQ/IFMA/VBMI/VL (no VBMI2).

| Part                                      | Vectorisable?                                  | Conclusion                   |
|-------------------------------------------|------------------------------------------------|------------------------------|
| Bit reader                                | no, serial                                     | needs a scalar rewrite       |
| Huffman decoding                          | no, each code's length depends on the previous | needs lookup tables          |
| Literal context                           | no, depends on the byte just decoded           | —                            |
| Back-reference copies                     | yes                                            | the only real SIMD candidate |
| Table building, move-to-front, transforms | partly                                         | too rare to matter           |

Conclusion: the decoder was 20–30× slower than C for scalar reasons; SIMD could
only help copies, and only after a scalar rewrite.

A one-stream-per-lane AVX-512 decoder was also discussed: it would help many
independent streams, not one large file, so it was not pursued. Threads would
give 2× more simply for that use.

## 3. The table-driven rewrite (six steps)

Rewritten after libbrotli's structure:

1. typed code and a preallocated simple output vector;
2. a 64-bit bit buffer refilled 8 bytes at a time;
3. two-level Huffman lookup tables (8-bit root);
4. precompiled dictionary transforms written straight into the output;
5. per-block state hoisted out of the literal loop;
6. a single end-of-input check per refill (the whole input is in memory, so
   libbrotli's separate "safe" streaming path collapses to that).

|          | src8m q9 / q11 | src1m q9 / q5 |
|----------|----------------|---------------|
| Original | 30.9 / 32.7    | 27.0 / 25.6   |
| Rewrite  | **645 / 615**  | **556 / 556** |

About **20–25×**. The first in-process comparison with libbrotli then (through
python-brotli) put it at 0.70–1.29× C.

## 4. AVX-512 copies and short-distance copies

| Change                                                               | Result       | Kept?         |
|----------------------------------------------------------------------|--------------|---------------|
| 64-byte AVX-512 copies for distance ≥ 64 and length > 32             | within noise | yes           |
| Short distances (< 8): copy one period, then 8-byte words            | within noise | yes           |
| Literal loop with a fixed tree when a block's context map is uniform | within noise | removed later |

On real data copies are not where the time goes, so the AVX-512 path never
showed a measurable gain; it is kept because it costs nothing.

## 5. Branch removal

wc4-style thinking: branches that test *unpredictable* data are expensive,
predictable ones nearly free.

| Change                                                                      | Result         | Kept? |
|-----------------------------------------------------------------------------|----------------|-------|
| Remove the uniform-context special case                                     | neutral        | yes   |
| Branch-free Huffman decode (always do the second lookup, select with masks) | **−20%**       | no    |
| Unconditional refill before every literal                                   | 0 to +3%       | yes   |
| Table-driven distance decoding with a branch-free ring update               | −5 to −8%      | no    |
| Table-driven distances with the original branchy ring update                | −2 to −5%      | no    |
| Typed table builder, bit-reversal lookup table                              | **+6 to +10%** | yes   |

The branch-free Huffman decode compiled cleanly (no generic arithmetic) but took
~35 instructions and two dependent loads, against ~12 instructions for the
branchy version whose branch (code longer than 8 bits) is almost never taken.
libbrotli keeps that branch too.

After this step: src8m 702–711, src1m 690–697.

## 6. Register pressure and representation

Instruction-level profiling with `perf` (sampled addresses mapped onto the
disassembly in the same process) showed the decode loop at ~75–83% of the time,
no calls or boxing in it, but ~15 values competing for ~12 free registers and
tagged-fixnum shifts (`SAR`/`SHL` by 1) around byte-array indexing.

| Change | Result | Kept? |
|---|---|---|
| Literal run as its own small function | **−5%** (a call and 6 returned values per command) | no |
| 62-bit `val` so it is always a fixnum | neutral, later found harmful (keeps `val` tagged) | reversed later |
| Fewer loop invariants (one context table per mode, one "context → table" vector per block type) | neutral | yes, simpler |
| Copies ≤ 16 bytes: both loads before either store; `p1`/`p2` from the copy source | **+2 to +3%** (store-forwarding stalls) | yes |
| `word` types on every index | **−12%** | no |
| `word` types on `p1`/`p2` only | identical machine code | no |
| SAP byte access in the literal loop + full 64-bit `val` + `bits`/`pos` as words with modular (`ldb`) arithmetic | **+4 to +5%** (156 → 111 tag shifts) | yes |
| `fp` as a word | −4% | no |
| `(the (integer 0 61) bits)` instead of `ldb` | re-tags `bits` (157 shifts) | no |

Lesson: in SBCL a type declaration states a range, not a representation. A
value is kept raw only when its *derived* type is genuinely word-sized and every
operation on it stays word-sized; anything that narrows it to fixnum range
(`the`, an `aref` result, mixing with fixnums) lets SBCL tag it again.

After this step: src8m 716–736, src1m 667–725.

## 7. Literal statistics and multi-symbol tables

Instrumenting the loop showed literals are only 2–3% of the output on the
source files, so decoding two literals per table lookup could not pay there. On
the fonts (measured later) literals are 14–36%, but every literal block is
context-modelled, so a second literal's table depends on the first and the idea
does not apply. Not built.

## 8. Allocation

The decoder allocated 2.3–2.4× the output size: the output buffer started at
4 KB and doubled (copying each time), and the result was copied again with
`subseq`. GC was only 0–2.6% of the time; the cost was the copying.

| Change                                                      | src8m q9 / q11    | src1m q9 / q5         | Kept? |
|-------------------------------------------------------------|-------------------|-----------------------|-------|
| Before                                                      | 716–736 / 718–724 | 711–725 / 667–681     |       |
| `%shrink-vector` instead of `subseq`, first buffer 4× input | 745–749 / 743–750 | 709–716 / 669–671     | yes   |
| `:size-hint` (output allocated once at the exact size)      | **773 / 774–777** | **741–750 / 694–697** | yes   |

SBCL arenas were considered: available in this build, but they could at most
save the remaining ~1.5% GC time, and the output cannot live in an arena.

## 9. The fonts, and per-stream setup

First WOFF2 measurement (with `:size-hint`):

|                           | Red Hat | Meslo | Tangut | SignWriting |
|---------------------------|---------|-------|--------|-------------|
| decompress               | 205     | 265   | 355    | 413         |
| libbrotli (python-brotli) | 322     | 261   | 306    | 462         |

Large fonts were near C; the small one was 0.64× C. Profiling it: ~40% of the
time was setup — reading prefix codes and building 94 Huffman tables (31 of 256
symbols, 5 of 704, 45 tiny code-length tables).

| Change | Red Hat | Kept? |
|---|---|---|
| Inline the header bit-reader functions, stack-allocate scratch arrays, skip the 256-slot subtable passes when no code exceeds 8 bits, 5-bit (32-entry) tables for code-length codes | 205 → **249** (+21%) | yes |
| `(safety 0)` in the table builder alone | none | yes (harmless) |
| Skip zero lengths in the counting loop | neutral | no |
| A pool holding all of a meta-block's tables, addressed by offset | 236–241 (slower: an extra add on every lookup) | no |
| Build tables from the used symbols only (libbrotli's method: collect symbol/length pairs while reading, counting sort) | 249 → **283** (+14%) | yes |

## 10. Removing run-time tests of fixed values

| Change | Result | Kept? |
|---|---|---|
| AVX-512 chosen at compile time instead of a run-time `simd` flag (one fewer live variable) | **+2 to +5%** everywhere (Red Hat 288) | yes |
| Literal run without the end-of-input test when `pos + 2k ≤ limit` | fonts ±1%, large files −2 to −4% | no |
| `(min window fp)` computed once per command | −1 to −3% (one more live value) | no |
| One test of (distance, length) per copy instead of five | +0 to +3% fonts, +1 to +8% source | yes |
| Loop variant for a window covering all output (wc4-style, generated by a macro) | **+1 to +3%** on every file | yes |
| Stage macros for the command loop | identical machine code (readability) | yes |
| Variants per `npostfix` (0–3) | +1 to +3% source, **−3 to −5% Red Hat** | no |
| Variants for `ndirect = 0` or one block type | never applies to the fonts (they use npostfix 1–2, ndirect 12–16, many block types) | not built |

## 11. Shortening the per-literal chain

Each literal costs a dependency chain: bit buffer → table index → table load →
shift → symbol → context lookup → next table. Ideas aimed at that chain:

| Change | Result | Kept? |
|---|---|---|
| `p1`/`p2` packed into one register with a combined context table (one load per literal) | **−9%** on fonts | no |
| The same with the packed value forced untagged (bit 63 kept set) | still −9%: the chain got longer (6 ALU steps instead of an `OR`) | no |
| Refill byte count `(* 8 k)` → shift (it compiled to `IMUL`) | first neutral; with the next item +0.5 to +1.5% | yes |
| Context parts precomputed into the decode-table entries, two loop variants | −4 to −6% (the carried value was spilled, extraction tagged) | no |
| The literal run as a hand-written x86-64 VOP, all values in chosen registers | −1 to −3% fonts, **−10%** large files (saving/restoring every register around each short run) | no |
| Subtable passes over only the needed root slots | Red Hat **+1.3 to +1.7%** | yes |

## 12. Smaller cuts

| Change                                                                                    | Result                                                          | Kept?            |
|-------------------------------------------------------------------------------------------|-----------------------------------------------------------------|------------------|
| Read the input in place (copy only its last 8 bytes, zero-padded)                         | small font neutral, large fonts +0.5 to +2%                     | yes              |
| Globals (`define-load-time-global`) instead of specials for all tables and the dictionary | +0.5 to +2% on large fonts                                      | yes              |
| Lexically bind the remaining specials read in loops                                       | noise; binding in the decode loop cost 1% (one more live value) | setup parts kept |
| Inline all small functions                                                                | noise (setup only)                                              | yes              |

## 13. Measurement corrections

* **python-brotli is not a fair C baseline.** It does not know the output size
  and decodes through the streaming API into a growing buffer. Switching to
  libbrotli's one-shot `BrotliDecoderDecompress` through `sb-alien` (decoding
  into a buffer of the known size, like `:size-hint`) made C considerably
  faster on large streams: the large-file ratios went from 1.15–1.22× to
  0.95–1.13×.
* **The benchmark timer.** `get-internal-real-time` ticks in milliseconds on
  this machine; with ~100 ms rounds that quantised results to ~1% steps
  (identical numbers in different runs gave it away). The benchmarks use
  `sb-ext:get-time-of-day` (microseconds) now.

## 14. The from-scratch rewrite

The decoder was rewritten as a decoder-only library (five files, ~1,060 lines),
SBCL-only, keeping the measured design and adding to the setup path: simple
prefix codes built from their 1–4 symbols, code-length counts gathered while
reading, code-length codes straight into 32-entry tables.

Testing against libbrotli (round trips through libbrotlienc at all qualities,
windows and modes; corrupted and truncated streams judged by libbrotlidec)
found eight format checks the earlier decoders did not make. They accepted
corrupt streams that libbrotli rejects (none crashed):

* nonzero padding before uncompressed or metadata blocks;
* nonzero padding after the last meta-block;
* incomplete or oversubscribed prefix codes;
* incomplete code-length codes;
* code-length repeats running past the alphabet;
* context-map zero runs past the end of the map;
* the reserved window size;
* duplicate symbols in simple prefix codes.

Speed is unchanged against the previous version (within noise), as estimated.

## Final result (Brotli, before the deflate work)

| Stream                | decompress | libbrotli (one-shot, sb-alien) | ratio         |
|-----------------------|-------------|--------------------------------|---------------|
| Red Hat Text          | 315         | 343                            | **0.92×**     |
| Meslo Nerd Font Bold  | 298         | 281                            | 1.06×         |
| Noto Serif Tangut     | 395         | 351                            | 1.13×         |
| Noto Sans SignWriting | 500         | 497                            | 1.01×         |
| src8m q9 / q11        | 882 / 840   | 888 / 882                      | 0.99× / 0.95× |
| src1m q9 / q5         | 803 / 736   | 872 / 804                      | 0.92× / 0.92× |

From the original decoder: about **30×** on the source files.

## What limits the Brotli decoder now

* **Large streams:** the per-literal dependency chain plus SBCL's register
  allocation in one large loop body. Every attempt to free registers or remove
  tag shifts *off* that chain was neutral, and anything that lengthened the
  chain lost. SBCL has no loop-invariant hoisting and does not allocate
  registers inner-loop-first; closing the last gap would need that compiler
  work.
* **Small streams:** building the prefix-code tables each stream needs.

## 15. Deflate / zlib, for WOFF 1.0

A deflate (RFC 1951) and zlib (RFC 1950) decoder was added next to the Brotli
one (`src/inflate.lisp`). chipz (`~/repos/cl/chipz`) has an inflater, but it is
a resumable streaming state machine, the opposite of the design that made the
Brotli decoder fast, so nothing was reused from it. Deflate uses the same
LSB-first bit order and canonical prefix codes as Brotli, so the bit reader,
the decode tables and the back-reference copy are shared. Errors are a
separate condition, `deflate-error`; both conditions inherit from
`decompression-error`.

Oracles: libz (zlib-ng 1.3.1 on this machine). Round trips through
`deflateInit2_` at every level, window size, memory level and strategy, as
zlib and raw deflate; corrupted zlib streams judged by `uncompress`, corrupted
raw deflate (no checksum, so the decoder's own checks decide) by `inflate`.
Every test passed from the first full run (18,044 default checks; 86,000 and
67,000 with other seeds).

Test data: two real WOFF 1.0 fonts (every compressed table decoded, each with
its origLength as size hint), the Meslo TTF and the 8 MB SBCL source as zlib
streams.

| Change                                                                                                                        | Red Hat WOFF  | Source Code Pro WOFF | Meslo .z    | src8m z6 / z9         | Kept?             |
|-------------------------------------------------------------------------------------------------------------------------------|---------------|----------------------|-------------|-----------------------|-------------------|
| First version                                                                                                                 | 341–343       | 222–224              | 265–266     | 482–484 / 492–494     |                   |
| Output buffer + 258 bytes (an exact size hint made the loop regrow and copy the whole buffer at the end) and AVX-512 Adler-32 | **458–464**   | **272–275**          | **332–335** | **761–767 / 795–804** | yes               |
| Refill only when needed (15 bits per symbol, 33 for a match) instead of before every symbol                                   | −2 to −4%     | +1.5%                | +3%         | +6 to +8%             | see below         |
| Short-distance copy period from a table instead of `(* d (ceiling 8 d))` (compiled to `IDIV`)                                 | neutral       | neutral              | neutral     | neutral               | yes (no division) |
| Reader: raw-word `val` slot and the classic 64-bit refill (`bits \|= 56`), so `val` is no longer tagged                       | +6 to +8%     | +6%                  | +9%         | +8 to +9%             | yes               |
| Reader: modular `bits` subtraction                                                                                            | neutral       | +1.5%                | neutral     | neutral               | yes               |
| Refill per symbol (re-tested with the new reader) vs only when needed                                                         | **+6 to +7%** | neutral              | −4%         | −7 to −8%             | yes, for fonts    |
| Refill below 48 bits                                                                                                          | −9%           | −5%                  | −4%         | −5%                   | no                |

Adler-32 was 25–28% of the time as a byte loop. With AVX-512: per 64-byte
block, `vpsadbw` adds the bytes, `vpmaddubsw` with weights 64…1 then
`vpmaddwd` with ones gives their weighted sum, and a third accumulator keeps
the earlier blocks' sums; reduced modulo 65521 every 64 KB. It now takes about
0.3%. One sb-simd quirk: `s16.32-madd` (`vpmaddwd`) is declared to return
`s16.32` although the instruction produces sixteen 32-bit sums; the result is
reinterpreted with `s32.16!`. The AVX2 `s16.16-madd` has the same
declaration, from upstream sb-simd.

The reader change (raw-word slot, 64-bit refill) also sped up the Brotli
decoder by 1–3% on most streams.

| Stream | decompress | libz (zlib-ng) | ratio |
|---|---|---|---|
| Red Hat Text, WOFF 1.0 | 515 | 880 | 0.59× |
| Source Code Pro, WOFF 1.0 | 290 | 579 | 0.50× |
| Meslo TTF, level 9 | 345 | 580 | 0.60× |
| src8m, level 6 / 9 | 798 / 830 | 1286 / 1352 | 0.62× / 0.61× |

From the first version that is +50% on the Red Hat WOFF and +65–70% on large
streams. zlib-ng's inflate is a heavily SIMD-tuned C decoder, a harder
baseline than libbrotli. Two-literals-per-lookup tables were considered (deflate
has no context modelling, so they would apply), but building a 2,048-entry
table per dynamic block would cost more than it saves on small font tables.

## 16. Deflate, second round: zlib-ng's ideas, and opmask registers

zlib-ng's source (`inffast_tpl.h`, `chunkset_avx512.c`) was the reference.
Statistics from instrumenting the block loop on the test data decided which of
its ideas mattered:

| Stream               | Literals | Symbols needing a second lookup (8-bit root) | Match distance < 8 | Match length ≤ 16 |
|----------------------|----------|----------------------------------------------|--------------------|-------------------|
| Red Hat Text WOFF    | 73%      | 17.5%                                        | 6%                 | 92%               |
| Source Code Pro WOFF | 80%      | 22.3%                                        | 17%                | 95%               |
| Meslo .z             | 71%      | 17.0%                                        | 2%                 | 95%               |
| src8m .z             | 31%      | 6.3%                                         | 0.4%               | 83%               |

Results in order (same-session A/B runs):

| Change                                                                                                                                                                   | Red Hat WOFF                      | Source Code Pro WOFF | Meslo .z            | src8m z6 / z9                   | Kept?   |
|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----------------------------------|----------------------|---------------------|---------------------------------|---------|
| Start of this round                                                                                                                                                      | 503                               | 286                  | 358                 | 823 / 856                       |         |
| Literal/length decode tables with a 9 / **10** / 11-bit root (`build-table` and `br-sym` generalised to any root size)                                                   | 522 / **524** / 497               | 313 / **334** / 309  | 391 / **414** / 419 | ≈ +1%                           | yes, 10 |
| Opmask: input position in a `k` register (`m8.64`)                                                                                                                       | SBCL still spilled it (see below) |                      |                     |                                 | no      |
| No calls in the loop: error checks record the message and leave the loop (a local inline `BERR`), signalled afterwards. Stack references 98 → 69                         | +1%                               | +2%                  | ±0                  | ±0                              | yes     |
| Opmask: `cap` and `limit` (loop invariants) in `k` registers                                                                                                             | −5%                               | −3%                  | −2%                 | −1.5%                           | no      |
| Up to three literals per refill, unrolled (zlib-ng's fast path)                                                                                                          | **+7%**                           | +4%                  | +6%                 | +4–5%                           | yes     |
| Decode-table entries carry a match's base and extra bits (`build-table`'s new MAP argument), so a match needs no second lookup and one range check rejects invalid codes | **+14%**                          | +6%                  | +12%                | **+18–21%**                     | yes     |
| AVX-512 VBMI pattern copy for distances < 16 (one `vpermb`, one 64-byte store, as zlib-ng's `chunkmemset`)                                                               | −0.5%                             | +1%                  | ±0                  | −1.5% (Brotli source −2 to −4%) | no      |
| Next root lookup issued before the refill (the refill's load overlaps the table load; the match path keeps 43 bits)                                                      | **+10%**                          | +2 to +6%            | +8%                 | +7–9%                           | yes     |
| Distance tables with a 9 / 10-bit root                                                                                                                                   | −1% / −4%                         | −2% / −7%            | ±0 / −2%            | ±0                              | no      |

From the start of the round: Red Hat WOFF 503 → **704** (+40%), Source Code
Pro 286 → **394** (+38%), Meslo 358 → **503** (+40%), src8m 823 / 856 →
**1052 / 1108** (+28–29%). Against zlib-ng: 0.80× / 0.68× / 0.86× /
0.82–0.83×, up from 0.59× / 0.50× / 0.60× / 0.61–0.62×.

**Opmask registers as storage.** The idea: keep values that SBCL would spill
in AVX-512 mask registers (`k1`–`k7`, sb-simd's `m8.64` type), moved with
`kmovq`. Only numbers can live there (the GC does not see `k` registers, so
no Lisp pointers). Two findings:

1. The input position, held as an `m8.64`, was *still* spilled (`KMOVQ [RBP-56],
   K1` and reloads). The cause was not a lack of registers: the loop contained
   calls (the error exits), and SBCL gives every value that is live across a
   call site a stack home, in any register class. Removing the calls from the
   loop kept the position in a general register on its own; that is the change
   kept above.
2. With the calls gone, what remained on the stack were read-only loop
   invariants. Moving two of them (`cap`, `limit`) into `k` registers made the
   loop longer (469 → 510 instructions) and 1.5–5% slower: for a value that
   does not change, an L1 load is off the critical path, while `kmovq` costs
   2–3 cycles and competes for the shuffle port.

So opmask registers did not help as spill space here; removing calls from hot
loops did.

**Brotli.** The same changes were tried on the Brotli loop: removing its error
calls (22 call sites → 0, stack references 545 → 411) measured −1% to +1% —
`dictionary-word` is still a real call in the loop, and inlining it brought
more calls in (`replace`, copy routines, generic arithmetic) and made the loop
larger; a 9- or 10-bit root for the command tables was neutral. None of it was
kept.

## Side experiment: CFF charstring tokenizing with AVX-512

Prompted by the same question (can SIMD help a serial format?), a prototype
tokenizer for CFF/CFF2 charstrings (`~/repos/skia/cff-simd/cff-tok.lisp`) was
written. Token length there depends only on the first byte, so token starts can
be found 64 bytes at a time: two `vpermi2b` lookups give each byte's length,
then pointer doubling with `vpermi2b` (J², J⁴ … J⁶⁴) gives the compacted list
of token offsets per block, with the next block's start carried over, as
wordcount carries its previous byte.

| Version (Noto Serif / Sans CJK)                | MB/s                                                           | vs scalar    |
|------------------------------------------------|----------------------------------------------------------------|--------------|
| Scalar                                         | 435–441                                                        | 1×           |
| AVX-512, before type declarations (1)          | ~760–840                                                       | ~1.7–2.5×    |
| AVX-512, typed                                 | 1750–1876                                                      | ~4.2×        |
| Wordcount's `POPCNT` VOP instead of `LOGCOUNT` | same (the loop is limited by the shuffle chain)                | —            |
| Two glyphs in flight                           | **2000–2160**                                                  | **4.6–4.9×** |
| A separate prefix pass, 1–4 glyphs in flight   | 1530–1860 (slower: it no longer overlaps with the vector work) | —            |

(1) a `U8.64` coercion call around every `vpermi2b`

Brotli itself cannot use this method: its dependency runs through decoded
output (the previous two bytes choose the next Huffman table) and Huffman codes
have no self-delimiting structure.

## SBCL codegen in `inflate-block`: allocator choice and an untagged output position

SBCL 2.6.9 master + sb-simd-512 (2026-10-26), Source Code Pro WOFF, best of 3×3
rounds, same session, `:force t`.

**Register allocator** (`sb-regalloc::*register-allocation-method*` while
compiling): `:greedy` 381–385, `:adaptive` (default) 379–386, `:iterative`
380 MB/s. No difference.

**Disassembly of `inflate-block`** (686 instructions):

|                            | count     | what                                                                                                                                                                             |
|----------------------------|-----------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| register-to-register `MOV` | 170 (25%) | mostly `MOV RAX,X; op RAX; MOV X,RAX`: a two-address op whose result is not packed into its argument's register (e.g. `OR bits,56`, `SHR val,cl`, `SUB bits,n`)                  |
| `SAR`/`SHL` by 1           | 55        | `fp` (R11) is a tagged fixnum: each literal store is `SAR; store; SAR; INC; LEA` to re-tag; `cap` is spilled *and* untagged at every room check; `len`/`dist`/`d` are tagged too |
| stack references           | 78        | loop invariants live on the stack: `ltab`, `dtab`, `in`, `limit`, `cap`, the SAP base; `len` stored and reloaded                                                                 |

**Output position as a raw address** (`op = sap-int(buf) + fp` of type `word`,
literal stores through `int-sap`, a pointer version of the copy): tag
conversions 55 → 32, but stack references 78 → 95 (`base`/`ocap` are new
live values) and the round-trip `MOV`s are unchanged. Measured 383–386 vs
381–383 MB/s; `perf stat` −2% instructions, +1% cycles. Neutral, not kept.

Disassemblies and the pointer variant: `doc/codegen/`.

Conclusion: the excess is not tagging or (at this size) the allocator choice;
it is two-address move coalescing (a quarter of the instructions) plus spilled
loop invariants under register pressure. An SBCL-side fix would target VOP
result targeting / move coalescing first, then live-range splitting; LICM would
not help.

## SBCL pass: retarget result moves after pack

An SBCL patch, not a change here; see `doc/codegen/mov-retarget/`. It removes
`MOV tmp,x; op tmp; MOV x,tmp` round trips (in `inflate-block`: 190 → 131
reg-reg MOVs, 685 → 626 instructions, stack references unchanged). It passes
SBCL's test suite and decompress's tests. Executed instructions drop 7–12%
across five inputs, but MB/s ranges from −3% to +1.4%: neutral, because Zen 5
eliminates reg-reg MOVs at rename and these loops are dependency-bound.

## 17. Deflate, third round: libdeflate's ideas

libdeflate (`lib/decompress_template.h`, `build_decode_table` in
`lib/deflate_decompress.c`) was the reference. Its decompressor has no SIMD of
its own (the x86 variant is the same C built with BMI2); its SIMD is in
Adler-32 and CRC-32. What it has is a table builder and an entry layout.
Profile at the start (Source Code Pro WOFF, sb-sprof): block loop 48%, table
setup (`build-table`, `deflate-table`, `read-dynamic-tables`) 45%.

SBCL 2.6.9 sb-simd-512 (`~/repos/sb-simd-512`), same-session A/B against the
previous commit, two alternating rounds each; MB/s:

| Change (cumulative)                                                                                                                                                                                       | Red Hat WOFF | Source Code Pro WOFF | Meslo .z | src8m z6 / z9         | Kept? |
|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------|----------------------|----------|-----------------------|-------|
| Start (previous commit)                                                                                                                                                                                   | 624–640      | 361–375              | 485–497  | 1046–1081 / 1113–1147 |       |
| Deflate's own table builder (`build-deflate-table`): codewords enumerated bit-reversed (libdeflate's `bsr` increment), root filled by doubling with block copies, no overlap checks (completeness is checked first) | +5–7%        | +7–9%                |          |                       | yes   |
| Length entries' bit count includes the extra bits: one shift consumes code and extra bits, which are read from the bit buffer saved before it (libdeflate's `saved_bitbuf`)                             | **+16%**     | **+22–24%**          | +7%      | +3–5%                 | yes   |
| The same for distances (6-bit count field, links at ≥ 32)                                                                                                                                                 | −0 to −1%    | −3 to −4%            | −3%      | −1%                   | no    |
| Next root lookup and refill before the match copy (libdeflate's order)                                                                                                                                    | −5%          | −1%                  | −2%      | −1 to −2%             | no    |
| `deflate-table` without its unused symbol/length arrays; code lengths decoded with the reader state in locals, one refill per code length                                                                  | +2%          | +3%                  | ±0       | ±0                    | yes   |
| **Result**                                                                                                                                                                                                | **754–758**  | **470–475**          | 524–528  | 1108–1114 / 1161–1174 | |

+18% / +26% on the two WOFF fonts, +8% Meslo, +3–5% src8m. Against libz
(zlib-ng) in the same session (874 / 584 / 579 / 1273 / 1332): 0.86× / 0.81× /
0.91× / 0.87× / 0.88×.

Both rejected changes add a value live across the match path (`saved` through
the distance decode, the preloaded entry across the copy); under SBCL's
register pressure in this loop that costs more than the shorter dependency
chain gains, as with the opmask experiments in section 16.

**No input check per refill (libdeflate's fast and generic loops).** The
output is one preallocated buffer and the input is read in place, as in
libdeflate, but every refill tested whether 8 more input bytes exist (else
it loads from the zero-padded tail). Tried: the symbol loop generated twice
from one macro, a fast copy whose refills have no test, entered while at
least 15 input bytes remain (an iteration refills at most twice, each
advancing at most 7 bytes), and the checked copy for the end of the input.
Same-session A/B against the commit above, two rounds each:

| Variant                                            | Red Hat WOFF | Source Code Pro WOFF | Meslo .z | src8m z6 / z9     |
|----------------------------------------------------|--------------|----------------------|----------|-------------------|
| Test `pos + 15 > limit` per iteration              | ±0           | −3.5%                | ±0 / +1% | ±0 to +3% / +3–4% |
| Precomputed `fast-end`, one compare per iteration  | ±0           | −3 to −6%            | +1–2%    | +2% / +2%         |

Not kept: large streams gain 1–4%, but the font case loses, probably to
the doubled loop code (the tests removed were well predicted, and the loop
still needs a bound value live).

**AVX2 Adler-32.** On a stock SBCL (no sb-simd-512) the AVX-512 code is
compiled out and the byte loop took 11% of a WOFF decode. With stock
sb-simd's AVX2 (`u8.32-sad`, `s16.16-maddubs`, `s16.16-madd`; libdeflate's
`adler32_avx2` scheme, weights 32…1), feature `:decompress-avx2`. Stock SBCL
2.6.9, same-session A/B, all of this round: Red Hat 502–545 → 650–659,
Source Code Pro 328–341 → 423–431.
