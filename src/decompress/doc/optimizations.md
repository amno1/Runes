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

| Part                              | Vectorisable? | Conclusion                   |
|-----------------------------------|---------------|------------------------------|
| Bit reader                        | no, serial    | needs a scalar rewrite       |
| Huffman decoding                  | no¹           | needs lookup tables          |
| Literal context                   | no²           | —                            |
| Back-reference copies             | yes           | the only real SIMD candidate |
| Tables, move-to-front, transforms | partly        | too rare to matter           |

1. Each code's length depends on the previous one.
2. It depends on the byte just decoded.

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

| Change                                      | Result            | Kept?          |
|---------------------------------------------|-------------------|----------------|
| Literal run as its own function¹            | **−5%**           | no             |
| 62-bit `val`, always a fixnum               | neutral²          | reversed later |
| Fewer loop invariants³                      | neutral           | yes, simpler   |
| Short copies: loads before stores⁴          | **+2 to +3%**     | yes            |
| `word` types on every index                 | **−12%**          | no             |
| `word` types on `p1`/`p2` only              | same machine code | no             |
| SAP literals, raw `val`, word `bits`/`pos`⁵ | **+4 to +5%**     | yes            |
| `fp` as a word                              | −4%               | no             |
| `(the (integer 0 61) bits)`                 | re-tags `bits`⁶   | no             |

1. A call and 6 returned values per command.
2. Later found harmful: it keeps `val` tagged.
3. One context table per mode, one "context → table" vector per block type.
4. Copies ≤ 16 bytes do both loads before either store and take `p1`/`p2` from
   the copy source: no store-forwarding stalls.
5. SAP byte access in the literal loop, a full 64-bit `val`, and `bits`/`pos`
   as words with modular (`ldb`) arithmetic: 156 → 111 tag shifts.
6. Instead of `ldb`: 157 tag shifts.

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

| Change                             | src8m q9 / q11    | src1m q9 / q5         | Kept? |
|------------------------------------|-------------------|-----------------------|-------|
| Before                             | 716–736 / 718–724 | 711–725 / 667–681     |       |
| `%shrink-vector`, 4× first buffer¹ | 745–749 / 743–750 | 709–716 / 669–671     | yes   |
| `:size-hint`²                      | **773 / 774–777** | **741–750 / 694–697** | yes   |

1. `%shrink-vector` instead of `subseq`, and a first buffer of 4× the input.
2. The output allocated once, at the exact size.

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

| Change                             | Red Hat              | Kept?          |
|------------------------------------|----------------------|----------------|
| Leaner header reading¹             | 205 → **249** (+21%) | yes            |
| `(safety 0)` in the table builder  | none                 | yes (harmless) |
| Skip zero lengths when counting    | neutral              | no             |
| One pool for all tables²           | 236–241              | no             |
| Tables from the used symbols only³ | 249 → **283** (+14%) | yes            |

1. Inline the header bit-reader functions, stack-allocate scratch arrays, skip
   the 256-slot subtable passes when no code exceeds 8 bits, and 5-bit
   (32-entry) tables for code-length codes.
2. All of a meta-block's tables in one pool, addressed by offset: slower, an
   extra add on every lookup.
3. libbrotli's method: collect symbol/length pairs while reading, then a
   counting sort.

## 10. Removing run-time tests of fixed values

| Change                                     | Result                              | Kept?     |
|--------------------------------------------|-------------------------------------|-----------|
| AVX-512 chosen at compile time¹            | **+2 to +5%** (Red Hat 288)         | yes       |
| No end-of-input test in literal runs²      | fonts ±1%, large −2 to −4%          | no        |
| `(min window fp)` once per command         | −1 to −3%³                          | no        |
| One (distance, length) test per copy⁴      | fonts +0–3%, source +1–8%           | yes       |
| Loop variant for a full window⁵            | **+1 to +3%** everywhere            | yes       |
| Stage macros for the command loop          | same code (readability)             | yes       |
| Variants per `npostfix` (0–3)              | source +1–3%, **Red Hat −3 to −5%** | no        |
| Variants for `ndirect = 0`, one block type | never applies⁶                      | not built |

1. Instead of a run-time `simd` flag: one fewer live variable.
2. When `pos + 2k ≤ limit`.
3. One more live value.
4. Instead of five.
5. A window covering all output (wc4-style, generated by a macro).
6. The fonts use npostfix 1–2, ndirect 12–16 and many block types.

## 11. Shortening the per-literal chain

Each literal costs a dependency chain: bit buffer → table index → table load →
shift → symbol → context lookup → next table. Ideas aimed at that chain:

| Change                                 | Result                          | Kept? |
|----------------------------------------|---------------------------------|-------|
| `p1`/`p2` packed in one register¹      | **−9%** on fonts                | no    |
| The same, forced untagged²             | still −9%                       | no    |
| Refill byte count as a shift³          | +0.5 to +1.5%                   | yes   |
| Context parts in the table entries⁴    | −4 to −6%                       | no    |
| Literal run as a hand-written VOP⁵     | fonts −1 to −3%, large **−10%** | no    |
| Subtable passes over needed slots only | Red Hat **+1.3 to +1.7%**       | yes   |

1. With a combined context table: one load per literal.
2. Bit 63 kept set. The chain got longer: 6 ALU steps instead of an `OR`.
3. `(* 8 k)` compiled to `IMUL`. Neutral at first; +0.5 to +1.5% together with
   the next item.
4. Context parts precomputed into the decode-table entries, two loop variants:
   the carried value was spilled and its extraction tagged.
5. An x86-64 VOP with all values in chosen registers; it saved and restored
   every register around each short run.

## 12. Smaller cuts

| Change                                 | Result                             | Kept?            |
|----------------------------------------|------------------------------------|------------------|
| Read the input in place¹               | small font ±0, large fonts +0.5–2% | yes              |
| Globals instead of specials²           | large fonts +0.5–2%                | yes              |
| Bind the remaining specials lexically³ | noise; −1% in the decode loop      | setup parts kept |
| Inline all small functions             | noise (setup only)                 | yes              |

1. Copy only its last 8 bytes, zero-padded.
2. `define-load-time-global` for all tables and the dictionary.
3. Specials read in loops. Binding one in the decode loop cost 1% (one more
   live value).

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

| Change                           | Red Hat       | Source Code Pro | Meslo       | src8m z6 / z9         | Kept?     |
|----------------------------------|---------------|-----------------|-------------|-----------------------|-----------|
| First version                    | 341–343       | 222–224         | 265–266     | 482–484 / 492–494     |           |
| Bigger buffer, AVX-512 Adler-32¹ | **458–464**   | **272–275**     | **332–335** | **761–767 / 795–804** | yes       |
| Refill only when needed²         | −2 to −4%     | +1.5%           | +3%         | +6 to +8%             | see below |
| Copy period from a table³        | neutral       | neutral         | neutral     | neutral               | yes       |
| Raw-word `val`, 64-bit refill⁴   | +6 to +8%     | +6%             | +9%         | +8 to +9%             | yes       |
| Modular `bits` subtraction       | neutral       | +1.5%           | neutral     | neutral               | yes       |
| Refill per symbol, re-tested⁵    | **+6 to +7%** | neutral         | −4%         | −7 to −8%             | fonts     |
| Refill below 48 bits             | −9%           | −5%             | −4%         | −5%                   | no        |

1. Output buffer + 258 bytes (an exact size hint made the loop regrow and copy
   the whole buffer at the end), and AVX-512 Adler-32.
2. 15 bits per symbol, 33 for a match, instead of a refill before every
   symbol.
3. The short-distance copy period from a table instead of `(* d (ceiling 8
   d))`, which compiled to `IDIV`. Kept: no division.
4. A raw-word `val` slot and the classic 64-bit refill (`bits |= 56`), so
   `val` is no longer tagged.
5. Refill per symbol against only when needed, re-tested with the new reader.
   Kept for the fonts.

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

| Stream               | Literals | Second lookup¹ | Distance < 8² | Length ≤ 16² |
|----------------------|----------|----------------|---------------|--------------|
| Red Hat Text WOFF    | 73%      | 17.5%          | 6%            | 92%          |
| Source Code Pro WOFF | 80%      | 22.3%          | 17%           | 95%          |
| Meslo .z             | 71%      | 17.0%          | 2%            | 95%          |
| src8m .z             | 31%      | 6.3%           | 0.4%          | 83%          |

1. Symbols needing a second lookup with an 8-bit root.
2. Share of matches.

Results in order (same-session A/B runs):

| Change                         | Red Hat        | Source Code Pro | Meslo    | src8m z6 / z9 | Kept? |
|--------------------------------|----------------|-----------------|----------|---------------|-------|
| Start of this round            | 503            | 286             | 358      | 823 / 856     |       |
| Literal root of 9 bits¹        | 522            | 313             | 391      | ≈ +1%         |       |
| Literal root of 10 bits¹       | **524**        | **334**         | **414**  | ≈ +1%         | yes   |
| Literal root of 11 bits¹       | 497            | 309             | 419      | ≈ +1%         |       |
| Opmask: input position in `k`² | spilled anyway |                 |          |               | no    |
| No calls in the loop³          | +1%            | +2%             | ±0       | ±0            | yes   |
| Opmask: `cap`, `limit` in `k`  | −5%            | −3%             | −2%      | −1.5%         | no    |
| Three literals per refill⁴     | **+7%**        | +4%             | +6%      | +4–5%         | yes   |
| Match data in table entries⁵   | **+14%**       | +6%             | +12%     | **+18–21%**   | yes   |
| VBMI pattern copy⁶             | −0.5%          | +1%             | ±0       | −1.5%⁷        | no    |
| Root lookup before the refill⁸ | **+10%**       | +2 to +6%       | +8%      | +7–9%         | yes   |
| Distance root of 9 / 10 bits   | −1% / −4%      | −2% / −7%       | ±0 / −2% | ±0            | no    |

1. `build-table` and `br-sym` generalised to any root size.
2. As an `m8.64`; SBCL still spilled it (see below).
3. Error checks record the message and leave the loop (a local inline `BERR`);
   the error is signalled afterwards. Stack references 98 → 69.
4. Unrolled, as zlib-ng's fast path.
5. Decode-table entries carry a match's base and extra bits (`build-table`'s
   new MAP argument), so a match needs no second lookup and one range check
   rejects invalid codes.
6. AVX-512 VBMI pattern copy for distances < 16: one `vpermb` and one 64-byte
   store, as zlib-ng's `chunkmemset`.
7. The Brotli source files: −2 to −4%.
8. The next root lookup issued before the refill, so the refill's load
   overlaps the table load; the match path keeps 43 bits.

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

| Version (Noto Serif / Sans CJK)  | MB/s          | vs scalar    |
|----------------------------------|---------------|--------------|
| Scalar                           | 435–441       | 1×           |
| AVX-512, untyped¹                | ~760–840      | ~1.7–2.5×    |
| AVX-512, typed                   | 1750–1876     | ~4.2×        |
| `POPCNT` VOP for `LOGCOUNT`²     | same³         | —            |
| Two glyphs in flight             | **2000–2160** | **4.6–4.9×** |
| Separate prefix pass, 1–4 glyphs | 1530–1860⁴    | —            |

1. Before type declarations: a `U8.64` coercion call around every `vpermi2b`.
2. Wordcount's `POPCNT` VOP instead of `LOGCOUNT`.
3. The loop is limited by the shuffle chain.
4. Slower: it no longer overlaps with the vector work.

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

|                            | Count     | What                     |
|----------------------------|-----------|--------------------------|
| register-to-register `MOV` | 170 (25%) | two-address round trips¹ |
| `SAR`/`SHL` by 1           | 55        | tag conversions²         |
| stack references           | 78        | spilled loop invariants³ |

1. Mostly `MOV RAX,X; op RAX; MOV X,RAX`: a two-address op whose result is not
   packed into its argument's register (e.g. `OR bits,56`, `SHR val,cl`, `SUB
   bits,n`).
2. `fp` (R11) is a tagged fixnum: each literal store is `SAR; store; SAR; INC;
   LEA` to re-tag. `cap` is spilled *and* untagged at every room check;
   `len`/`dist`/`d` are tagged too.
3. On the stack: `ltab`, `dtab`, `in`, `limit`, `cap` and the SAP base; `len`
   is stored and reloaded.

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

| Change (cumulative)          | Red Hat     | Source Code Pro | Meslo   | src8m z6 / z9         | Kept? |
|------------------------------|-------------|-----------------|---------|-----------------------|-------|
| Start (previous commit)      | 624–640     | 361–375         | 485–497 | 1046–1081 / 1113–1147 |       |
| Deflate's own table builder¹ | +5–7%       | +7–9%           |         |                       | yes   |
| One shift for length codes²  | **+16%**    | **+22–24%**     | +7%     | +3–5%                 | yes   |
| One shift for distances³     | −0 to −1%   | −3 to −4%       | −3%     | −1%                   | no    |
| Preload before the copy⁴     | −5%         | −1%             | −2%     | −1 to −2%             | no    |
| Lighter table setup⁵         | +2%         | +3%             | ±0      | ±0                    | yes   |
| **Result**                   | **754–758** | **470–475**     | 524–528 | 1108–1114 / 1161–1174 |       |

1. `build-deflate-table`: codewords enumerated bit-reversed (libdeflate's
   `bsr` increment), the root filled by doubling with block copies, no overlap
   checks (completeness is checked first).
2. A length entry's bit count includes its extra bits, so one shift consumes
   both; the extra bits are read from the bit buffer saved before the shift
   (libdeflate's `saved_bitbuf`).
3. The same for distance entries, with a 6-bit count field and subtable links
   at counts ≥ 32.
4. The next root lookup and the refill issued before the match copy, in
   libdeflate's order.
5. `deflate-table` without its unused symbol and length arrays; code lengths
   decoded with the reader state in locals, one refill per code length.

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

| Variant                               | Red Hat | Source Code Pro | Meslo    | src8m z6 / z9     |
|---------------------------------------|---------|-----------------|----------|-------------------|
| Test `pos + 15 > limit` per iteration | ±0      | −3.5%           | ±0 / +1% | ±0 to +3% / +3–4% |
| Precomputed `fast-end`, one compare   | ±0      | −3 to −6%       | +1–2%    | +2% / +2%         |

Not kept: large streams gain 1–4%, but the font case loses, probably to
the doubled loop code (the tests removed were well predicted, and the loop
still needs a bound value live).

**Fewer live values in the block loop.** The loop keeps about 14 values
live across an iteration (`val`, `bits`, `pos`, `fp`, `in`, `limit`, `r`,
`bs`, `buf`, `cap`, `ltab`, `dtab`, `sym`, `saved`) against libdeflate's
~9, and SBCL has roughly 12–13 general registers to give. Packing loop
invariants into shared registers was not tried: a spilled invariant is one
memory operand off the critical path, cheaper than unpacking it on every use
(the opmask result of section 16). Three ways to remove values instead,
same-session A/B against the previous commit, two rounds each:

| Change                        | Red Hat | Source Code Pro | Meslo | src8m z6 / z9         | Kept? |
|-------------------------------|---------|-----------------|-------|-----------------------|-------|
| Match copy through the SAP¹   | ±0      | ±0              | +1%   | +1–3% / +2–4%         | yes   |
| Distance table in one vector² | ±0      | −4 to −6%       | +2–3% | +2–4% / +3–5%         | no    |
| One input bit position³       | −6%     | **+8–10%**      | −2%   | −2 to −4% / −3 to −5% | no    |

1. `copy-match` copies through the output SAP that literals already use, so
   `buf` is not live in the loop.
2. The distance table at a constant offset (1344, above zlib's 1332-entry
   bound) in the literal table's vector: the offset folds into the address,
   so one register holds both tables.
3. `pos` and `bits` as one input bit position; a refill reloads `val` from
   it. With no bit count, the match path always refills.

(The joint table first copied the literal table in, which cost Red Hat 3%
and Source Code Pro 8%; building it in place fixed Red Hat only.) The bit
position puts the refill's load on the dependency chain of `val`: its
address now waits for every shift before it, where `pos` alone changed only
at refills. Source Code Pro, the stream with the most literals and the most
distances under 8, is the one that responds in the opposite direction to
both rejected changes; why is not known.

**Table setup, second pass.** After the changes above, setup was still 37%
of Source Code Pro's time and 23% of Red Hat's (sb-sprof, self + callees of
`read-dynamic-tables`), `build-deflate-table` alone 22% / 15%. Same-session
A/B against the previous commit, two rounds each:

| Change                          | Red Hat | Source Code Pro | Meslo | src8m z6 / z9          | Kept? |
|---------------------------------|---------|-----------------|-------|------------------------|-------|
| Reused, in-place tables¹        | +4–5%   | **+11–13%**     | ±0    | −0.5 to −2% / −0.5%    | yes   |
| Builder inlined per kind²       | −1–2%   | −2%             |       |                        | no    |
| Leaner `deflate-table`³         | ±0      | ±0              |       |                        | no    |

1. As libdeflate's decompressor object: the literal, distance and
   code-length tables are built in place in arrays reused across blocks and
   streams, so a block no longer allocates and zeroes its tables or copies
   them out of a stack scratch area. A stream takes a set from a lock-free
   pool (`compare-and-swap` on a list) and returns it at the end. A set is
   ~273 KB (two 34,816-entry arrays, a complete code keeps subtables within
   2^15 entries; the code-length table needs 256). Checked with 8 threads
   decoding concurrently: 19,200 decodes, no mismatches, 8 sets pooled.
2. One inlined copy of `build-deflate-table` per KIND, so the per-entry
   `case` on KIND folds away. Relative to the row above.
3. `deflate-table`'s counting loop at `(speed 3) (safety 0)`, and the
   code-length array not zero-filled. Relative to the first row.

Against libz in the same session (874 / 571): Red Hat 0.91×, Source Code
Pro 0.92×.

**libdeflate itself.** libdeflate 1.26 (built from `~/repos/skia/libdeflate`,
one decompressor object reused across calls), libz (zlib-ng) and this
decoder in one process, interleaved, same method as `bench-c.lisp`; MB/s:

| Stream          | libz        | libdeflate  | decompress  | ld / libz¹  | ours / ld¹  |
|-----------------|-------------|-------------|-------------|-------------|-------------|
| Red Hat         | 864         | 994         | 788         | 1.15        | 0.79        |
| Source Code Pro | 572         | 779         | 511         | 1.36        | 0.66        |
| Meslo           | 569         | 729         | 535         | 1.28        | 0.73        |
| src8m z6 / z9   | 1259 / 1322 | 1607 / 1723 | 1124 / 1201 | 1.28 / 1.30 | 0.70 / 0.70 |

1. `ld` is libdeflate, `ours` this decoder.

On this machine libdeflate is 1.15–1.36× zlib-ng, not the 2–3× quoted
against original zlib.

**Short distances, as libdeflate.** Distances below 8 copied one period byte
by byte (8–14 bytes), then 8-byte words. Same-session A/B, two rounds each:

| Variant                    | Red Hat | Source Code Pro | Meslo   | src8m z6 / z9 | Kept? |
|----------------------------|---------|-----------------|---------|---------------|-------|
| A. Distance 1 broadcast¹   | −0.5%   | −1 to −2%       | −0.5%   | ±0 / −0.5%    | no    |
| B. Stepping stores²        | −0.5%   | +1%             | +1–2%   | +2–4%         | yes   |
| C. Stepping, then words³   | ≈ B     | ≈ B             | ≈ B     | ≈ B, −1%      | no    |

1. One multiply by `#x0101010101010101`, then word stores; relative to the
   byte loop.
2. libdeflate's method for every distance below 8: overlapping 8-byte
   stores, each advancing by the distance. Each store's first DIST bytes are
   right and the rest is rewritten by the next.
3. B for the first period only, then 8-byte words at the period. Compared
   directly with B: within noise.

**BMI2 shifts.** Zen 5 has BMI2, and the fork's assembler already defines
`SHRX`/`SHLX`/`SARX`/`BZHI` (not exposed as VOPs or sb-simd functions). A
prototype VOP pair (`defknown` + `define-vop` in a scratch copy) made
`br-drop` and `br-refill` use `SHRX`/`SHLX`: in `inflate-block` 16 shifts by
`CL` became 13 `SHRX`/`SHLX` and 7 `CL` shifts, 640 → 607 instructions, 69 → 57
stack references, all tests passing. Measured against the same code without
it:

| Stream          | Change              |
|-----------------|---------------------|
| Red Hat         | ±1%                 |
| Source Code Pro | +1.4% / −1.6%       |
| Meslo           | −1.6%               |
| src8m z6 / z9   | −2% / −2%           |
| Brotli streams  | ±1–2%               |

Not kept, and not added to sb-simd-512. Like the mov-retarget patch: fewer
instructions, but Zen 5 removes the `MOV RCX` copies at rename and the loops
are bound by dependency chains. (A real version would also need a type
deriver: the VOP's word result widened derived types in Brotli's header
code, which then used generic arithmetic.)

**The SIMD-constant patch.** `simd-constant-fix` (`83319aeb7`) stops SBCL
re-propagating SIMD constants into loops. It applies to `adler32` only:
`inflate-block` has no SIMD variables. Before it, the AVX-512 loop reloaded
`ones` from the constant pool every 64 bytes and the spilled weights vector
from the stack. Now applied to sb-simd-512 (`9b7e3b5a7`): the constant-pool
load is gone, but all three constants (`zero`, the weights, `ones`) are
reloaded from the stack instead, because `adler32`'s outer loop makes full
calls (a generic `FLOOR`, `TRUNCATE` for `mod`, `multiple-value-call #'+` for
the lane sums) and SBCL gives every value live across a call a stack home.
Removing those calls would keep them in registers; Adler-32 is 0.3–1% of a
decode, so neither costs measurable time.

**AVX2 Adler-32.** On a stock SBCL (no sb-simd-512) the AVX-512 code is
compiled out and the byte loop took 11% of a WOFF decode. With stock
sb-simd's AVX2 (`u8.32-sad`, `s16.16-maddubs`, `s16.16-madd`; libdeflate's
`adler32_avx2` scheme, weights 32…1), feature `:decompress-avx2`. Stock SBCL
2.6.9, same-session A/B, all of this round: Red Hat 502–545 → 650–659,
Source Code Pro 328–341 → 423–431.

**A fourth batch of suggestions.** Each was A/B tested against the version
before it, two rounds, same session. Source Code Pro varies by up to ±10%
between sessions, so only the same-session pairs count.

| Change                         | Red Hat | Source Code Pro | Meslo  | src8m z6 / z9   | Kept |
|--------------------------------|---------|-----------------|--------|-----------------|------|
| Short copy for `dist >= len`¹  | ±0      | +4%             | +2%    | +1% / +1%       | yes  |
| 32-byte copy tier²             | ±0      | ±1%             | +1%    | ±0 / ±1%        | no   |
| One test per root literal³     | −5%     | −16%            | −5%    | −7% / −7%       | no   |
| Literal root of 11 bits⁴       | +1–3%   | +1%             | ±1%    | −2% / −2 to −5% | no   |
| Unmasked refill shift⁵         | ±1%     | ±0 to −5%       | ±1%    | −2% / −2 to −4% | no   |

1. `copy-match`'s two-word path took only `dist >= 16`; it now takes any
   match of up to 16 bytes with `dist >= len` (the bytes past `len` are slack
   that later output overwrites).
2. An AVX 32-byte loop for `32 <= dist < 64`, beside the 64-byte AVX-512 one.
3. A root entry below `256<<5` is always a literal (links, the end code and
   lengths are above), so a literal needs one test and no `saved` write; the
   rest went to a separate `slow` label. The loop got longer and slower.
4. Needs a new link marker: an 11-bit length code plus 5 extra bits makes
   16, the old marker. Section 16 found ≈ +1% as well.
5. `(the (integer 0 63) bits)` instead of `(logand bits 63)` in `br-refill`,
   with `low-bits` for the distance extra bits: 2 fewer instructions in
   `inflate-block`, but slower.

**One test per literal, retried in place.** To separate the idea from the
`slow` label: each slot tests the root entry once, and on failure runs the
old two-test code right there (no new variable, matches go straight to
`other`, subtable literals stay in the pass). Against HEAD:

| Stream          | HEAD      | One test  | Change      |
|-----------------|-----------|-----------|-------------|
| Red Hat         | 790–797   | 792–794   | ±0          |
| Source Code Pro | 586–593   | 503–512   | −13 to −15% |
| Meslo           | 551–554   | 543–544   | −1.5%       |
| src8m z6 / z9   | 1144–1148 / 1198–1224 | 1134–1136 / 1197–1210 | −1% / ±1% |

perf on a Source Code Pro run (whole process): instructions −1.7%,
branches −1%, branch misses +18% (+7.7 M), cycles +2.3%. The extra misses
alone, at 15–20 cycles each, account for most of the added cycles. The cost
is prediction, not loop size: before, "subtable link?" was a
rarely-taken branch and "literal?" a separate one, each with its own
history. The fused test makes one branch decide both, and a failed test
leads to two more branches at new sites with little history. Source Code
Pro, which has the most long literal codes (subtable links) of the test
streams, pays most. Not kept.

Checked and not needed: `saved` is a raw word (no allocation in the loop),
and the `(ldb (byte 64 0) …)` masks in `br-refill` compile to nothing. The
refill without an input check was tried before (the fast/checked loop
split, above).

**Two literals per lookup (ISA-L's multi-symbol table), measured first.**
Deflate has no context modelling, so section 7's objection does not apply.
`tests/bench/literal-pairs.lisp` counts, for each stream, the output bytes that could
come from greedy pairs of consecutive literals whose two codes fit in one
root lookup:

| Stream          | Literals | Pairs, 10-bit root | 11-bit | 12-bit |
|-----------------|----------|--------------------|--------|--------|
| Red Hat         | 27%      | 1.2%               | 3.0%   | 6.4%   |
| Source Code Pro | 36%      | 3.7%               | 5.3%   | 8.6%   |
| Meslo           | 28%      | 0.0%               | 0.4%   | 2.5%   |
| src8m z6        | 5.8%     | 0.0%               | 0.4%   | 1.5%   |
| src8m z9        | 3.8%     | 0.0%               | 0.0%   | 0.0%   |

(Percentages of output bytes.) Font literals have codes of mostly 8 bits or
more, so two rarely fit. Even a 12-bit root, which cost src8m 2–5% at
11 bits, would save the branch for under 9% of the output. Not built.

**CPU counters against libdeflate, before deciding on a VOP.**
`tests/bench/perf-compare.lisp` runs each decoder under `perf stat` on
~200 MB of output, minus a run that decodes nothing (so loading and
compiling cancel out). Per output byte:

| Stream          | Cycles, ours / ld | Instr., ours / ld | IPC, ours / ld | Misses/KB, ours / ld |
|-----------------|-------------------|-------------------|----------------|----------------------|
| Red Hat         | 6.55 / 5.08       | 32.8 / 13.7       | 5.00 / 2.69    | 3.0 / 3.6            |
| Source Code Pro | 8.75 / 5.87       | 41.4 / 16.0       | 4.73 / 2.72    | 27.1 / 8.7           |
| Meslo           | 8.97 / 6.82       | 31.3 / 12.5       | 3.49 / 1.83    | 135 / 123            |
| src8m z6        | 3.93 / 3.34       | 16.1 / 6.6        | 4.10 / 1.97    | 66 / 51              |
| src8m z9        | 3.97 / 2.70       | 16.1 / 6.6        | 4.04 / 2.45    | 60 / 44              |

sb-sprof splits our time: Source Code Pro 55% `inflate-block`, 35% table
setup (`read-dynamic-tables` and the builder); src8m z9 81%
`inflate-block`.

1. We execute 2.4× libdeflate's instructions, at 1.6–2.1× its IPC.
   libdeflate runs at an IPC of 2–2.7, so its time is the dependency
   chain. Ours runs at 4–5, near what Zen 5 sustains, so here the extra
   instructions do cost time. (Single `MOV`s are still free, as above:
   the cost is the 2.4× total, not any one instruction.)
2. On src8m nearly all of it is the symbol loop: about 13 instructions
   per byte against libdeflate's ~6. A loop written as one VOP, with
   libdeflate's instruction count, would be bound by the same chain, so
   up to libdeflate's cycles: +15% (z6) to +45% (z9) at best.
3. On the fonts a VOP touches only 55% of the time. Table setup (35% on
   Source Code Pro) needs its own work; it also has most of the extra
   branch misses (27 against 9 per KB).

**The symbol loop as one VOP.** `src/inflate-vop.lisp`: `inflate-block`'s
loop written in SBCL's x86-64 assembler, with the same steps (refill with
one load, three literals per refill, early root lookup, the one-shift
length, `copy-match`'s copy classes). Twelve registers hold the bit buffer,
bit count, input and output positions, the four bases and four scratch
values; only `cap` and the input limit stay on the stack. `SHRX`, `SHLX`
and `BZHI` (BMI2, in the sb-simd-512 assembler) shift by any register, so
`CL` is not reserved. Built only with `:decompress-avx512`.

It runs while at least 16 input bytes remain, so its refills need no
check, and stops at the top of an iteration (output full, input near its
end); the Lisp loop then finishes the block. Invalid codes and distances
stop it with a status, and `inflate-block` signals the same errors as
before. The tests pass unchanged.

| Stream          | HEAD        | VOP         | Change   |
|-----------------|-------------|-------------|----------|
| Red Hat         | 787–790     | 842–857     | +7–8%    |
| Source Code Pro | 560–589     | 628–629     | +7–12%   |
| Meslo           | 544–549     | 607–612     | +11–12%  |
| src8m z6 / z9   | 1134–1150 / 1206–1221 | 1262–1278 / 1324–1351 | +11% / +10% |

Counters per byte (`perf-compare.lisp`), ours / libdeflate:

| Stream          | Cycles      | Instructions | IPC         |
|-----------------|-------------|--------------|-------------|
| Red Hat         | 6.42 / 4.86 | 18.7 / 13.7  | 2.91 / 2.81 |
| Source Code Pro | 7.92 / 6.94 | 26.0 / 16.0  | 3.29 / 2.30 |
| Meslo           | 8.21 / 7.00 | 15.7 / 12.5  | 1.91 / 1.78 |
| src8m z6 / z9   | 3.79 / 2.91, 3.28 / 2.54 | 8.1 / 6.6 | 2.13 / 2.26, 2.45 / 2.59 |

Instructions per byte on src8m fell from 16.1 to 8.0 (libdeflate 6.6), and
IPC fell to libdeflate's: the loop is now bound by its dependency chain,
as libdeflate's is. What is left: more branch misses than libdeflate (64
against 45 per KB on src8m), and on the fonts the table setup. libdeflate's
numbers vary by up to 15% between runs (Source Code Pro 5.87 earlier, 6.94
here).

## 18. After the VOP: re-testing the register-bound rejections

The VOP removed SBCL's register allocation from the symbol loop, so ideas
rejected earlier because they added a live value were tried again. Each
was A/B tested against the version before it, two rounds, same session.

**Where the font time goes now.** `tests/bench/profile.lisp` now takes
zlib and WOFF streams (all tables, sampled every 0.2 ms):

| Share of time           | Source Code Pro | Red Hat |
|-------------------------|-----------------|---------|
| `inflate-block`¹        | 60%             | 72%     |
| Table setup²            | 34%             | 18%     |
| `lisp_alloc`            | 2.2%            | ≈ 0     |
| Output fill / shrink    | 2.6% / 0.1%     | 0.4% / 0.3% |

1. The VOP and the Lisp loop that finishes each block.
2. `read-dynamic-tables` with everything it calls (`deflate-table`,
   `build-deflate-table`).

Allocation is noise, so caching the reader's 32-byte tail vector was not
done. The font gap left is table setup.

**Results:**

| Change                           | Red Hat | Source Code Pro | Meslo  | src8m z6 / z9 | Kept |
|----------------------------------|---------|-----------------|--------|---------------|------|
| VOP in a function of its own³    | −2 to +1% | −3 to +1%     | ±0     | ±1%           | yes  |
| One shift for distances⁴         | ±0      | −1%             | ±1%    | ±0 / ±1%      | no   |
| Preload before the copy⁵         | ±1%     | ±1%             | ±1%    | −1 to −3% / −1% | no |
| Fast/slow split⁶                 |         |                 |        |               | (already) |

3. Changing the distance decode in the Lisp loop made `inflate-block`
   fail to compile: the VOP's twelve registers no longer fitted around the
   call. `inflate-fast` now holds the call alone; it runs once per block.
   (The VOP and `defknown` are in `eval-when`, so the same file can
   compile the function with them.) Not a speed change but a maintenance
   hedge: the VOP's twelve registers constrain whatever is live around its
   call, so with the call isolated, the Lisp loop can change without
   re-checking that the VOP still fits.
4. As for lengths: the code length at bit 21, the bit count including the
   extra bits, the extra bits read from the buffer saved before the shift.
   A root entry can then count 8 + 13 = 21 bits, so distance subtable links
   moved to 24. It removes no step from the chain: the extra bits still
   wait for the shift of the saved buffer.
5. In the short-copy path (distance dead once the source is known) the
   next root entry was loaded before the two stores, entering the next
   iteration after its lookup.
6. The VOP already is libdeflate's fast loop: it checks the input once per
   iteration (perfectly predicted), its refills have no check, and the
   Lisp loop is the slow tail. Nothing to split.

**What the VOP did and did not change.** It gained 7–12% by halving the
instructions (no tag shifts, no stack traffic), and moved the loop from
throughput-bound to bound by its dependency chain. Neither re-tested idea
shortens that chain, so neither pays: their earlier losses were partly
register pressure, but without it they are neutral, not gains. Rejections
that still stand for other reasons: the 32-byte copy tier and the
two-literal table (the data: few matches at those distances, few literal
pairs that fit), the 11-bit root and the one-test literal (table setup and
branch prediction). The next gains are in what the chain does per symbol
and in table setup, not in what holds the values.

## 19. Table setup on the fonts

The fonts' blocks are small: Source Code Pro has 45 dynamic blocks in
138 KB of output (about 3 KB each), and each builds three tables. Their
alphabets are not small, though: a literal table uses about 195 of 286
symbols and puts about 6 codes in subtables (Red Hat: 216 and 26). So a
small-alphabet fast path does not apply. perf samples mapped onto the
builder's disassembly found the cost instead: about a quarter of
`build-deflate-table` was its counting sort, where `sorted[offs[l]] = s;
offs[l]++` reads `offs[l]` right after the previous symbol stored it, a
chain through memory (store forwarding) per symbol. `deflate-table`'s
`count[l]++` loop has the same chain.

| Change                          | Red Hat   | Source Code Pro | Meslo  | src8m z6 / z9 | Kept |
|---------------------------------|-----------|-----------------|--------|---------------|------|
| Sort by length masks¹           | +1%       | +3–4.5%         | ±1%    | ±1% / ±1%     | yes  |
| 16-bit `integer-length` operand² | −1 to +1% | +0.5–2%        | ±1%    | ±0 / ±0.5%    | no   |
| Count by mask popcounts³        | +1–2%     | +4.5–5%         | ±0     | ±1% / +0.5%   | yes  |
| No fill for zero runs⁴          | +0.5–2%   | +0.5–2.5%       | ±1%    | ±0.5% / +0.5% | yes  |

1. With AVX-512, one compare of 64 code lengths against L gives the mask
   of the symbols of length L; reading each mask's bits in turn (for L
   from 1 up, chunks in order) lists the symbols in canonical order with a
   counter in a register. 8–10 lengths × 5 chunks of compares per literal
   table. The loads read up to 63 bytes past the lengths (masked off), so
   the code-length and distance length arrays grew to 64 bytes; arrays
   without that room (the fixed tables) keep the counting sort.
2. Removes the sign handling around `BSR` in the next-codeword step.
3. `count[l]` as the sum of popcounts of the same masks, 15 lengths × 5
   chunks; the symbol of a one-symbol code is found by a scan.
4. The code-length decoder called `fill` (out of line) for each repeat.
   The lengths array starts zeroed, so zero runs (17, 18) are skipped, and
   a repeat of the previous length (16, 3–6 bytes) is stored inline.

Table setup went from 34% to 27% of Source Code Pro's time. Most of the
rest is the builder's per-entry loop, a chain through the codeword
(next-codeword, then the store), which libdeflate's builder has too.
