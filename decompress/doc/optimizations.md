# decompress: what is tried, and what is gained

A record of the work that took the decoder from about 26 MB/s to the same
speed as libbrotli or better, in the order it was done. Some time ago someone
posted a link to their [Brotli decoder/encoder
written in pure Common Lisp](https://github.com/modus-lisp/brotli-pure). While I
hoped it would be possible to use it and build on it, it turned out it is really
bad, code-wise and performance-wise. It looks as if someone told Claude to
write a library and Claude did, without any supervision. It was not usable to
me, so I rewrote every part I use from scratch, but I do use their test data.
Their speed, about 26 MB/s, is where this started, but the real yardstick is
libbrotli, Google's original and, as far as I know, the fastest implementation
of Brotli.

Every idea is listed, including the ones that lost, with the measurement that
decided it. I am not sure how to present this; it has been a week of work and
tests, so now when I am done, and about to publish this, I would like to present
it so it is readable both for those who would just like to glance over results,
and for those who are interested to read about "war stories". I have tried to
find a middle by presenting main ideas in a table with result and whether it is
implemented or not. I have also expanded on those details I personally have
to remember and on some I believe are interesting to read about. I hope you like
this style. Note also, the entire work is done with help of LLM, however, while
I do use LLMs to analyze, write and refactor the code for me, I also write
myself both text and the code. This document is mostly manually written by hand,
but nowadays we have a luxury to enter both code and the text into a LLM and
have it factually checked. So it somehow becomes a mix of both human and machine
written. Anyhow, this is not "click a button and make a wish" style of document.

Since I also needed zlib's inflate for WOFF 1.0 fonts, I added that too. I
hoped to reuse more between the two; they share some parts, but not much. The
references for inflate, for design and measurement, were zlib-ng and
libdeflate.

When it comes to code, you can assume that I type-declare all variables and do
as much as I can to keep all important variables untagged (unboxed) in registers
so SBCL can work with them directly. I will not type it throughout this
document, but all arithmetic and logic is performed efficiently in SBCL as far
as I am able to write it.

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
fell back to SBCL source because `/usr/share/dict/words` does not exist on my
machine (CachyOS Linux). The numbers are valid; only the name was wrong in some
places.

Data shape matters. The two SBCL source files are only 2–3% literals (the rest
is back-references and dictionary words); the fonts are 14–36% literals, all
context-modelled, and every stream uses a single context mode (Signed for all
four fonts, UTF8 for the source files).

## 1. Starting point: the original decoder

A straightforward RFC 7932 (Brotli) decoder: one function call per input bit,
Huffman codes walked bit by bit (zlib "puff" style), generic arithmetic, untyped
vectors, an adjustable output vector with a fill pointer.

|                  | src1m q9 | src8m q9 / q11 |
|------------------|----------|----------------|
| Original decoder | 21.6–27  | 30.9 / 32.7    |

The profile was generic `ASH`, `TWO-ARG-IOR`, hairy (non-simple) array access
and `READ-HUFF`.

## 2. Assessment: can AVX-512 speed it up?

Before writing code I checked what 512-bit vectors could do for Brotli.
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

A one-stream-per-lane AVX-512 decoder was also considered: it would help many
independent streams, not one large file, so it was not pursued. If we used
multiple threads we could probably get 2× more speed in that particular
scenario.

A remark, if you are not familiar with Brotli: it is the most disgusting x86
anti-pattern, invented on purpose to sabotage any parallel hardware your machine
might have! Just kidding, of course, but because of its design Brotli is very
hard, bordering on impossible, to parallelize: very little of the work can run
in parallel. After wrestling with it for more than a week, I looked up the
history behind it. Brotli first appeared in 2013, as the compressor for WOFF2
web fonts, and was released as a general-purpose format in 2015. Google wanted
pages to render faster on phones on slow mobile networks: text often cannot be
drawn before its font is downloaded, so fonts are among the first assets a page
needs.

Its main technique is context modelling: the previous two bytes choose which of
several Huffman tables decodes the next literal. It is also a stream-oriented
format. My guess is that it was tuned for Arm CPUs, which dominate phones,
because the decoder keeps about 15 values live in its inner loop. x86-64 has 16
general registers, and the stack pointer takes one, so even C, C++ and Rust
compilers must spill some of them to the stack. It is worse for us: SBCL
reserves three more registers for its own state, which leaves 12 for our code,
so we spill more than C does. Arm64 CPUs have 31 general registers, so spilling
to stack is less of a problem there.

Since both C and SBCL spill on x86-64, we can reach the same speed as libbrotli.
zlib inflate needs far fewer registers, so there a C compiler can keep more in
registers than we can, and it shows in the numbers.

## 3. The table-driven rewrite (six steps)

The Brotli decoder is written after libbrotli's structure:

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
| Rewrite  | 645 / 615      | 556 / 556     |

## 4. AVX-512 copies and short-distance copies

| Change                                                               | Result       | Kept?         |
|----------------------------------------------------------------------|--------------|---------------|
| 64-byte AVX-512 copies for distance ≥ 64 and length > 32             | within noise | yes           |
| Short distances (< 8): copy one period, then 8-byte words            | within noise | yes           |
| Literal loop with a fixed tree when a block's context map is uniform | within noise | removed later |

On real data, copying is not where the time goes, so the AVX-512 path never
showed a measurable gain. It is kept because it costs nothing.

The literal loop is the inner loop that decodes a command's literal bytes.

Brotli output is a sequence of insert-and-copy commands. Each command says
"insert N literal bytes, then copy M bytes from earlier output". The literal
loop is the part that decodes those N bytes one at a time.

## 5. Branch removal

Branches that test *unpredictable* data are expensive, predictable ones nearly
free. Removing a branch helps only when it was mispredicted; otherwise it
just adds work, as the second row shows.

| Change                                                                      | Result     | Kept? |
|-----------------------------------------------------------------------------|------------|-------|
| Remove the uniform-context special case                                     | neutral    | yes   |
| Branch-free Huffman decode (always do the second lookup, select with masks)² | −20%      | no    |
| Unconditional refill before every literal³                                  | 0 to +3%   | yes   |
| Table-driven distance decoding with a branch-free ring update               | −5 to −8%  | no    |
| Table-driven distances with the original branchy ring update                | −2 to −5%  | no    |
| Typed table builder, bit-reversal lookup table                              | +6 to +10% | yes   |

2. A branch-free Huffman decoder without generic arithmetic (everything was
   typed) was implemented, but it took ~35 instructions and two dependent loads,
   against ~12 instructions for the branchy version whose branch (code longer
   than 8 bits) is almost never taken.  After the branch-free wordcount, I
   really had expectations for this one, but it turned out slower.

3. Decoding a literal takes up to 15 bits from the bit buffer val. Before
   decoding, the decoder must be sure the buffer holds at least 15 bits. There
   are two ways:

   Conditional, the earlier way: (br-need 15), which refills only if bits < 15
   (`br-need` in `src/reader.lisp`). That’s a branch on the bit count, and the bit count
   changes with every symbol’s code length, so it follows the data. Whether a
   refill is needed comes in an irregular pattern, so the CPU mispredicts it
   fairly often, at 15–20 cycles each time.  Unconditional, the current way:
   always do (br-refill) before each literal. You can see it in the literal
   loop: `(br-sym … (br-refill))` in `command-loop`.

   The unconditional refill is cheap because it has no branch of its own (apart
   from the input-end check, which is almost always false and well
   predicted). It loads 8 bytes, shifts them past the bits already there, ORs
   them in, advances pos by (63 − bits) / 8 bytes, and sets bits |= 56. If the
   buffer was already nearly full, that advance is 0 and the work is wasted, but
   a few ALU instructions cost less than a mispredicted branch. Afterwards there
   are always at least 56 bits, more than any literal needs.

   This is the "branchless-style" thinking: branches on unpredictable data are
   expensive, while straight-line work is cheap. Here it paid a little (0 to
   +3%) while in the branch-free Huffman decode (−20%) it didn't: there the
   branch was predictable, so removing it actually added work.

## 6. Register pressure and representation

Instruction-level profiling with `perf` (sampled addresses mapped onto the
disassembly in the same process) showed the decode loop at ~75–83% of the time,
no calls or boxing in it, but ~15 values competing for ~12 free registers and
tagged-fixnum shifts (`SAR`/`SHL` by 1) around byte-array indexing.

| Change                                      | Result            | Kept?          |
|---------------------------------------------|-------------------|----------------|
| Literal run as its own function¹            | −5%               | no             |
| 62-bit `val`, always a fixnum               | neutral²          | reversed later |
| Fewer loop invariants³                      | neutral           | yes, simpler   |
| Short copies: loads before stores⁴          | +2 to +3%         | yes            |
| `word` types on every index                 | −12%              | no             |
| `word` types on `p1`/`p2` only              | same machine code | no             |
| SAP literals, raw `val`, word `bits`/`pos`⁵ | +4 to +5%         | yes            |
| `fp` as a word                              | −4%               | no             |
| `(the (integer 0 61) bits)`                 | re-tags `bits`⁶   | no             |

1. A call and six returned values per command.
   The literal run was moved out of the command loop into a separate function,
   so each command made one function call to it.  That function changed six
   pieces of state, and they all had to be brought back to the loop as return
   values.

   In SBCL, returning several values goes through the multiple-values
   convention: they’re moved into fixed registers, or onto the stack beyond the
   first few, and then moved back where the loop keeps them. Paying for that
   once per command cost more than the smaller loop saved, so the change lost 5%
   and wasn't kept.

2. Later found harmful: it keeps `val` tagged.
3. One context table per mode, one "context → table" vector per block type.

   How a literal's Huffman table is chosen: each literal block type has a
   context mode (LSB6, MSB6, UTF8 or Signed), which turns the last two output
   bytes, `p1` and `p2`, into a context number from 0 to 63; the context map
   then picks a Huffman table for (block type, context number). The RFC gives
   each mode its own formula: for LSB6 p1 & 0x3f, for MSB6 p1 >> 2, for UTF8
   lut0[p1] | lut1[p2], and for Signed (lut2[p1] << 3) | lut2[p2]. We do
   differently:

   - One context table per mode (`+ctx-mode-luts+`, 512 bytes): the mode's
     shifts and masks are built in, so every mode is `cid = T[p1] | T[256 + p2]`,
     two loads and an OR.
   - One context-to-table vector per block type (`lslices`, 64 entries): built
     from the context map when the meta-block starts, so slot `cid` holds the
     Huffman table itself, and choosing it is one load.

   The literal loop keeps just these two, which change only at a literal block
   switch; the mode, the context map and the list of tables stay out of it.
   Fewer loop invariants means less pressure on SBCL's ~12 free registers. That
   one also took quite long to come up with; I have tried to figure out some way
   to remove number of live variables and tested lots of various ideas, to keep
   them in zmm registers, in mask registers, as a struct and just load the
   struct pointer, but at the end of the day they all have to be brought in to
   decide how to read that byte :).

4. Copies ≤ 16 bytes do both loads before either store and take `p1`/`p2` from
   the copy source: no store-forwarding stalls.
5. SAP byte access in the literal loop, a full 64-bit `val`, and `bits`/`pos`
   as words with modular (`ldb`) arithmetic: 156 → 111 tag shifts.

   Background: SBCL stores small integers (fixnums) shifted left by one bit,
   with a 0 tag bit at the bottom, so that the garbage collector can tell
   numbers from pointers. 5 is stored as 10. That costs nothing for adding and
   comparing, but whenever a fixnum is used as a plain machine number (a byte
   offset into memory, a shift count, a mask), SBCL must first untag it (SAR
   reg,1), and often tag the result again afterwards (SHL reg,1 or a LEA). In
   the Brotli loop, those tag shifts sat on the per-literal dependency chain.

   SBCL keeps a value raw (an untagged 64-bit word in a register) only when its
   type is word-sized and every operation on it stays word-sized. The change did
   three things to get there:

   SAP byte access in the literal loop: instead of (aref buf fp), the loop reads
   and writes bytes through sap-ref-8 on a system-area pointer, a raw memory
   address. A SAP offset is a raw integer, so fp, p1 and p2 no longer had to be
   fixnums. You can see it in the current code, with the comment "a SAP offset
   is a raw integer, so FP/P1/P2 stay untagged" in `command-loop`.  A full
   64-bit val: the bit buffer was typed as a full (unsigned-byte 64). That
   doesn't fit in a fixnum, so SBCL can't tag it and keeps it raw. The earlier
   62-bit version fit in a fixnum, so SBCL tagged it, which note 2 of the same
   table calls harmful.  bits/pos as words with modular arithmetic: arithmetic
   is wrapped in (ldb (byte 64 0) …). That tells SBCL the result is taken modulo
   2⁶⁴, so it can use a plain machine ADD/SUB on raw words with no overflow
   check and no tagging.

   156 → 111 is the number of tag shifts (SAR/SHL by 1) in the disassembled
   loop, before and after. The change gave +4–5%.

6. Instead of `ldb`: 157 tag shifts.

   The last row is about the same thing, from the other direction.

   Instead of keeping bits raw with modular arithmetic (ldb, as in note 5), this
   tried declaring its range with (the (integer 0 61) bits). That is true, since
   the bit count never exceeds 61, and it looks like more information for the
   compiler. But 0–61 fits in a fixnum, so SBCL went back to storing bits
   tagged, and the untag and re-tag shifts came back: 157 tag shifts, against
   111 with ldb and 156 before note 5's change. That gave back everything note 5
   had gained, so it wasn't kept.
   
   Lesson learned: in SBCL a type declaration states a range, not a
   representation. A value is kept raw only when its *derived* type is genuinely
   word-sized and every operation on it stays word-sized; anything that narrows
   it to fixnum range (`the`, an `aref` result, mixing with fixnums) lets SBCL
   tag it again.

## 7. Literal statistics and multi-symbol tables

Instrumenting the loop showed literals are only 2–3% of the output on the source
files, so decoding two literals per table lookup would not give much there. On
the fonts (measured later) literals are 14–36%, but every literal block is
context-modelled, so a second literal's table depends on the first and the idea
does not apply, so I didn't implement that one.

## 8. Allocation

The decoder allocated 2.3–2.4× the output size: the output buffer started at
4 KB and doubled (copying each time), and the result was copied again with
`subseq`. GC was only 0–2.6% of the time; the cost was the copying.

| Change                             | src8m q9 / q11    | src1m q9 / q5     | Kept? |
|------------------------------------|-------------------|-------------------|-------|
| Before                             | 716–736 / 718–724 | 711–725 / 667–681 |       |
| `%shrink-vector`, 4× first buffer¹ | 745–749 / 743–750 | 709–716 / 669–671 | yes   |
| `:size-hint`²                      | 773 / 774–777     | 741–750 / 694–697 | yes   |

1. `%shrink-vector` instead of `subseq`, and a first buffer of 4× the input.
2. The output allocated once, at the exact size.

SBCL arenas were considered, since they are available in this build (and any
modern SBCL), but that would need a major rewrite and could at most save the
remaining ~1.5% GC time, and the output cannot live in an arena, so that wasn't
tested either.

## 9. The fonts, and per-stream setup

First WOFF2 measurement (with `:size-hint`):

|            | Red Hat | Meslo | Tangut | SignWriting |
|------------|---------|-------|--------|-------------|
| decompress | 205     | 265   | 355    | 413         |
| libbrotli  | 322     | 261   | 306    | 462         |

Large fonts were near C; the small one was 0.64× C. Profiling it: ~40% of the
time was setup: reading prefix codes and building 94 Huffman tables (31 of 256
symbols, 5 of 704, 45 tiny code-length tables).

| Change                             | Red Hat          | Kept?          |
|------------------------------------|------------------|----------------|
| Leaner header reading¹             | 205 → 249 (+21%) | yes            |
| `(safety 0)` in the table builder  | none             | yes (harmless) |
| Skip zero lengths when counting    | neutral          | no             |
| One pool for all tables²           | 236–241          | no             |
| Tables from the used symbols only³ | 249 → 283 (+14%) | yes            |

1. Inline the header bit-reader functions, stack-allocate scratch arrays, skip
   the 256-slot subtable passes when no code exceeds 8 bits, and 5-bit
   (32-entry) tables for code-length codes.
2. All of a meta-block's tables in one pool, addressed by offset: slower, an
   extra add on every lookup.
3. libbrotli's method: collect symbol/length pairs while reading, then do a
   counting sort.

## 10. Removing run-time tests of fixed values

| Change                                     | Result                          | Kept?     |
|--------------------------------------------|---------------------------------|-----------|
| AVX-512 chosen at compile time¹            | +2 to +5% (Red Hat 288)         | yes       |
| No end-of-input test in literal runs²      | fonts ±1%, large −2 to −4%      | no        |
| `(min window fp)` once per command         | −1 to −3%³                      | no        |
| One (distance, length) test per copy⁴      | fonts +0–3%, source +1–8%       | yes       |
| Loop variant for a full window⁵            | +1 to +3% everywhere            | yes       |
| Stage macros for the command loop          | same code (readability)         | yes       |
| Variants per `npostfix` (0–3)              | source +1–3%, Red Hat −3 to −5% | no        |
| Variants for `ndirect = 0`, one block type | never applies⁶                  | not built |

1. Instead of a run-time `simd` flag: one fewer live variable, so less register
   pressure.
2. When `pos + 2k ≤ limit`.  If a run of k literals has pos + 2k ≤ limit (each
   literal takes at most 15 bits, so under 2 bytes), the run can’t reach the end
   of the input, so the per-literal input check could be skipped. Fonts didn’t
   change (±1%) and large streams lost 2–4%, so it wasn’t kept.
   
   The input check is once per refill, and a refill loads 8 bytes at once.
   From `br-refill` (`src/reader.lisp`):

   ```lisp
   (if (> (+ pos 8) limit)
       ;; near the end: read from the zero-padded tail copy (or error)
       (load64 %tail …)
       (load64 in pos))     ; normal case: one 8-byte load
   ```

   So each refill does one compare, pos + 8 > limit. It’s false almost every
   time, so the CPU predicts it almost perfectly and it costs very little. Only
   in the last 8 bytes does it switch to the reader’s tail, a zero-padded copy
   of the input’s last bytes.

   How often a refill happens depends on the decoder:

   Brotli, Lisp loop: one refill before every literal ("unconditional refill
   before every literal", §5), so in effect one well-predicted compare per
   decoded literal, not per input byte.  Brotli VOP (brotli-vop.lisp): the same
   checked refill in assembler, with a compare per refill, and near the end it
   reads the tail. Deflate VOP: no check in its refills at all. It runs only
   while at least 16 input bytes remain, checks that once per iteration, and
   the Lisp loop finishes the block.

   This change tried to skip even the per-literal compare for a run of literals that
   provably stays clear of the end. That didn't make the code faster, which
   suggests the compare is close to free already.

3. Added one more live value -> more register pressure.
4. One classification of the copy instead of five separate tests.

   This one was about eliminating branches and doing less computations.

   Background: a back-reference copies len bytes from dist bytes back in the
   output. How to copy quickly depends on both numbers:

   The source overlaps the destination (dist < len): the output repeats every
   dist bytes, and a plain wide copy would read bytes it hasn't written yet.
   The distance is very short (dist < 8): even one 8-byte word overlaps, so it
   needs the period trick.  The match is long and far back: it can use 64-byte
   AVX-512 copies. The match is short: two 8-byte words are enough.

   Now: copy-back-reference classifies (dist, len) once
   into straight-line paths, most common first:

   dist ≥ 16 and len ≤ 16: short, no overlap. Two 8-byte loads, then two stores,
   with no loop. This is the common case on fonts, and one combined test reaches
   it.  dist ≥ len: longer, no overlap. A loop of 16 bytes per step, or 64-byte
   AVX-512 steps if dist ≥ 64.  Otherwise it overlaps. If dist ≥ 8, 16-byte
   steps still work. Below 8, it copies the first period byte by byte, then
   8-byte words at that period (+copy-period+).

   Each path also sets p1/p2, the last two bytes (the literal context), the
   cheapest way for that case. The earlier version tested the conditions one
   after another for every copy, so the common case went through several
   branches.

5. A window covering all output
   
   The "distance" is a number each Brotli command carries: it tells how far back
   in the output to copy from. Each insert-and-copy command means "write these N
   literal bytes, then copy M bytes starting distance bytes back". With distance
   5 and length 4, you copy 4 bytes from 5 bytes behind the current position.

   Brotli also has a static dictionary: a fixed list of 122,784 bytes of common
   words and phrases ("the", "function", "</div>", and many in other languages),
   the same in every decoder. It's our data/dictionary.bin, from RFC 7932
   Appendix A. A stream can use those words without ever having written them.

   But a command has no separate field for "use a dictionary word". Brotli
   reuses the distance: any distance larger than the allowed maximum,
   min(window, fp) (the window, or everything written so far if that’s shorter),
   can’t be a real copy, so it means a dictionary reference instead:

   The copy length (4–24) picks the word length. How far past the maximum the
   distance goes gives two numbers: which word of that length, and which of 121
   transforms to apply. A transform can be "as is", "capitalise the first
   letter", "add a space after", "add ing", "put . before" and so on.

   An example: the output so far is 1,000 bytes and the window is
   larger. Distance 600 means "copy from 600 bytes back". Distance 1,050 can't
   be a copy, since there are not 1,050 bytes yet. So with length 5, it means
   word number k of the 5-letter words, with transform t, both worked out from
   the 50 it overshoots by. The decoder writes that word, transformed, into the
   output (dictionary-word in src/decoder.lisp).

   That's why each command has to compare the distance with min(window, fp): the
   result decides between a copy and a dictionary word. In the big-window loop
   variant, min(window, fp) is just fp, which saves one step per command.

   The macro: define-loop-variants (`src/decoder.lisp`) generates command-loop
   for those two cases:

   - `command-loop`, with `+big-window+` = NIL, uses `(min window fp)` for
     every command;
   - `command-loop/big-window`, with `+big-window+` = T, uses just `fp`.

   `compressed-meta-block` picks one, once per meta-block. The test of a value
   that is fixed for the whole stream moves out of the loop into the choice of
   loop, which also removes a live value (`window`) and a step per command.
   
6. The fonts use npostfix 1–2, ndirect 12–16 and many block types.

## 11. Shortening the per-literal chain

Each literal costs a dependency chain: bit buffer → table index → table load →
shift → symbol → context lookup → next table. Ideas aimed at that chain:

| Change                                 | Result                      | Kept? |
|----------------------------------------|-----------------------------|-------|
| `p1`/`p2` packed in one register¹      | −9% on fonts                | no    |
| The same, forced untagged²             | still −9%                   | no    |
| Refill byte count as a shift³          | +0.5 to +1.5%               | yes   |
| Context parts in the table entries⁴    | −4 to −6%                   | no    |
| Literal run as a hand-written VOP⁵     | fonts −1 to −3%, large −10% | no    |
| Subtable passes over needed slots only | Red Hat +1.3 to +1.7%       | yes   |

1. With a combined context table: one load per literal.
2. Bit 63 kept set. The chain got longer: 6 ALU steps instead of an `OR`.
3. `(* 8 k)` compiled to `IMUL`. Neutral at first; +0.5 to +1.5% together with
   the next item.
4. Context parts precomputed into the decode-table entries, two loop variants:
   the carried value was spilled and its extraction tagged.
5. An x86-64 VOP with all values in chosen registers; it saved and restored
   every register around each short run.

## 12. Smaller cuts

| Change                                 | Result                             | Kept?       |
|----------------------------------------|------------------------------------|-------------|
| Read the input in place¹               | small font ±0, large fonts +0.5–2% | yes         |
| Globals instead of specials²           | large fonts +0.5–2%                | yes         |
| Bind the remaining specials lexically³ | noise; −1% in the decode loop      | setup parts |
| Inline all small functions             | noise (setup only)                 | yes         |

1. Copy only its last 8 bytes, zero-padded.
2. `define-load-time-global` for all tables and the dictionary.
3. Specials read in loops. Binding one in the decode loop cost 1% (one more
   live value).

## 13. Measurement corrections

I have tried python-brotli, but it is not a fair C baseline. It does not know
the output size and decodes through the streaming API into a growing
buffer. Switching to libbrotli's `BrotliDecoderDecompress` through `sb-alien`
(decoding into a buffer of the known size, like `:size-hint`) made C
considerably faster on large streams: the large-file ratios went from 1.15–1.22×
to 0.95–1.13×.

The benchmark timer `get-internal-real-time` ticks in milliseconds on this
machine. With ~100 ms rounds that quantised results to ~1% steps (identical
numbers in different runs gave it away). Instead I have used
`sb-ext:get-time-of-day` which ticks in microseconds.

## 14. The from-scratch rewrite

The decoder was rewritten as a decoder-only library (five files, ~1,060 lines),
SBCL-only, keeping the design in tune with the benchmarks. The biggest deviation
is the setup: simple prefix codes built from their 1–4 symbols, code-length
counts gathered while reading, code-length codes straight into 32-entry tables.

Testing against libbrotli means round trips through libbrotlienc at all
qualities, windows and modes. Corrupted and truncated streams also have to be
tested and rejected exactly as libbrotlidec accepts or rejects them. I found
eight format checks the earlier decoders did not make. They accepted corrupt
streams that libbrotli rejects:

* nonzero padding before uncompressed or metadata blocks;
* nonzero padding after the last meta-block;
* incomplete or oversubscribed prefix codes;
* incomplete code-length codes;
* code-length repeats running past the alphabet;
* context-map zero runs past the end of the map;
* the reserved window size;
* duplicate symbols in simple prefix codes.

Speed was unchanged against the previous version (within noise), as estimated.

## Final result (Brotli, before the deflate work)

| Stream                | decompress | libbrotli | ratio         |
|-----------------------|------------|-----------|---------------|
| Red Hat Text          | 315        | 343       | 0.92×         |
| Meslo Nerd Font Bold  | 298        | 281       | 1.06×         |
| Noto Serif Tangut     | 395        | 351       | 1.13×         |
| Noto Sans SignWriting | 500        | 497       | 1.01×         |
| src8m q9 / q11        | 882 / 840  | 888 / 882 | 0.99× / 0.95× |
| src1m q9 / q5         | 803 / 736  | 872 / 804 | 0.92× / 0.92× |

From the original decoder: about **30×** on the source files.

**The same streams now** (2026-11-02, after the command-loop VOP of section 20;
SBCL 2.6.9 with sb-simd-512, best of two alternating rounds of `bench.lisp` and
`bench-c.lisp`):

| Stream                | decompress  | libbrotli | ratio         |
|-----------------------|-------------|-----------|---------------|
| Red Hat Text          | 376¹        | 345       | 1.09×         |
| Meslo Nerd Font Bold  | 319         | 283       | 1.13×         |
| Noto Serif Tangut     | 416         | 353       | 1.18×         |
| Noto Sans SignWriting | 552         | 500       | 1.10×         |
| src8m q9 / q11        | 1120 / 1005 | 910 / 900 | 1.23× / 1.12× |
| src1m q9 / q5         | 1007 / 921  | 872 / 815 | 1.15× / 1.13× |

1. The other round gave 347 (1.01×); the small font varies most between runs.

From the original decoder: **31–37×** on the source files.

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
Brotli decoder fast, so nothing was (re)used from it. Deflate uses the same
LSB-first bit order and canonical prefix codes as Brotli, so the bit reader,
the decode tables and the back-reference copy were shared at first (later
rounds gave deflate its own table builder and copy; the bit reader is still
shared). Errors are a
separate condition, `deflate-error`; both conditions inherit from
`decompression-error`.

Comparison with libz (zlib-ng 2.3.3 in zlib-compatible mode, zlib API 1.3.1,
on this machine): round trips through
`deflateInit2_` at every level, window size, memory level and strategy, as zlib
and raw deflate. I have also tried to make error handling the same as zlib's, so
that corrupted zlib streams are handled the same way by `inflate` as by
`uncompress`. All in all there are 18,044 checks by default, and about 86,000
and 67,000 with other seeds.

Tests with font data: two real WOFF 1.0 fonts (every compressed table decoded,
each with its origLength as size hint), the Meslo TTF and the 8 MB SBCL source
as zlib streams.

| Change                           | Red Hat   | Source Code Pro | Meslo   | src8m z6 / z9     | Kept?     |
|----------------------------------|-----------|-----------------|---------|-------------------|-----------|
| First version                    | 341–343   | 222–224         | 265–266 | 482–484 / 492–494 |           |
| Bigger buffer, AVX-512 Adler-32¹ | 458–464   | 272–275         | 332–335 | 761–767 / 795–804 | yes       |
| Refill only when needed²         | −2 to −4% | +1.5%           | +3%     | +6 to +8%         | see below |
| Copy period from a table³        | neutral   | neutral         | neutral | neutral           | yes       |
| Raw-word `val`, 64-bit refill⁴   | +6 to +8% | +6%             | +9%     | +8 to +9%         | yes       |
| Modular `bits` subtraction       | neutral   | +1.5%           | neutral | neutral           | yes       |
| Refill per symbol, re-tested⁵    | +6 to +7% | neutral         | −4%     | −7 to −8%         | fonts     |
| Refill below 48 bits             | −9%       | −5%             | −4%     | −5%               | no        |

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

| Stream                    | decompress | libz (zlib-ng) | ratio         |
|---------------------------|------------|----------------|---------------|
| Red Hat Text, WOFF 1.0    | 515        | 880            | 0.59×         |
| Source Code Pro, WOFF 1.0 | 290        | 579            | 0.50×         |
| Meslo TTF, level 9        | 345        | 580            | 0.60×         |
| src8m, level 6 / 9        | 798 / 830  | 1286 / 1352    | 0.62× / 0.61× |

From the first version that is +50% on the Red Hat WOFF and +65–70% on large
streams. zlib-ng's inflate is a heavily SIMD-tuned C decoder, a harder
baseline than libbrotli. Two-literals-per-lookup tables were considered (deflate
has no context modelling, so they would apply), but building a 2,048-entry
table per dynamic block would cost more than it saves on small font tables.

## 16. Deflate, second round: zlib-ng's ideas, and opmask registers

zlib-ng's source (`inffast_tpl.h`, `chunkset_avx512.c`) was the reference.
Statistics from instrumenting the block loop on the test data show which of
the ideas mattered:

| Stream               | Literals | Second lookup | Distance < 8 | Length ≤ 16 |
|----------------------|----------|---------------|--------------|-------------|
| Red Hat Text WOFF    | 73%      | 17.5%         | 6%           | 92%         |
| Source Code Pro WOFF | 80%      | 22.3%         | 17%          | 95%         |
| Meslo .z             | 71%      | 17.0%         | 2%           | 95%         |
| src8m .z             | 31%      | 6.3%          | 0.4%         | 83%         |

Literals: the share of decoded symbols that are literal bytes; the rest are
matches (copies from earlier output). On fonts about three quarters of symbols
are literals, so the literal path matters most there. src8m is mostly matches.

Second lookup: the share of symbols whose Huffman code is longer than 8
bits. With an 8-bit root table, those need a second table lookup, which is
slower. These numbers are why the root table became larger, 10 bits as in
zlib-ng.

Distance < 8: the share of matches that copy from fewer than 8 bytes back. The
source and destination overlap, so the copy can't move 8 or more bytes in one
step; it needs a slow path or a pattern-repeat trick. These are rare (0.4–17%),
so the fast path can assume wide copies.

Length ≤ 16: the share of matches that are 16 bytes or shorter. 83–95% of
matches fit in a single 16-byte copy, so one unconditional store handles most of
them without a loop.

Results in order (same-session A/B runs):

| Change                         | Red Hat        | Source Code Pro | Meslo    | src8m z6 / z9 | Kept? |
|--------------------------------|----------------|-----------------|----------|---------------|-------|
| Start of this round            | 503            | 286             | 358      | 823 / 856     |       |
| Literal root of 9 bits¹        | 522            | 313             | 391      | ≈ +1%         | no    |
| Literal root of 10 bits¹       | 524            | 334             | 414      | ≈ +1%         | yes   |
| Literal root of 11 bits¹       | 497            | 309             | 419      | ≈ +1%         | no    |
| Opmask: input position in `k`² | spilled anyway |                 |          |               | no    |
| No calls in the loop³          | +1%            | +2%             | ±0       | ±0            | yes   |
| Opmask: `cap`, `limit` in `k`  | −5%            | −3%             | −2%      | −1.5%         | no    |
| Three literals per refill⁴     | +7%            | +4%             | +6%      | +4–5%         | yes   |
| Match data in table entries⁵   | +14%           | +6%             | +12%     | +18–21%       | yes   |
| VBMI pattern copy⁶             | −0.5%          | +1%             | ±0       | −1.5%⁷        | no    |
| Root lookup before the refill⁸ | +10%           | +2 to +6%       | +8%      | +7–9%         | yes   |
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
**1052 / 1108** (+28–29%). Against zlib-ng: 0.80× / 0.68× / 0.87× /
0.82×, up from 0.59× / 0.50× / 0.60× / 0.61–0.62×.

Can opmask registers serve as temporary storage, similar as some compilers (GCC)
store some temporaries in zmm regs? The idea was to keep values that SBCL would
spill in AVX-512 mask registers (`k1`–`k7`, sb-simd's `m8.64` type), moved with
`kmovq`. The idea was to "spill" into mask register instead of stack. Only
numbers can live there (the GC does not see `k` registers, so no Lisp
pointers), but it is not a problem here. Two findings:

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

So opmask registers did not help as spill space here, but removing calls from hot
loops did.

**Brotli.** The same changes were tried on the Brotli loop: removing its error
calls (22 call sites → 0, stack references 545 → 411) measured −1% to +1% —
`dictionary-word` is still a real call in the loop, and inlining it brought more
calls in (`replace`, copy routines, generic arithmetic) and made the loop
larger; a 9- or 10-bit root for the command tables was neutral. None of this was
kept.

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

| Change (cumulative)          | Red Hat   | Source Code Pro | Meslo   | src8m z6 / z9         | Kept? |
|------------------------------|-----------|-----------------|---------|-----------------------|-------|
| Start (previous commit)      | 624–640   | 361–375         | 485–497 | 1046–1081 / 1113–1147 |       |
| Deflate's own table builder¹ | +5–7%     | +7–9%           |         |                       | yes   |
| One shift for length codes²  | +16%      | +22–24%         | +7%     | +3–5%                 | yes   |
| One shift for distances³     | −0 to −1% | −3 to −4%       | −3%     | −1%                   | no    |
| Preload before the copy⁴     | −5%       | −1%             | −2%     | −1 to −2%             | no    |
| Lighter table setup⁵         | +2%       | +3%             | ±0      | ±0                    | yes   |
| Result                       | 754–758   | 470–475         | 524–528 | 1108–1114 / 1161–1174 |       |

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

Both rejected changes add a value live across the match path (`saved` through
the distance decode, the preloaded entry across the copy); under SBCL's
register pressure in this loop that costs more than the shorter dependency
chain gains, as with the opmask experiments.

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

The loop keeps about 14 values live across an iteration (`val`, `bits`, `pos`,
`fp`, `in`, `limit`, `r`, `bs`, `buf`, `cap`, `ltab`, `dtab`, `sym`, `saved`)
against libdeflate's ~9. In SBCL has roughly 12–13 general registers for
disposal, which means we can keep fewer variables live compared to
libdeflate. Packing loop invariants into shared registers was not tried: a
spilled invariant is one memory operand off the critical path, cheaper than
unpacking it on every use (the opmask result of section 16). Three ways to
remove values instead, same-session A/B against the previous commit, two rounds
each:

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

After the changes above, setup was still 37% of Source Code Pro's time and 23%
of Red Hat's (sb-sprof, self + callees of `read-dynamic-tables`),
`build-deflate-table` alone 22% / 15%. Same-session A/B against the previous
commit, two rounds each:

| Change                    | Red Hat | Source Code Pro | Meslo | src8m z6 / z9       | Kept? |
|---------------------------|---------|-----------------|-------|---------------------|-------|
| Reused, in-place tables¹  | +4–5%   | +11–13%         | ±0    | −0.5 to −2% / −0.5% | yes   |
| Builder inlined per kind² | −1–2%   | −2%             |       |                     | no    |
| Leaner `deflate-table`³   | ±0      | ±0              |       |                     | no    |

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

libdeflate used for the comparison is version 1.26 (built from
`~/repos/skia/libdeflate`, one decompressor object reused across calls), libz
(zlib-ng) and this decoder in one process, interleaved, same method as
`bench-c.lisp`; MB/s:

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
are bound by dependency chains. A real version would also need a type
deriver: the VOP's word result widened derived types in Brotli's header
code, which then used generic arithmetic.

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

**AVX2 Adler-32.** On a stock SBCL (no sb-simd-512) the AVX-512 code is compiled
out and the byte loop took 11% of a WOFF decode. It now uses stock sb-simd's
AVX2 (`u8.32-sad`, `s16.16-maddubs`, `s16.16-madd`; libdeflate's `adler32_avx2`
scheme, weights 32…1), under the feature `:decompress-avx2`. Stock SBCL 2.6.9,
same-session A/B, all of this round: Red Hat 502–545 → 650–659, Source Code Pro
328–341 → 423–431.

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

| Stream          | HEAD                  | One test              | Change      |
|-----------------|-----------------------|-----------------------|-------------|
| Red Hat         | 790–797               | 792–794               | ±0          |
| Source Code Pro | 586–593               | 503–512               | −13 to −15% |
| Meslo           | 551–554               | 543–544               | −1.5%       |
| src8m z6 / z9   | 1144–1148 / 1198–1224 | 1134–1136 / 1197–1210 | −1% / ±1%   |

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

| Stream          | HEAD                  | VOP                   | Change      |
|-----------------|-----------------------|-----------------------|-------------|
| Red Hat         | 787–790               | 842–857               | +7–8%       |
| Source Code Pro | 560–589               | 628–629               | +7–12%      |
| Meslo           | 544–549               | 607–612               | +11–12%     |
| src8m z6 / z9   | 1134–1150 / 1206–1221 | 1262–1278 / 1324–1351 | +11% / +10% |

Counters per byte (`perf-compare.lisp`), ours / libdeflate:

| Stream          | Cycles                   | Instructions | IPC                      |
|-----------------|--------------------------|--------------|--------------------------|
| Red Hat         | 6.42 / 4.86              | 18.7 / 13.7  | 2.91 / 2.81              |
| Source Code Pro | 7.92 / 6.94              | 26.0 / 16.0  | 3.29 / 2.30              |
| Meslo           | 8.21 / 7.00              | 15.7 / 12.5  | 1.91 / 1.78              |
| src8m z6 / z9   | 3.79 / 2.91, 3.28 / 2.54 | 8.1 / 6.6    | 2.13 / 2.26, 2.45 / 2.59 |

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

| Share of time        | Source Code Pro | Red Hat     |
|----------------------|-----------------|-------------|
| `inflate-block`¹     | 60%             | 72%         |
| Table setup²         | 34%             | 18%         |
| `lisp_alloc`         | 2.2%            | ≈ 0         |
| Output fill / shrink | 2.6% / 0.1%     | 0.4% / 0.3% |

1. The VOP and the Lisp loop that finishes each block.
2. `read-dynamic-tables` with everything it calls (`deflate-table`,
   `build-deflate-table`).

Allocation is noise, so caching the reader's 32-byte tail vector was not
done. The font gap left is table setup.

**Results:**

| Change                        | Red Hat   | Source Code Pro | Meslo | src8m z6 / z9   | Kept      |
|-------------------------------|-----------|-----------------|-------|-----------------|-----------|
| VOP in a function of its own³ | −2 to +1% | −3 to +1%       | ±0    | ±1%             | yes       |
| One shift for distances⁴      | ±0        | −1%             | ±1%   | ±0 / ±1%        | no        |
| Preload before the copy⁵      | ±1%       | ±1%             | ±1%   | −1 to −3% / −1% | no        |
| Fast/slow split⁶              |           |                 |       |                 | (already) |

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
   Lisp loop is the slow tail. So there is nothing to split.

The VOP gained 7–12% by halving the instructions (no tag shifts, less stack
traffic), and moved the loop from throughput-bound to bound by its dependency
chain. Neither re-tested idea shortened that chain, so nothing paid off: the
earlier losses were partly register pressure, but with less pressure the
run-time is neutral, and I see no gains. Four earlier rejections were not
re-tested, because they failed for reasons other than register pressure, and the
VOP does not change those:

- The 32-byte copy tier: few matches have distances between 32 and 64.
- The two-literals-per-lookup table: few pairs of literals fit in one root
  lookup.
- The 11-bit literal root: the larger table costs more to build than it
  saves.
- Testing the root entry once per literal: it made branch prediction worse.

So further gains had to come from doing less work per symbol and from faster
table setup, not from keeping values in registers.

One way of minimizing the work is to not throw it away. Branch misses are costly,
so it makes sense to look at where they are in order to minimize them.

    perf record -e branch-misses 
    
recorded the address of each misprediction while decoding src8m (zlib level 9).
I then matched those addresses to the instructions of inflate-fast, the function
that runs the VOP. perf usually reports an address one or two instructions after
the branch that missed, so each sample was counted for the branch just before
it. The table shows each kind of branch’s share of all mispredictions:

| Branch                                  | Share of misses |
|-----------------------------------------|-----------------|
| Literal or not (three slots, `other`)   | ≈ 23%           |
| Copy class (`len > 16`, `dist`, exits)  | ≈ 25%           |
| Length and distance subtable links      | ≈ 10%           |

The literal branch can't be removed: whether the next symbol is a literal
depends on the data, so no code change can make it predictable. The copy’s
length branch can be removed, as libdeflate does: when the distance is at least
8, always copy the same number of 8-byte words, whether the match needs them all
or not; the extra bytes are overwritten by later output. Only a match longer
than those words needs a loop for the rest. Short matches, the common case, can
take the same path every time, with no branch on their length.

The idea: if every match is copied as, say, three 8-byte words (24 bytes), then
matches of 24 bytes or less all run the same code and the CPU has nothing to
guess. Copying a few bytes too many is safe because the decoder writes over them
next. The dist >= 8 condition matters because each word must be read before the
next one is written, and that’s only correct when source and destination are at
least 8 bytes apart.

| Change                       | Red Hat | Source Code Pro | Meslo | src8m z6 / z9     | Kept |
|------------------------------|---------|-----------------|-------|-------------------|------|
| Five words (40 bytes)¹       | −2%     | −2 to −3.5%     | −1.5% | +4–6% / +5–8.5%   | no   |
| Three words (24 bytes)¹      | ±0.5%   | ±1%             | −0.5% | +1–2% / +2–2.5%   | yes  |
| Loop top aligned to 32 bytes | ±0.5%   | ±0.5%           | ±0.5% | −1 to +2.5% / ±1% | no   |

1. Each word is loaded after the previous one is stored, which is correct
   for `dist >= 8`. Font matches are mostly 16 bytes or less, so five
   words is too much work for them; three words still removes the length
   branch for matches up to 24 bytes.

The gap left to libdeflate on large streams is structural: the literal
branch, which follows the data, and about 1.4 more instructions per byte.
Neither is a register-allocation artifact, so another VOP change will not
remove them.

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

| Change                           | Red Hat   | Source Code Pro | Meslo | src8m z6 / z9 | Kept |
|----------------------------------|-----------|-----------------|-------|---------------|------|
| Sort by length masks¹            | +1%       | +3–4.5%         | ±1%   | ±1% / ±1%     | yes  |
| 16-bit `integer-length` operand² | −1 to +1% | +0.5–2%         | ±1%   | ±0 / ±0.5%    | no   |
| Count by mask popcounts³         | +1–2%     | +4.5–5%         | ±0    | ±1% / +0.5%   | yes  |
| No fill for zero runs⁴           | +0.5–2%   | +0.5–2.5%       | ±1%   | ±0.5% / +0.5% | yes  |

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

## 20. The Brotli command loop as a VOP

The deflate VOP (section 17) removed SBCL's register allocation from the symbol
loop; section 6 found the Brotli gap was the register
pressure. `src/brotli-vop.lisp` now holds the common part of `command-loop` in
assembler: the command symbol and both lengths, the literal run, the distance
and the copy.

| Part                      | Where           |
|---------------------------|-----------------|
| Bit buffer, positions¹    | registers       |
| Other state²              | `st` vector     |
| Block switches³           | Lisp            |
| Dictionary words³         | Lisp            |
| Refills near the end⁴     | VOP, from tail  |

1. `val bits pos fp`, the input and output bases, and P1/P2 (the last
   two bytes) during the literal run: twelve registers with four temps.
2. Block counts, the current decode tables (tagged pointers of objects the
   wrapper pins), the distance ring and the stream constants, in a
   stack-allocated word vector addressed through one register.
3. The VOP stops with a status: at a command's top (insert switch), inside
   a literal run (literal switch), before the distance code (distance
   switch), or with a distance past the window (a word). The rest of the
   command is left in `st`; `command-loop` finishes it and calls again.
4. Refills are checked against the input's end and load the reader's tail
   there, so the VOP never stops for input.

**Results** (MB/s, two alternating rounds each, same session):

| Stream          | Before    | After       | Change     |
|-----------------|-----------|-------------|------------|
| Red Hat         | 303–311   | 324–347     | +4 to +14% |
| Meslo           | 305       | 315–316     | +3%        |
| Noto Tangut     | 399–400   | 406–408     | +2%        |
| SignWriting     | 517       | 543–547     | +5%        |
| src8m q9 / q11  | 889–897 / 842–861 | 1069–1112 / 972–1005 | +20% / +15% |
| src1m q9 / q5   | 809–812 / 746–749 | 1012–1013 / 908–923 | +25% / +22% |

The large streams, where the loop is all the time, gained the most: the decoder
is now 1.1–1.25× libbrotli on them. The fonts gained less because their time is
mostly table setup (section 9). The Lisp loop is unchanged without
`:decompress-avx512`.

**Section 19's mask sort in `build-table`.** Brotli's table builder has
the same counting-sort chain through `start[l]`, so the 64-length mask
walk was tried there too (for codes of 64+ used symbols; the length
array grew by 63 bytes for the loads).

| Change     | Fonts       | src8m q9 / q11 | src1m q9 / q5 | Kept |
|------------|-------------|----------------|---------------|------|
| Mask sort¹ | −0.3 to −8% | −2 to −4%      | −2 to −3.5%   | no   |

1. Two alternating rounds. It lost even on src8m, where table setup is a
   negligible share, so the loss is not the sort itself; likely the larger
   stack arrays or code layout in `read-complex-code`. Brotli's sort runs
   over the used symbols only (as libbrotli does), so there was less to
   gain than in deflate's builder, which scans the whole alphabet. The
   popcount count was not tried, since it builds on the same masks.

Why it worked in §19 and not here is not clear to me. In `build-deflate-table`
the sort is a large share of a small function. Here it adds code to
`build-table` and padding to `read-complex-code`'s stack arrays, and the loss is
bigger than table setup's whole share of the time (Red Hat: about 18%). So the
cost was not the sort itself. The likely cause is the change's side
effects on the surrounding code, as with the rejections in §6, but I haven't
measured. The test would be the mask sort behind a run-time switch with
everything else (arrays, padding) the same.
