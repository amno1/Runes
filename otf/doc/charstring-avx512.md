## Side experiment: CFF charstring tokenizing with AVX-512

I have written a [wordcount program](https://github.com/amno1/wordcount "GNU wc
clone") which is heavily optimized and where I do a little trick I haven't seen
elsewhere. Actually nothing special, it started with counting spaces, but it was
possible to extend that to other data (utf8). The way GNU wc works is to count
word by looking at crossings: if prev byte is a space and next byte is not a
space, than we have a word. GNU wc keeps that computation in a variable. At the
start of the program it is initialized to true. However, that does not work very
nice with neither SWAR nor SIMD techniques, because checking boolean variables
typically creates branching. Yes, we can eliminated it by using 0 and 1 instead
of booleans and perform logical ops on them, but is still unnecessary to compute
that state. If we instead of keeping a boolean variable keep the byte itself, we
can just do the same arithemtic "over the boundary" so to say, as we do in the
rest of the byte. I come up with that one when I implemented the wordcounter
with SWAR technique, in order to eliminate branching, and it was even more
important for the SIMD version of kernels because checking a scalar variables
costs more when working in SIMD registers. GNU wc by the way keeps it's state in
`in_word` variable which is set to FALSE when program starts, and to TRUE when
it sees a whitespace char.

Wordcount is a local problem. Whether byte i starts a word depends only on byte
i and byte i − 1: it's a start if it isn't a space and the byte before it is a
space. So you compare the 64-byte vector with itself shifted by one byte, get a
64-bit mask, and count the bits. The only state carried to the next block is
whether the last byte was a space (GNU wc's in_word).

A similar idea can work for CFF: in a charstring, a token's length depends only
on its first byte. So the starts of all tokens in 64 bytes can be found at once:
look up each byte's length, then find which bytes actually start tokens. That
gave 4–5× over the scalar tokenizer.

To explaing further: unlike wordcount CFF is not local. Whether byte i starts a
token depends on where the previous token started, and that depends on the one
before it, back to the start of the charstring. Byte i could be an operator, or
the second byte of a 2-byte number, or the fourth byte of a 5-byte number. We
can't tell by looking at it or at its neighbour. Some example:

* 32–246: a 1-byte number
* 247–254: 2 bytes
* 28: 3 bytes
* 255: 5 bytes
* 12: a 2-byte operator
* other bytes below 32: 1-byte operators

next is mostly wrong, because most bytes don't start tokens. But you know byte 0
of the charstring does, so you follow the chain 0 → next[0] → next[next[0]] and
so on. Following it one step at a time would just be the scalar loop
again. Pointer doubling avoids that: composing next with itself gives two steps
at once, then four, eight, and so on. That's six vpermi2b steps to cover 64
bytes, and all the real token starts fall out at once.

The carry-over is the only part that actually is from wordcount. A token can
cross the 64-byte boundary, so you carry where the first token in the next block
starts, just as wordcount carries whether the previous byte was a space. It is
long explanation for a small idea, but hopefully it also explains the entire
avx-512 implementation.

A prototype tokenizer for CFF/CFF2 charstrings is in
`~/repos/skia/cff-simd/cff-tok.lisp`. Token length there depends only on the
first byte, so token starts can be found 64 bytes at a time: two `vpermi2b`
lookups give each byte's length, then pointer doubling with `vpermi2b` (J², J⁴ …
J⁶⁴) gives the compacted list of token offsets per block, with the next block's
start carried over, as my wordcount program carries its previous byte.

| Version (Noto Serif / Sans CJK)  | MB/s       | vs scalar |
|----------------------------------|------------|-----------|
| Scalar                           | 435–441    | 1×        |
| AVX-512, untyped¹                | ~760–840   | ~1.7–2.5× |
| AVX-512, typed                   | 1750–1876  | ~4.2×     |
| `POPCNT` VOP for `LOGCOUNT`²     | same³      | —         |
| Two glyphs in flight             | 2000–2160  | 4.6–4.9×  |
| Separate prefix pass, 1–4 glyphs | 1530–1860⁴ | —         |

1. Before type declarations: a `U8.64` coercion call around every `vpermi2b`.
2. Wordcount's `POPCNT` VOP instead of `LOGCOUNT`.
3. The loop is limited by the shuffle chain.
4. Slower: it no longer overlaps with the vector work.

One caveat about length depending only on the first byte: hintmask and cntrmask
(19 and 20) are followed by mask bytes, and how many depends on the number of
stem hints declared earlier in the charstring.
