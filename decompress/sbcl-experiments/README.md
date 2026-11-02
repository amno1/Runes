# SBCL code generation notes for `inflate-block`

Experiments from 2026-10-26: SBCL 2.6.9 master plus sb-simd-512, Source Code
Pro WOFF. The measured results are summarised in `../doc/optimizations.md`
(section "SBCL codegen in `inflate-block`").

| File                            | What                                                               |
|---------------------------------|--------------------------------------------------------------------|
| `inflate-block-orig.asm.txt`    | Disassembly of the current `inflate-block` (1)                     |
| `inflate-block-pointer.asm.txt` | The same for the raw-address output variant (2)                    |
| `inflate-pointer-output.lisp`   | Full `src/inflate.lisp` of that variant (`copy-back-pointer`). (3) |
|                                 |                                                                    |

(1) 686 instructions: 170 register-to-register `MOV`s, 55 `SAR`/`SHL` by 1, 78 stack references
(2) 32 tag conversions, 95 stack references
(3) Tests pass; speed is neutral, so it was not kept

Count conversions and stack traffic in a new disassembly with:
`grep -cE '(SAR|SHL) [RE].., 1$'` and `grep -c 'RBP-'`.

## Registers SBCL reserves on x86-64

From `src/compiler/x86-64/vm.lisp` (the `define-gprs` `offsets-list`), the
allocator gets 12 of the 16 GPRs. Compiled C with `-fomit-frame-pointer` gets
15 (all except RSP).

| Register | Reserved as                            | Notes                                                                                                 |
|----------|----------------------------------------|-------------------------------------------------------------------------------------------------------|
| RSP      | stack pointer                          | also reserved in C                                                                                    |
| RBP      | frame pointer                          | spilled TNs are addressed `[RBP-n]`; backtraces and unwinding walk the RBP chain                      |
| R12      | `card-table-reg`, also `null-tn` (NIL) | the GC write barrier indexes the card table through it; NIL compares and `NIL` constants use it       |
| R13      | `thread-tn`                            | only on builds without `:gs-seg`; with `:gs-seg` the thread base is in the GS segment and R13 is free |

`call_into_lisp` (`src/runtime/x86-64-assem.S`) reloads R13 from its argument
and R12 from `gc_card_mark`, so Lisp code started from a signal handler
doesn't depend on the interrupted code's values of either register.

## Experiments

- [mov-retarget/](mov-retarget/README.md): an SBCL pass that removes
  register-to-register MOV round trips after pack. It cuts executed
  instructions by 7–12% on these decoders, but wall time is neutral (Zen 5
  eliminates these MOVs at rename). Includes the patch, a repro, the A/B
  harness and SBCL test results.
