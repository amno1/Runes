# Experiment: retargeting result moves after register allocation

SBCL master (d24b6e227) plus one pass in `src/compiler/ir2opt.lisp`
([0001-retarget-result-moves.patch](0001-retarget-result-moves.patch), about 55
lines). Measured on 2026-10-26 on an AMD Ryzen AI 9 365 (Zen 5) laptop, Linux.

## The problem

On x86-64, SBCL often compiles `(setf x (op x y))` into three instructions:

```
MOV RAX, RBX      ; copy x to a temporary
OR  RAX, 56       ; the operation
MOV RBX, RAX      ; copy back
```

instead of `OR RBX, 56`. The IR2 looks like this:

```
LOGIOR-C/UNSIGNED=>UNSIGNED BITS[RBX] {56} => t23[RAX]
WORD-MOVE t23[RAX] => BITS[RBX]
```

`t23` is written once and read once, by the move. Pack places it before it
knows (or without checking) that `BITS`'s register would be free for it. The
VOP's `x :target r` hint and the move's own targeting are both lost. In
decompress's `inflate-block`, 190 of 685 instructions (28%) were
register-to-register MOVs, many of this kind.

[repro.lisp](repro.lisp) shows it in ten lines. Its loop body goes from 17
instructions to 11 with the pass.

## The pass

`retarget-result-moves` runs right after pack, before `delete-no-op-vops`. It
looks for:

```
VOP ... => TMP[A]
MOVE TMP => DEST[B]       ; next VOP, a plain :move between compatible SCs
```

where `TMP` is written and read once. B holds nothing live between the two
VOPs: the move overwrites it, and nothing runs in between. So `TMP` can be
placed in B unless the VOP itself uses B, either:

- through an argument other than one that targets the result (`x :target r`:
  the VOP already expects that argument and the result to share a register),
  including any part of a multi-register argument, or
- for a temporary.

The pass then sets `TMP`'s offset to B. The move becomes B ⇒ B, which
`delete-no-op-vops` deletes. Each block is walked backwards, so chains such as
`op ⇒ t1; move t1 ⇒ t2; move t2 ⇒ x` collapse completely.

The pass only changes a temporary's register. Every global decision pack made
stays as it was, so no spills are added. `sb-c::*retarget-result-moves*`
(default `t`) turns it off for A/B comparisons inside one image.

### Bugs found on the way (why the guards exist)

1. **Pre-pack version** ([prototype/pre-pack-coalesce.lisp](prototype/pre-pack-coalesce.lisp)):
   it rewrote IR2 before pack, making the VOP write `DEST` directly.
   - It miscompiled ASDF when `TMP` was wired to a fixed register (a VOP body
     that assumes RAX, for example).
   - With that fixed, it removed the MOVs but changed pack's global choices.
     Stack references in `inflate-block` went from 78 to 102 (copy-loop
     counters were spilled) and it got about 1% slower. So it was dropped in
     favour of fixing things up after pack.
2. **Register pairs**: the first post-pack version compared only `tn-offset`.
   A 128-bit TN in RSI/RDI read by `MOVE-FROM-128` then let the result go into
   RDI and clobber the high half. SBCL's own test
   `compiler-3.pure / :REUSE-MOVE-COERCION-SCS` caught it. Locations are now
   compared over the whole range given by `sc-element-size`, and `TMP`/`DEST`
   must each be one register.

## Results

### Correctness

| Test | master | master + pass |
|---|---|---|
| SBCL self-build (the whole compiler and library compiled with the pass) | ok | ok |
| SBCL `tests/parallel-exec.lisp 12`: files passing | 398 | 398 |
| SBCL `tests/parallel-exec.lisp 12`: failing files | 7 | the same 7 |
| decompress `tests/run.lisp` (libz/libbrotli oracles), on sb-simd-512 + pass | 18044 passed | 18044 passed |

The 7 files fail on unmodified master too, so they come from this environment:
`style-warnings`, `setf`, `sb-cover`, `filesys`, `compiler.impure`,
`sb-posix`, `compiler-2`. Logs: `results/sbcl-tests-*.log`.

### Code

| | before | after |
|---|---|---|
| `inflate-block`: instructions | 685 | 626 |
| `inflate-block`: reg-reg MOVs | 190 | 131 |
| `inflate-block`: bytes | 2519 | 2348 |
| `inflate-block`: stack references | 78 | 78 |
| decompress: moves retargeted | — | 383 |
| SBCL core: machine code bytes | 10,808,535 | 10,806,912 (−0.015%) |

`count-movs.lisp` counts the MOVs; `codesize.lisp` measures the core.

The SBCL core barely changes because most of SBCL is tagged, call-heavy code.
The pattern is common only in raw-word arithmetic loops like decoders.

### Speed (decompress, sb-simd-512 A vs sb-simd-512 + pass B)

Each row is the mean of two alternating runs. Instructions and cycles are
`perf stat` `:u` counters, minus a load-only run of the same image. Run with
`./ab.sh A/run-sbcl.sh B/run-sbcl.sh`; raw output is in
`results/ab-sb-simd-512.txt`.

| file | MB/s A → B | decode instructions | decode cycles |
|---|---|---|---|
| SourceCodePro-Regular.woff | 368 → 368 (−0.1%) | 16.59G → 15.40G (−7.2%) | 3.95G → 3.93G (−0.7%) |
| meslo.z9.z | 485 → 470 (−3.2%) | 10.49G → 9.27G (−11.6%) | 2.98G → 3.12G (+4.5%) |
| src8m.z6.z | 963 → 977 (+1.4%) | 5.56G → 4.94G (−11.2%) | 1.50G → 1.56G (+3.8%) |
| src1m.q9.br | 732 → 742 (+1.3%) | 6.99G → 6.45G (−7.7%) | 1.97G → 1.97G (+0.1%) |
| RedHatText-Regular.br | 293 → 295 (+0.6%) | 18.66G → 17.29G (−7.3%) | 4.98G → 4.97G (−0.1%) |

Earlier runs of the prototype (`results/*-perf.txt`, `results/meslo-pmu.txt`)
show the same pattern: Brotli about +1–2%, deflate between −3% and +1%.
On `meslo.z9.z`, branch mispredicts, op-cache versus decoder dispatch and load
stalls did not change.

**Reading:** the pass reliably removes 7–12% of executed instructions, but
wall time is neutral, within about ±3% depending on the input. Zen 5 (like
Intel since Ivy Bridge) eliminates register-to-register MOVs at rename: they
take front-end slots but no execution ports and add no latency. These loops
are bound by dependency chains (bit-buffer → table load → shift), not by
front-end width. The deflate cases that got slower show no counter that
explains it. Code layout and alignment (the hot loop moved and shrank by 171
bytes) is the remaining suspect, and it is not proven here.

## Upstream case

- Fewer instructions and smaller code for raw-word arithmetic. Should help
  cores without move elimination and front-end-bound code more than these
  dependency-bound decoders.
- Purely local: it only moves single-use temporaries next to a move, after
  pack.
- Passes SBCL's test suite with no new failures, and its own bug was caught by
  an existing test.
- What's missing for a submission: a benchmark where the change shows up in
  wall time (cl-bench, or a front-end-bound loop), and runs on Intel and arm64.
  The pass is generic IR2, but only tested on x86-64.

## Files

| Path | What |
|---|---|
| `0001-retarget-result-moves.patch` | The pass, against SBCL master d24b6e227 |
| `repro.lisp` | Ten-line reproduction |
| `ab.sh`, `run.lisp` | A/B harness: two SBCLs, `perf stat`, five inputs |
| `count-movs.lisp` | Counts instructions, reg-reg MOVs, stack references and tag shifts in a disassembly |
| `codesize.lisp` | Total machine-code bytes in a core |
| `prototype/` | Runtime-loadable versions (pre-pack, which was dropped; post-pack with a rule for moves before a VOP, which fired twice and was dropped) |
| `results/` | Raw benchmark output, PMU counters, disassemblies, SBCL test logs |

Worktrees (uncommitted): `~/repos/sbcl-mov-coalesce` (master + pass, branch
`mov-coalesce`), `~/repos/sb-simd-512-mov-retarget` (sb-simd-512 + pass,
branch `sb-simd-512-mov-retarget`), `~/repos/sbcl-master-baseline` (master,
detached, for comparison).
