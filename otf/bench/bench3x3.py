#!/usr/bin/env python3
"""
bench3x3.py — 3x3 benchmark matrix (3 batches x 3 runs) of hinted glyph loading:
  - FreeType (C)
  - runes/otf (Common Lisp)

For each batch of 3 runs, takes the lowest time of each benchmark; the result
is the median of those 3 lowest values.  Adapted from antron/tools/bench.

  bench3x3.py [--sbcl PATH]    PATH is passed to run_comparison.sh as SBCL_BIN
"""

import os
import re
import statistics
import subprocess
import sys

sys.path.insert(0, os.path.dirname(os.path.realpath(__file__)))
import bench_disclosure

SCRIPT_DIR = os.path.dirname(os.path.realpath(__file__))
RUN_COMPARISON = os.path.join(SCRIPT_DIR, "run_comparison.sh")

# In the ratio: the benchmarks both sides have.
COMPARED = ["dejavu-9", "dejavu-16", "dejavu-32", "liberation-12"]
# Only runes/otf has these.
ONLY_RUNES = ["dejavu-16-contours", "dejavu-16-advances"]

ROW = re.compile(r"^([a-zA-Z0-9\-_]+)\s+(\d+)\s+([\d.]+)\s+ms")


def run_once(label, env):
    print(f"  [Running {label}]...", end="", flush=True)
    res = subprocess.run([RUN_COMPARISON], capture_output=True, text=True, env=env)
    if res.returncode != 0:
        print(f" ERROR (exit code {res.returncode})")
        print(res.stderr)
        sys.exit(1)
    print(" done.")
    return parse(res.stdout)


def parse(output):
    if "RUNNING RUNES/OTF" not in output:
        raise ValueError("unexpected benchmark output: no runes/otf section")
    ft_part, runes_part = output.split("RUNNING RUNES/OTF", 1)
    ft, runes, ops = {}, {}, {}
    for part, into in ((ft_part, ft), (runes_part, runes)):
        for line in part.splitlines():
            m = ROW.match(line.strip())
            if m:
                into[m.group(1)] = float(m.group(3))
                ops[m.group(1)] = int(m.group(2))
    return ft, runes, ops


def median_of_batch_minima(batches, index, name):
    minima = []
    for batch in batches:
        values = [run[index][name] for run in batch if name in run[index]]
        if values:
            minima.append(min(values))
    return statistics.median(minima) if minima else None


def main():
    env = dict(os.environ)
    if "--sbcl" in sys.argv:
        env["SBCL_BIN"] = sys.argv[sys.argv.index("--sbcl") + 1]
    sbcl = env.get("SBCL_BIN", "/home/arthur/repos/sb-simd-512/run-sbcl.sh")
    bench_disclosure.print_banner([f"SBCL: {sbcl}"])
    print("=" * 80)
    print("  RUNES/OTF vs FREETYPE: 3x3 HINTING BENCHMARK MATRIX (3 BATCHES x 3 RUNS)")
    print("=" * 80)

    batches, ops = [], {}
    for b in range(1, 4):
        print(f"\n---> Set {b}/3 (3 runs):")
        batch = []
        for r in range(1, 4):
            ft, runes, o = run_once(f"Set {b} Run {r}", env)
            batch.append((ft, runes))
            ops.update(o)
        batches.append(batch)

    print("\n" + "=" * 86)
    print("      RESULTS (MEDIAN OF THE 3 LOWEST RUNS OF EACH 3-RUN SET), ms and microseconds per glyph")
    print("=" * 86)
    rule = "+----------------------+------------+--------------+--------------+----------+"
    print(rule)
    print("| Benchmark            | Operations |  FreeType    |  runes/otf   | runes/FT |")
    print(rule)
    for name in COMPARED + ONLY_RUNES:
        n = ops.get(name, 0)
        ft = median_of_batch_minima(batches, 0, name)
        rn = median_of_batch_minima(batches, 1, name)
        per = lambda t: f"{t:7.2f} ms {1000 * t / n:5.2f}us" if t is not None and n else "N/A"
        ratio = f"{rn / ft:7.2f}x" if ft and rn else "      -"
        print(f"| {name:<20} | {n:>10,} | {per(ft):>12} | {per(rn):>12} | {ratio:>8} |")
    print(rule)
    print(bench_disclosure.banner([f"SBCL: {sbcl}"]))


if __name__ == "__main__":
    main()
