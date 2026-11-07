#!/usr/bin/env python3
"""
bench_open.py — the time to open a font: FreeType's FT_New_Memory_Face and
runes/otf's open-font-loader, for the fonts it finds.  This is not the hinting
benchmark (bench3x3.py) and its numbers are not part of that ratio: a program
that opens a font once and hints many glyphs is the hinting benchmark's case,
one that opens many fonts and hints few is this one's.

  bench_open.py [--sbcl PATH]
"""
import glob, os, subprocess, sys

HERE = os.path.dirname(os.path.realpath(__file__))
FONTS_DIR = os.path.join(HERE, "..", "tests", "fonts")

def first(*patterns):
    for pattern in patterns:
        found = sorted(glob.glob(pattern))
        if found:
            return found[0]

def fonts():
    candidates = {
        "dejavu": first("/usr/share/fonts/TTF/DejaVuSans.ttf",
                        "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
                        "/usr/share/fonts/dejavu/DejaVuSans.ttf"),
        "liberation": os.path.join(FONTS_DIR, "LiberationSans-Regular.ttf"),
        "cjk-65535-glyphs": first("/usr/share/fonts/**/NotoSansCJK-Regular.ttc",
                                  "/usr/share/fonts/**/NotoSerifCJK-Regular.ttc"),
    }
    return {name: path for name, path in candidates.items() if path and os.path.exists(path)}

def rows(output):
    result = {}
    for line in output.splitlines():
        if line.startswith("#"):
            print("  " + line[2:])
        else:
            parts = line.split()
            if len(parts) >= 2:
                try:
                    result[parts[0]] = (float(parts[1]), int(parts[2]) if len(parts) > 2 else None)
                except ValueError:
                    pass
    return result

def main():
    sbcl = os.environ.get("SBCL_BIN", "/home/arthur/repos/sb-simd-512/run-sbcl.sh")
    if "--sbcl" in sys.argv:
        at = sys.argv.index("--sbcl") + 1
        if at >= len(sys.argv):
            sys.exit("usage: bench_open.py [--sbcl PATH]")
        sbcl = sys.argv[at]
    found = fonts()
    if not found:
        sys.exit("error: no font found")
    args = [f"{name}={path}" for name, path in found.items()]
    binary = os.path.join(HERE, "ft_open_bench")
    source = binary + ".c"
    if not os.path.exists(binary) or os.path.getmtime(source) > os.path.getmtime(binary):
        flags = subprocess.run(["pkg-config", "--cflags", "--libs", "freetype2"],
                               capture_output=True, text=True, check=True).stdout.split()
        subprocess.run(["cc", "-O3", "-march=native", source] + flags + ["-o", binary], check=True)
    ft = rows(subprocess.run([binary] + args, capture_output=True, text=True, check=True).stdout)
    # The arguments follow "--" for open_bench.lisp.
    lisp = subprocess.run([sbcl, "--noinform", "--disable-debugger", "--load",
                           os.path.join(HERE, "open_bench.lisp"), "--eval", "(sb-ext:exit)", "--"] + args,
                          capture_output=True, text=True, check=True)
    rn = rows(lisp.stdout)
    print()
    rule = "+------------------+----------------+----------------+---------------+"
    print(rule)
    print("| Font             | FreeType (us)  | runes/otf (us) | runes alloc   |")
    print(rule)
    for name in found:
        f = ft.get(name, (None,))[0]
        r, b = rn.get(name, (None, None))
        print(f"| {name:<16} | {f if f is None else format(f, '.3f'):>14} | "
              f"{r if r is None else format(r, '.3f'):>14} | {('%d B' % b) if b is not None else '-':>13} |")
    print(rule)

if __name__ == "__main__":
    main()
