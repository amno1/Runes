"""What the numbers of bench3x3.py compare, and what they do not."""

COMMON = [
    "Operation: one glyph hinted at one size, the size's engine made, the font in memory, no rendering.",
    "  FreeType: FT_Load_Glyph with FT_LOAD_NO_BITMAP | FT_LOAD_NO_AUTOHINT | FT_LOAD_TARGET_NORMAL (grayscale,",
    "  no subpixel rendering), so its v40 TrueType interpreter.  The outline stays in the glyph slot, which",
    "  FreeType reuses: it allocates nothing for it.  A failed load stops the run.",
    "  runes/otf: hint-outline-26dot6-into: hints into the engine's own buffers and copies the outline (x, y,",
    "  on-curve, contour ends, phantom points) into one caller-owned buffer that is reused, as FreeType's glyph slot is:",
    "  nothing is allocated per glyph.  The copy-out is in the timed work on both sides.",
    "Rows without a FreeType counterpart (not in the ratio, and not comparable with the rows above):",
    "  dejavu-16-fresh: hint-outline-26dot6, the same with four new arrays (and the phantom points) returned",
    "    for each glyph, about 350 bytes: the difference to the dejavu-16 row is the price of the fresh arrays.",
    "  dejavu-16-contours: hinted-contours, what a renderer calls: hinting plus one control-point structure per",
    "    point in pixels (single-floats), vectors per contour; about 150 MB allocated per row.",
    "  dejavu-16-advances: hinted-advance-width, the hinted advance of the glyph in pixels: hinting without",
    "    building an outline.  Reading either against the hinting rows alone gives a wrong ratio.",
    "Garbage collection: a collection runs before each row and the nursery is large enough that none runs during it,",
    "  so runes/otf's allocation is paid for but no pause is timed.  This is checked: a row in which the bytes",
    "  allocated exceed the nursery, or the collector's run time changed, stops the run.",
    "Equivalence: the outlines match FreeType's point for point (otf/tests/hint-test.lisp), so the work is the same.",
    "  That holds for the FreeType version and build the reference files were made with (2.15, as packaged by",
    "  Arch Linux); the version used for the timing is printed below.",
    "Glyphs: the first 4000 (DejaVu Sans) or 2500 (Liberation Sans) glyphs, simple and composite, in glyph order.",
    "Arithmetic: the 32-bit multiplies of the interpreter (mul-div, mul-fix) compile to a plain IMUL and DIV with no",
    "  overflow or bignum check and no allocation (checked by disassembly: the declared types bound the products), and the",
    "  hinting path allocates 6.6 bytes per glyph (the engine pool), so no garbage collection falls inside a row.",
    "Scope: these numbers are hinting only: a font opened once, then glyphs hinted.  Opening a font is another cost",
    "  with another benchmark (bench_open.py), and a program that opens many fonts and hints few is not served by this",
    "  ratio.  The open rows compare unlike work, too: runes/otf opens a font lazily (the name, kern and glyph tables",
    "  are read when asked for), FreeType parses more of the face when it opens it.",
    "Cost of the fresh arrays: hint-outline-26dot6 takes about 6.5% longer than the -into call (dejavu-16-fresh 76.84 ms",
    "  against dejavu-16 72.05 ms): the four new arrays per glyph are what a caller pays for not reusing a buffer.",
    "Tried and reverted: assembling composites in the glyph zone through base offsets (BASE and CONTOUR-BASE added",
    "  to every point access).  The offsets in the shared point accessors taxed simple glyphs by 4-6% (the allocating",
    "  call by 8%) and the saving on composites was smaller than that, so it was taken out; the rows here are without it.",
    "Final result (this machine, 3x3): DejaVu Sans 9/16/32 ppem 1.27x / 1.28x / 1.29x, Liberation Sans 12 ppem 1.36x",
    "  of FreeType's time.  The 3x3 cannot see differences below about 3%: smaller changes were judged by alternating",
    "  runs of two builds in one session.",
    "Environment: one machine, FreeType from the distribution, runes/otf under the SBCL named above.",
    "  ft_hint_bench is built with -march=native and is not portable; it is built on first use and not committed.",
    "  Ratios are not portable; the CPU governor is whatever the machine had.",
]

def banner(extra=()):
    lines = ["", "-" * 80, "BENCHMARK LIMITS (read before citing any ratio):"]
    lines += ["  * " + l if not l.startswith("  ") else "    " + l.strip() for l in COMMON]
    lines += ["  * " + e for e in extra]
    lines += ["-" * 80, ""]
    return "\n".join(lines)

def print_banner(extra=()):
    print(banner(extra))
