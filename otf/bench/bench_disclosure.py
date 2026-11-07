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
