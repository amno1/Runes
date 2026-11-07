"""What the numbers of bench3x3.py compare, and what they do not."""

COMMON = [
    "Operation: one glyph hinted at one size, the size's engine made, the font in memory, no rendering.",
    "  FreeType: FT_Load_Glyph with FT_LOAD_NO_BITMAP | FT_LOAD_NO_AUTOHINT, so its v40 TrueType interpreter.",
    "  runes/otf: hint-outline-26dot6, which also copies the outline out into four fresh arrays; the 'contours'",
    "  and 'advances' rows have no FreeType counterpart and are not part of the ratio.",
    "Garbage collection: a collection runs before each row and the nursery is large enough that none runs during it,",
    "  so runes/otf's allocation (about 350 bytes per glyph in hint-outline-26dot6) is paid for but no pause is timed.",
    "Equivalence: the outlines match FreeType's point for point (otf/tests/hint-test.lisp), so the work is the same.",
    "Glyphs: the first 4000 (DejaVu Sans) or 2500 (Liberation Sans) glyphs, simple and composite, in glyph order.",
    "Environment: one machine, FreeType from the distribution, runes/otf under the SBCL named above.",
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
