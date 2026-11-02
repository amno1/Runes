# Test fonts

These fonts are used only as test data. They are redistributed unmodified
under their own licenses, which are independent of the runes/otf/zpb-ttf code
license. Full license texts are in `licenses/`.

| File | Copyright | License | Source |
|---|---|---|---|
| `LiberationSans-Regular.ttf` | Digitized data © 2010 Google Corporation, with Reserved Font Arimo, Tinos and Cousine; © 2012 Red Hat, Inc., with Reserved Font Name Liberation | SIL OFL 1.1 (`licenses/OFL.txt`) | https://github.com/liberationfonts/liberation-fonts |
| `Cantarell-VF.otf` | © 2019 The Cantarell Project Authors | SIL OFL 1.1 (`licenses/OFL.txt`) | https://gitlab.gnome.org/GNOME/cantarell-fonts |
| `base.ttf` ("LOCL Test") | © 2014-2016 Adobe Systems Incorporated | SIL OFL 1.1 (`licenses/OFL.txt`) | Adobe test font |
| `DejaVuMathTeXGyre.ttf` | © 2003 Bitstream, Inc.; DejaVu changes and TeX Gyre math extensions are public domain | Bitstream Vera license (`licenses/DejaVu.txt`) | https://dejavu-fonts.github.io/ |
| `TTC.ttc` | © 2015 FontTools, "No rights reserved" | MIT (`licenses/FontTools.txt`) | https://github.com/fonttools/fonttools |
| `chromacheck-cbdt.ttf`, `chromacheck-colr.ttf`, `chromacheck-sbix.ttf`, `chromacheck-svg.ttf` | © 2018 Roel Nieskens; © 2018 Google LLC | MIT (`licenses/ChromaCheck.txt`) | https://github.com/RoelN/ChromaCheck |
| `vazirmatn_var_trimmed.ttf` | The Vazirmatn project (Saber Rastikerdar); subset by the Fontations authors | SIL OFL 1.1 (`licenses/OFL.txt`) | https://github.com/rastikerdar/vazirmatn, via Fontations' `font-test-data` |
| `material_symbols_subset.ttf` | © 2020-2022 Google LLC; subset by the Fontations authors | Apache 2.0 (`licenses/Apache-2.0.txt`) | Google Material Symbols, via Fontations' `font-test-data` |
| `avar2checker.ttf` ("avar2 checker") | The fencer project authors (the font has no copyright notice) | Apache 2.0 (`licenses/Apache-2.0.txt`) | https://github.com/Lorp/fencer (`src/fonts`), via Fontations' `font-test-data` |
| `Amstelvar-avar2.A.ttf` | The Amstelvar avar2 project authors (no copyright notice in the font); subset by the Fontations authors | SIL OFL 1.1 (`licenses/OFL.txt`) | https://github.com/googlefonts/amstelvar-avar2, via Fontations' `font-test-data` |
| `hvar_with_truncated_adv_index_map.ttf`, `glyf_components.ttf` | © 2019 Fontations Developers (test fonts made for Fontations) | MIT (`licenses/Fontations-MIT.txt`) | https://github.com/googlefonts/fontations (`font-test-data`) |

`reference/` holds the outlines FreeType 2.13 produces for the last four fonts,
from Fontations' `font-test-data/test_data/extracted` (generated there by
`extract_glyphs.py`, under Fontations' MIT license); the tests compare against
them.

`woff/` holds WOFF 1.0 and WOFF2 copies of `base.ttf`, `glyf_components.ttf`,
`vazirmatn_var_trimmed.ttf`, `LiberationSans-Regular.ttf`, `Cantarell-VF.otf`
and (WOFF2 only) `TTC.ttc`, under the same licenses, made by
`woff/make-woff.py`: WOFF 1.0 by its own zlib encoder (`base.woff` also
carries a test metadata and private data block), WOFF2 by `woff2_compress`,
which drops the `DSIG` table. The font data is otherwise unchanged; the
tests compare the decoded tables with the originals.

The OFL and Bitstream Vera licenses forbid selling the fonts by themselves and
require modified versions to be renamed. Don't modify these files in place;
if a test needs a changed font, give it a new name.
