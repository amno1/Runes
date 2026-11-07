/* ft_hint_bench.c — FreeType's hinted glyph loading, timed as hint_bench.lisp
 * times runes/otf: one FT_Load_Glyph per glyph and size, the size set and the
 * glyphs warm, no rendering.  Prints "NAME OPERATIONS TIME ms" rows.
 *
 * Build: cc -O3 -march=native $(pkg-config --cflags freetype2) ft_hint_bench.c \
 *          $(pkg-config --libs freetype2) -o ft_hint_bench
 */
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <ft2build.h>
#include FT_FREETYPE_H

#define REPETITIONS 20

static double now_ms(void) {
  struct timespec t;
  clock_gettime(CLOCK_MONOTONIC, &t);
  return t.tv_sec * 1e3 + t.tv_nsec * 1e-6;
}

/* A load that fails would be timed as a very fast one: stop instead. */
static void load(FT_Face face, int glyph, int ppem, FT_Int32 flags) {
  FT_Error error = FT_Load_Glyph(face, glyph, flags);
  if (error) {
    fprintf(stderr, "FT_Load_Glyph failed: error 0x%02x, glyph %d at %d ppem\n",
            error, glyph, ppem);
    exit(1);
  }
}

static void bench(FT_Library lib, const char *name, const char *path, int ppem, int glyphs) {
  FT_Face face;
  if (FT_New_Face(lib, path, 0, &face)) {
    fprintf(stderr, "cannot open %s\n", path);
    exit(1);
  }
  int n = glyphs < face->num_glyphs ? glyphs : (int)face->num_glyphs;
  FT_Set_Pixel_Sizes(face, 0, ppem);
  /* The default hinted load: the v40 interpreter, no bitmaps, no auto-hinter,
   * the normal load target (grayscale, no subpixel rendering). */
  FT_Int32 flags = FT_LOAD_NO_BITMAP | FT_LOAD_NO_AUTOHINT | FT_LOAD_TARGET_NORMAL;
  for (int g = 0; g < n; g++) load(face, g, ppem, flags);
  double start = now_ms();
  for (int r = 0; r < REPETITIONS; r++)
    for (int g = 0; g < n; g++) load(face, g, ppem, flags);
  printf("%s %d %.3f ms\n", name, n * REPETITIONS, now_ms() - start);
  FT_Done_Face(face);
}

int main(int argc, char **argv) {
  const char *dejavu = argc > 1 ? argv[1] : "/usr/share/fonts/TTF/DejaVuSans.ttf";
  const char *liberation = argc > 2 ? argv[2] : "LiberationSans-Regular.ttf";
  FT_Library lib;
  if (FT_Init_FreeType(&lib)) {
    fprintf(stderr, "FT_Init_FreeType failed\n");
    return 1;
  }
  FT_Int major, minor, patch;
  FT_Library_Version(lib, &major, &minor, &patch);
  /* Lines starting with # are read by bench3x3.py for its disclosure. */
  printf("# FreeType %d.%d.%d, FT_LOAD_NO_BITMAP | FT_LOAD_NO_AUTOHINT | FT_LOAD_TARGET_NORMAL (v40 interpreter)\n",
         major, minor, patch);
  bench(lib, "dejavu-9", dejavu, 9, 4000);
  bench(lib, "dejavu-16", dejavu, 16, 4000);
  bench(lib, "dejavu-32", dejavu, 32, 4000);
  bench(lib, "liberation-12", liberation, 12, 2500);
  return 0;
}
