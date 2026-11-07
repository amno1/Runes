/* ft_open_bench.c — FreeType's opening of a face from memory, timed as
 * open_bench.lisp times runes/otf's open-font-loader: FT_New_Memory_Face and
 * FT_Done_Face, the file already read.  Prints "NAME MICROSECONDS" rows, the
 * best of five batches of 2000 opens, per open.
 *
 * Build: cc -O3 -march=native $(pkg-config --cflags freetype2) ft_open_bench.c \
 *          $(pkg-config --libs freetype2) -o ft_open_bench
 * Usage: ft_open_bench NAME=PATH ...
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <ft2build.h>
#include FT_FREETYPE_H

#define BATCH 2000
#define BATCHES 5

static double now_us(void) {
  struct timespec t;
  clock_gettime(CLOCK_MONOTONIC, &t);
  return t.tv_sec * 1e6 + t.tv_nsec * 1e-3;
}

int main(int argc, char **argv) {
  FT_Library lib;
  if (FT_Init_FreeType(&lib)) { fprintf(stderr, "FT_Init_FreeType failed\n"); return 1; }
  printf("# FreeType open: FT_New_Memory_Face + FT_Done_Face, best of %d batches of %d\n", BATCHES, BATCH);
  for (int a = 1; a < argc; a++) {
    char *eq = strchr(argv[a], '=');
    if (!eq) { fprintf(stderr, "expected NAME=PATH: %s\n", argv[a]); return 1; }
    *eq = 0;
    FILE *f = fopen(eq + 1, "rb");
    if (!f) { fprintf(stderr, "cannot open %s\n", eq + 1); return 1; }
    fseek(f, 0, SEEK_END); long size = ftell(f); fseek(f, 0, SEEK_SET);
    unsigned char *data = malloc(size);
    if (fread(data, 1, size, f) != (size_t)size) { fprintf(stderr, "short read\n"); return 1; }
    fclose(f);
    double best = 1e18;
    for (int b = 0; b < BATCHES; b++) {
      double start = now_us();
      for (int i = 0; i < BATCH; i++) {
        FT_Face face;
        if (FT_New_Memory_Face(lib, data, size, 0, &face)) {
          fprintf(stderr, "FT_New_Memory_Face failed for %s\n", argv[a]); return 1;
        }
        FT_Done_Face(face);
      }
      double each = (now_us() - start) / BATCH;
      if (each < best) best = each;
    }
    printf("%s %.3f\n", argv[a], best);
    free(data);
  }
  return 0;
}
