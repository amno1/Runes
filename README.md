# Runes

Pure Common Lisp TrueType and OpenType font management: finding the
installed fonts, loading and caching them, metrics, and line layout
(formerly *clxcb-truetype*).

## Systems & Packages

- **`runes`** (package `RUNES`, nicknames `TRUETYPE`, `CL-TRUETYPE`, in
  `src/core/`): the font manager.  Font discovery, the `FONT` class, open font
  loaders, metrics, and glyph positions along a line (`layout-string`,
  `font-decorations`).  It draws nothing and rasterizes nothing, and depends on
  no graphics library.
- **`runes/raster`** (package `RUNES/RASTER`, in `src/raster/`): rasterizes
  strings and single glyphs to A8 alpha maps with CL-VECTORS and CL-AA
  (`text-pixarray`, `layout-glyphs`, `glyph-image`).  A simple CPU text
  renderer, which the two bridges draw with.
- **`runes/xcb`** (package `RUNES/XCB`, nicknames `XCB-TRUETYPE`, `XFT`, in
  `src/xcb/`): text bridge for X11, drawing through XCB and RENDER glyph sets.
- **`runes/wayland`** (package `RUNES/WAYLAND`, in `src/wayland/`): text bridge
  for client-side rendering, as on Wayland: cached glyph runs blended into the
  client's own pixels, through blend and fill functions the client passes, or
  into a plain `#x00RRGGBB` buffer.
- Backward compatibility: `cl-truetype.asd` (now `runes` plus `runes/raster`)
  and `clxcb-truetype.asd` (`runes/xcb`) are provided as aliases.
- **`runes/otf`** (package `RUNES/OTF`, nickname `OTF`, in `otf/`): the OpenType
  loader, formerly zpb-otf. Bounds-checked readers for ~50 OpenType tables,
  CFF/CFF2 charstrings (`runes/otf/charstring`), variable fonts, WOFF and
  WOFF2. MIT licensed; see `otf/README.md`.
- **`zpb-ttf`** (`zpb-ttf.asd`, sources in `otf/src/`): the zpb-ttf API on top
  of `runes/otf`, kept so existing zpb-ttf programs work unchanged. BSD
  2-Clause.
- **`decompress`** (in `decompress/`): Brotli, zlib and raw deflate decoders
  with no dependencies, usable on their own. MIT licensed; see
  `decompress/README.md`.

Antron renders text from `runes` alone: it takes fonts, metrics and
layout from it and rasterizes the glyph outlines itself, so it does not
use `runes/raster`.  The two bridges draw text for programs that do not
use Antron.

## Overview

`runes` finds and opens fonts and answers questions about them: which
families are installed, how big a string is, where each glyph of a line
goes.  A renderer asks it for a line's layout -- each glyph index at its
pen position -- and draws the glyphs itself, adding `RUNES:TARGET-DPI`
methods for its own drawables.

`runes/raster` is a simple CPU text renderer: it turns glyph outlines,
read by ZPB-TTF, into 8-bit coverage masks with CL-VECTORS and CL-AA, one
glyph at a time and cached per face, and the bridges put them on screen:

- `runes/xcb` uploads each glyph once per connection into a RENDER
  glyph set, and draws a string with one small request naming its
  glyphs -- the way Xft draws text.  It re-exports the `RUNES` and
  `RUNES/RASTER` API, so code written against `XCB-TRUETYPE` is
  unchanged.
- `runes/wayland` blends each glyph image into pixels the client owns,
  and paints underlines, strikethroughs and overlines, through the
  client's own blend and fill functions, so the client keeps its clipping
  and its fast paths.

The X11 bridge is a port of [clx-truetype][clx-truetype] from CLX to
XCB.  The rasterization pipeline (ZPB-TTF → CL-VECTORS → CL-AA) is
reused unchanged; the X-specific code was rewritten against its own
drawable wrappers, and the CLX cache layers were replaced with hash
tables keyed on the connection.

[clx-truetype]: https://github.com/stumpwm/clx-truetype

## Requirements

- SBCL, preferably.  The library is developed and tested on SBCL.
  It uses portable libraries rather than SBCL-isms, so it may work on
  other implementations, but I haven't tested them myself.
- ASDF 3.x for building the library
- UIOP for portable getenv and directory/file listing
- [zpb-ttf][zpb-ttf] — TrueType font parser (included, on top of `runes/otf`)
- [cacle][cacle] — LFU caches for string metrics and alpha maps
- [bordeaux-threads][bordeaux-threads] for the locks around the font
  loaders, the font cache and the bridges' caches
- `runes/raster` only: [cl-vectors][cl-vectors], [cl-paths-ttf][cl-paths-ttf],
  [cl-aa][cl-aa] for path representation and rasterization, and
  [trivial-garbage][trivial-garbage] for its per-font caches
- `runes/xcb` only: [clxcb][clxcb], the XCB binding it draws through,
  and trivial-garbage for portable finalizers

## Quick start

Load the system and build the font cache:

    (asdf:load-system :runes/xcb)
    (runes/xcb:cache-fonts)

`cache-fonts` scans `*font-dirs*` (by default `/usr/share/fonts/`,
`/usr/local/share/fonts/`, `~/.local/share/fonts/`, `~/.fonts/`, and on
macOS also `/Library/Fonts/` and `/System/Library/Fonts/`) and indexes
every `.ttf` it finds.  It only needs to be called once per image, or again after
installing a new font.

Wrap an XCB window or pixmap in a `window` or `pixmap` object, and
draw a string on it.  A window is drawn on once the server has sent an
Expose event for it:

    (xcb:with-x-connection (conn)
      (let* ((screen (first (xcb:conn-roots conn)))
             (win-id (xcb:generate-id conn))
             (font   (make-instance 'runes/xcb:font
                                 :family    "DejaVu Sans"
                                 :subfamily "Book"
                                 :size      24)))
        (xcb:create-window conn 0 win-id (xcb:root screen) 100 100 400 200 0
                           xcb-const:+window-class-input-output+ 0
                           (logior xcb-const:+cw-back-pixel+
                                   xcb-const:+cw-event-mask+)
                           :background-pixel (xcb:black-pixel screen)
                           :event-mask xcb-const:+event-mask-exposure+)
        (xcb:map-window conn win-id)
        (xcb:flush conn)
        (let ((win (make-instance 'runes/xcb:window
                               :connection conn :id win-id :screen screen)))
          ;; ... wait for the window's Expose event, then:
          (runes/xcb:draw-text win font "Hello, Runes" 10 60
                               :colour #xFFFFFF)
          (xcb:flush conn))))

`examples/hello.lisp` is the complete program, with the event loop.
Load and run it with:

    (load "examples/hello.lisp")
    (runes-example:hello)

You should see a simple message:

![hello](screenshots/hello.png)

## API

### Fonts

`make-instance 'font :family ... :subfamily ... :size ...` builds a
font object.  `subfamily` may be omitted or NIL, in which case the
first of Regular, Book, or the first available subfamily is chosen.

The accessors are `font-family`, `font-subfamily`, `font-size`,
`font-underline`, `font-strikethrough`, `font-overline`,
`font-antialias`.  All of them have `setf` methods that flush the
font's string caches, so a font can be reused and reconfigured.

`cache-fonts` repopulates `*font-cache*` from `*font-dirs*`.  Use
`get-font-families` and `get-font-subfamilies` to enumerate what's
installed.

### Layout

    (layout-string    font string &key dpi-x letter-spacing word-spacing kerning-table)
    (font-decorations font string x y dpi-x dpi-y line-p)

`layout-string` returns a vector of `(glyph-index pen-x)`, the pen
positions exact and in pixels from the start of the line, and the
line's total advance.  `font-decorations` returns the font's underline,
strikethrough and overline for a string drawn at baseline (x, y), as
rectangles `(left top width height)`.

### Drawing on the client (`runes/wayland`)

    (draw-text      font string x y &key dpi blend fill)
    (draw-text-line font string x y &key dpi blend fill)
    (draw-text-into-buffer pixels width height font string x y colour &key dpi line-p)

`draw-text` calls `(blend left top width height data stride)` for each
glyph's alpha map and `(fill left top width height)` for each
decoration; the caller blends its text colour into its own surface.
`draw-text-into-buffer` does both into a vector of `#x00RRGGBB` pixels.
Glyph runs are cached per font, string and DPI; `flush-glyph-runs`
forgets them, as `flush-font-caches` does for one font.

### Drawables

`window` and `pixmap` are the two drawable classes, one for each X
drawable type; `drawable` is their abstract base and cannot be made
itself.  A drawable carries its connection, its XID, its visual
(defaulting to the screen's root visual), and its screen.

Whoever creates an X resource frees it.  A drawable always frees the
RENDER picture it creates for drawing.  It frees the window or pixmap
itself only when it owns it, given `:owned t` when it is made:

    (make-instance 'xcb-truetype:window :connection c :id win)
        ;; frees only its picture: the window is the application's
    (make-instance 'xcb-truetype:pixmap :connection c :id pm :owned t)
        ;; also frees the pixmap

`destroy-drawable` frees the picture, and the window or pixmap too if
the drawable owns it.  `release-drawable` frees only the picture.  Every
drawable also has a finalizer installed at construction as a safety
net; the finalizer does not write to the X socket — it queues a free
request on a weak per-connection list, drained by
`flush-deferred-frees` (which `draw-text`, `draw-text-line`, and
`destroy-drawable` call automatically).

### Drawing

    (draw-text      drawable font string x y &key colour)
    (draw-text-line drawable font string x y &key colour)

Both draw STRING with its baseline at (x, y).  They differ in the box
that underline, strikethrough and overline span: the ink bounding box
for `draw-text`, the advance-width line box for `draw-text-line`, which
is what you want for laying out multiple lines.

Text is drawn through RENDER glyph sets: each glyph is rasterized once
per face, size and DPI, and uploaded once per connection, so drawing a
string again costs a few bytes per character.  Each glyph's origin is
put on a whole pixel, so a line is never more than half a pixel off its
measured width.  Binding `*draw-with-glyph-sets*` to NIL draws the old
way instead, rasterizing and uploading the whole string on every call.

`colour` is an `#x00RRGGBB` integer; the renderer expands it to the
16-bit channels RENDER expects.

### Metrics

The metric functions take a *target* — a drawable, a screen, an
integer DPI, or a `(dpi-x . dpi-y)` cons — and a font:

    (text-width             target font "string")
    (text-height            target font "string")
    (text-line-width        target font "string")
    (text-line-height       target font "string")
    (font-ascent            target font)
    (font-descent           target font)
    (font-line-gap          target font)
    (baseline-to-baseline   target font)

`fit-font` and `text-center` are convenience wrappers:

    ;; Return a font sized so string fits inside the box.
    (fit-font drawable "DejaVu Sans" "Book" string box-width box-height
              :min-size 8)

    ;; Return (values baseline-x baseline-y) that centre string in the box.
    (text-center drawable font string box-width box-height)

### Caches

`*font-cache*` maps family names to subfamily-to-pathname tables.
`*font-loader-cache*` holds open ZPB-TTF loaders keyed by pathname;
call `close-font-loaders` to close them.

`cache-fonts` scans into a new table and swaps it in when done, so
other threads go on finding fonts while it runs.

Each glyph, rasterized by `runes/raster` for one face at one size and
DPI, is kept on the client (at most 256 faces' worth) and, by
`runes/xcb`, uploaded once per connection
into a glyph set.  A connection keeps at most 64 glyph sets, one per
face, size, DPI and antialiasing, freeing the least recently used.

`*fill-cache*`, `*mask-gc-cache*` and the glyph sets are kept in weak
hash tables keyed on connection, holding solid-fill pictures, the
depth-8 GC used for mask uploads, and glyph sets.  None keeps a
connection alive; the server frees what they hold when the connection
closes.

## Differences from clx-truetype

If you are migrating from the CLX version:

- **XCB, not CLX**:  No `xlib` symbols, no second connection type.
  Everything draws through a connection produced by `xcb:`.

- **Drawable wrapper classes**:  `window` and `pixmap` wrap an XID
  and its connection.  `drawable` is abstract; instantiate one of the
  two concrete classes.  They free the window or pixmap only when made
  with `:owned t`; clx-truetype's always did.

- **Colour is passed explicitly**:  `draw-text` and `draw-text-line`
  take `:colour #x...` rather than reading the current foreground off
  a GC, because XCB has no way to read GC state back.  The 1×1 pen
  pixmap trick is gone; a solid-fill picture per colour is used
  instead.

- **Glyph sets**:  Text is drawn through RENDER glyph sets, as Xft
  does, instead of rasterizing and uploading each string's alpha mask
  on every call: about 30 times faster for a repeated string and over
  100 times for a new one, and only a few bytes per character over
  `ssh -X`.  Glyphs sit on whole pixels; underline, strikethrough and
  overline are crisp whole-pixel lines; and `draw-text-line` no longer
  clips the overline away.  See `CHANGES`.

- **No `draw-background-p`**:  The background fill that clx-truetype
  performed per call is not implemented; callers draw their own
  background rectangle first.

- **Caches keyed on connection**:  The plist-based drawable caches are
  replaced by weak hash tables keyed on the connection object, so a
  connection that becomes unreachable does not keep its cache or
  itself alive.

- **Deferred finalization**:  Finalizers queue frees on a weak
  per-connection list rather than writing to the socket.  The queue
  is drained on the application thread.

## Tests

    tests/run.sh                  # all tests
    tests/run.sh bench.lisp       # only those named

`run.sh` starts a scratch Xephyr server with two screens (depths 24
and 16) on display `:11`, or on `XEPHYR_DISPLAY`, runs each test in a
fresh SBCL, and stops the server.  Any Xephyr already on that display is
stopped first, so do not point it at one you use.  It exits non-zero if
a check fails.

- `drawables.lisp`: ownership, and which drawable classes can be made
- `font-cache.lisp`: font lookups during a rescan, units per em
- `screens-and-threads.lisp`: two screens, four threads on one connection
- `glyphs-vs-masks.lisp`: glyph sets against the old alpha masks;
  prints ink per case and writes `cmp-NN.pgm` images, old above new
- `bench.lisp`: time per string, both ways

## License

The GNU Lesser General Public License, version 2.1 or later
(`LGPL-2.1-or-later`).  See `LICENSE`, and `COPYING` for the license
text.  This covers everything except `otf/` and `decompress/`, which
are MIT licensed (with the zpb-ttf code in `otf/` under BSD 2-Clause);
see the `LICENSE` in each.

Runes started as clxcb-truetype, a derivative work of [clx-truetype][clx-truetype] by
Michael Filonenko and contributors, which was published under the MIT
license.  clx-truetype is said to have grown out of McCLIM's TrueType
rendering, which is under the LGPL; that could not be confirmed, so
Runes uses McCLIM's license, which is right either way.  The
MIT notice of clx-truetype's code is kept in `LICENSE`.  See `AUTHORS`
for everyone who worked on it, and for what is known of the history.

## History

According to online research, clx-truetype grew out of the TrueType rendering
written for McCLIM by Gilbert Baumann and Andy Hefner; we have not had this
confirmed by them (see `AUTHORS`).  It was written and maintained by Michael
Filonenko.  It is the standard text renderer for [StumpWM][stumpwm], via the
[ttf-fonts][ttf-fonts] contrib module.

clxcb-truetype, now Runes, ported the X-specific portions of that library to XCB
while keeping the client-side rasterization pipeline intact.

[clxcb]:            https://github.com/amno1/clxcb
[zpb-ttf]:          https://github.com/xach/zpb-ttf
[cl-vectors]:       https://github.com/fjolliton/cl-vectors
[cl-paths-ttf]:     https://github.com/fjolliton/cl-vectors
[cl-aa]:            https://github.com/fjolliton/cl-vectors
[cacle]:            https://github.com/jlahd/cacle
[trivial-garbage]:  https://github.com/trivial-garbage/trivial-garbage
[bordeaux-threads]: https://github.com/sionescu/bordeaux-threads
[stumpwm]:        https://github.com/stumpwm/stumpwm
[ttf-fonts]:      https://github.com/stumpwm/stumpwm-contrib/tree/master/util/ttf-fonts
