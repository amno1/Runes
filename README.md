# Runes

Pure Common Lisp TrueType and OpenType font management: finding the
installed fonts, loading and caching them, metrics, and line layout
(formerly *clxcb-truetype* + *zpb-ttf*).

## Overview

`runes` is meant to be a complete stop for managing and using fonts in Common
Lisp. It aims to do Freetype's job of loading fonts and serving font metrics, 
fontserver job of discovering and caching fonts. The aim is similar to
Google's Fontations. The idea is that we should not have to call into foreign
function libraries (Freetype, fontconfig, etc).

Provided are:

- `runes`: the font manager: finding the installed fonts, loading and caching
  them, metrics, and line layout.
- `runes/raster`: a simple CPU renderer that rasterizes glyphs to 8-bit alpha
  maps with cl-vectors and cl-aa, cached per face.
- `runes/xcb`: draws text on X11 through RENDER glyph sets, as Xft does.
- `runes/wayland`: draws text into pixels the client owns, as on Wayland.
- `zpb-ttf`: the zpb-ttf font loader API, on top of `runes/otf`, so zpb-ttf
  programs work unchanged.  BSD 2-Clause.
- `runes/otf`: the OpenType loader: TrueType and CFF/CFF2 outlines, variable
  fonts, collections, WOFF and WOFF2.  Still a work in progress: the hinting
  virtual machine is missing.  MIT.
- `decompress`: Brotli, zlib and deflate decoders with no dependencies.  MIT.

The manual describes each system (see Documentation below).

The X11 bridge is a port of [clx-truetype][clx-truetype] from CLX to XCB. The
rasterization pipeline (ZPB-TTF → CL-VECTORS → CL-AA) is reused unchanged. The
X-specific code was rewritten against own drawable wrappers, and the CLX cache
layers were replaced with hash tables keyed on the connection.

[clx-truetype]: https://github.com/stumpwm/clx-truetype

## Requirements

- SBCL, preferably.  The library is developed and tested on SBCL.
  It uses portable libraries rather than SBCL-isms, so it may work on
  other implementations, but I haven't tested them myself. Decompression code used
  for web fonts (woff1 and woff2) is much slower in portable Lisp than in SBCL.
- ASDF 3.x for building the library
- UIOP for portable getenv and directory/file listing
- [zpb-ttf][zpb-ttf] — TrueType font parser (included, on top of `runes/otf`)
- [cacle][cacle] — LFU caches for string metrics and alpha maps
- [bordeaux-threads][bordeaux-threads] for the locks around the font
  loaders, the font cache and the bridges' caches
- `runes/raster` only: [cl-vectors][cl-vectors], [cl-paths-ttf][cl-paths-ttf],
  [cl-aa][cl-aa] for path representation and rasterization, and
  [trivial-garbage][trivial-garbage] for its per-font caches
- `runes/xcb` only: [clxcb][clxcb], the XCB binding to draw on X11
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

## Documentation

The manual is `doc/runes.texi`, in Texinfo.  It documents every system:
the font manager, the rasterizer, both bridges, the zpb-ttf loader,
the OpenType tables, the CFF interpreter and the decompressors. Build
it with `make -C doc` and read it in Emacs with `C-u C-h i
doc/runes.info`, or with `info -f doc/runes.info`.  `make -C doc html`
gives one HTML page, and `make -C doc pdf` a PDF.  Building the manual
needs Texinfo; the PDF also needs TeX, such as TeX Live.
`doc/design-notes.md` explains the design changes from clx-truetype.

## Differences from clx-truetype

Migrating from CLX: the bridge draws through XCB, `window` and
`pixmap` free their X resource only when made with `:owned t`, the
colour is passed as `:colour`, text goes through RENDER glyph sets, and
there is no background fill.  The manual's chapter "Migrating from
clx-truetype" and `doc/design-notes.md` have the details.

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
