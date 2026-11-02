Runes — changes from clx-truetype
=================================

Runes started as clxcb-truetype: clx-truetype ported from CLX to XCB (clxcb).
The font class, font cache, metrics and rasterization are clx-truetype's and
work as before. The changes are in how the library talks to the X server.

This part of Runes is distributed under the LGPL, version 2.1 or later,
where clx-truetype was published under the MIT license; LICENSE and
AUTHORS say why.

Drawables, and who frees what
-----------------------------

In the X protocol a DRAWABLE is a type of XID, naming a window or a pixmap;
the resources the server keeps are windows, pixmaps and pictures, and
whoever creates a resource is responsible for freeing it. Runes
follows that too:

  * A WINDOW or a PIXMAP is the library's handle on a window or pixmap
    that text is drawn on; the class says which, and so which request
    frees it. The RENDER picture made for drawing is the library's, so
    the library always frees it.

  * The window or pixmap itself is freed only when its creator hands it
    over, with :OWNED T when the drawable is made:

      (make-instance 'xcb-truetype:window :connection c :id win)
          ;; frees only its picture: the window is the application's
      (make-instance 'xcb-truetype:pixmap :connection c :id pm :owned t)
          ;; DESTROY-DRAWABLE, or the garbage collector, also frees the pixmap

  * RELEASE-DRAWABLE frees the picture and leaves the window or pixmap
    alone, owned or not.  DESTROY-DRAWABLE also frees it when owned.

clx-truetype's WINDOW and PIXMAP always freed the window or pixmap when
destroyed or dropped. A wrapper around an application's own window,
collected by the garbage collector, would destroy that window behind the
application's back. Now they own it only when told so. Code that relied
on the old behaviour adds :OWNED T. DRAWABLE is their abstract base.

Text through glyph sets
-----------------------

DRAW-TEXT and DRAW-TEXT-LINE draw through RENDER glyph sets, as Xft does.  Each
glyph of a face, at one size and DPI, is rasterized once and uploaded once per
connection. A string is then one small request naming its glyphs, a few bytes
per character, however often it is drawn. clx-truetype rasterized every string
as a whole and uploaded its alpha mask on every call. It is about 9 KB for one
24-pixel line of 300 pixels, which over ssh -X crossed the network each time.
Measured on Xephyr, a 14-pixel label:

    repeated string     280 us  ->  8.5 us
    new string each time 1150 us ->  8.8 us

What looks different:

  * Each glyph's origin is put on a whole pixel (the pen position, kept
    exact, rounded), so edges differ slightly from rasterizing the string
    as a whole; a line is never more than half a pixel off its measured
    width.  The total ink is the same.

  * Underline, strikethrough and overline are crisp rectangles of whole
    pixels, not antialiased at fractional positions.

  * DRAW-TEXT-LINE no longer clips to the line box.  clx-truetype's did,
    and so cut off the overline, which lies above the ascender.

Binding *DRAW-WITH-GLYPH-SETS* to NIL draws the old way.  TEXT-PIXARRAY and
TEXT-LINE-PIXARRAY still return a string's alpha mask, for code that wants
one.

Other changes
-------------

  * Built on clxcb: the X protocol through XCB, the Xrender path on clxcb's
    RENDER extension.

  * Server resources freed by finalizers are queued and sent from the
    application's thread (FLUSH-DEFERRED-FREES), never from the finalizer
    thread, which would corrupt the connection's request stream.

  * Font loaders are shared through a lock, so several threads can draw and
    measure text at once.

  * Fonts are also looked for in ~/.local/share/fonts,
    /usr/local/share/fonts and, on macOS, /System/Library/Fonts.  The
    font cache is guarded by a lock too.

  * The overline is drawn above the text, not over the tops of the glyphs,
    and the bounding box grows to hold it; clx-truetype had the underline
    position's sign the wrong way round.

  * Fully covered pixels of a glyph are fully opaque (255; clx-truetype's
    blend made them 254).

  * The GC that uploads glyph masks is kept per screen, so text works on
    every screen of a connection, not only the first used.

  * A drawable used again after DESTROY-DRAWABLE or RELEASE-DRAWABLE gets
    its finalizer back with its new picture, which no longer leaks.

  * WINDOW and PIXMAP are exported names: a package that :USEs both this
    one and CLX or CLIM should shadow them, or write XCB-TRUETYPE:DRAWABLE.

  * A font printed readably keeps its size.
