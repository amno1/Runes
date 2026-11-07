#!/usr/bin/env python3
"""Dump FreeType's hinted outlines, for hint-test.lisp's reference data.

usage: freetype-hinted-dump.py FONT PPEM[,PPEM...] FIRST-GID LAST-GID STEP [advance] [TAG=VALUE,...]

The last argument, if it has an "=", sets the variation instance of a
variable font, as user coordinates by axis tag ("wght=700"); axes it does not
name are left at their defaults.

The reference files were made with FreeType 2.15 (libfreetype.so.6.20.7), as
packaged by Arch Linux, with FT_LOAD_NO_BITMAP | FT_LOAD_NO_AUTOHINT: the v40
TrueType interpreter, grayscale (no subpixel rendering), the default load
target.

Prints one line per glyph and size, "PPEM GID X Y X Y ...", the points of the
outline FreeType's default (v40 TrueType interpreter) loading produces, in
26.6 pixels; with "advance", "PPEM GID ADVANCE" instead, its hinted advance
width in 26.6 pixels.  Needs libfreetype.so.6; run it with python3 -I.
"""
import ctypes as C, sys
ft=C.CDLL("libfreetype.so.6")
class Vec(C.Structure): _fields_=[("x",C.c_long),("y",C.c_long)]
class Outline(C.Structure): _fields_=[("n_contours",C.c_short),("n_points",C.c_short),("points",C.POINTER(Vec)),("tags",C.c_char_p),("contours",C.POINTER(C.c_short)),("flags",C.c_int)]
class Bitmap(C.Structure): _fields_=[("rows",C.c_uint),("width",C.c_uint),("pitch",C.c_int),("buffer",C.c_void_p),("num_grays",C.c_ushort),("pixel_mode",C.c_ubyte),("palette_mode",C.c_ubyte),("palette",C.c_void_p)]
class Metrics(C.Structure): _fields_=[(n,C.c_long) for n in "w h hbx hby ha vbx vby va".split()]
class Slot(C.Structure): _fields_=[("library",C.c_void_p),("face",C.c_void_p),("next",C.c_void_p),("glyph_index",C.c_uint),("generic",C.c_void_p*2),("metrics",Metrics),("lha",C.c_long),("lva",C.c_long),("advance",Vec),("format",C.c_int),("bitmap",Bitmap),("bl",C.c_int),("bt",C.c_int),("outline",Outline)]
class Face(C.Structure): _fields_=[("num_faces",C.c_long),("face_index",C.c_long),("face_flags",C.c_long),("style_flags",C.c_long),("num_glyphs",C.c_long),("family",C.c_char_p),("style",C.c_char_p),("nfs",C.c_int),("sizes",C.c_void_p),("ncm",C.c_int),("cm",C.c_void_p),("generic",C.c_void_p*2),("bbox",C.c_long*4),("upem",C.c_ushort),("asc",C.c_short),("desc",C.c_short),("height",C.c_short),("maw",C.c_short),("mah",C.c_short),("up",C.c_short),("ut",C.c_short),("glyph",C.POINTER(Slot)),("size",C.c_void_p),("charmap",C.c_void_p)]
class VarAxis(C.Structure): _fields_=[("name",C.c_char_p),("minimum",C.c_long),("default",C.c_long),("maximum",C.c_long),("tag",C.c_ulong),("strid",C.c_uint)]
class MMVar(C.Structure): _fields_=[("num_axis",C.c_uint),("num_designs",C.c_uint),("num_namedstyles",C.c_uint),("axis",C.POINTER(VarAxis)),("namedstyle",C.c_void_p)]
args=[a for a in sys.argv if "=" not in a]
instance=dict(a.split("=") for a in sys.argv[1:] if "=" in a for a in a.split(","))
sys.argv=args
path=sys.argv[1]; ppems=[int(a) for a in sys.argv[2].split(",")]
lib=C.c_void_p(); ft.FT_Init_FreeType(C.byref(lib))
face=C.POINTER(Face)()
assert ft.FT_New_Face(lib,path.encode(),0,C.byref(face))==0
ng=face.contents.num_glyphs
if instance:
    mm=C.POINTER(MMVar)()
    assert ft.FT_Get_MM_Var(face,C.byref(mm))==0
    n=mm.contents.num_axis
    coords=(C.c_long*n)()
    for i in range(n):
        ax=mm.contents.axis[i]
        tag=bytes([(ax.tag>>s)&255 for s in (24,16,8,0)]).decode()
        coords[i]=int(float(instance[tag])*65536) if tag in instance else ax.default
    assert ft.FT_Set_Var_Design_Coordinates(face,n,coords)==0
first,last,step=int(sys.argv[3]),int(sys.argv[4]),int(sys.argv[5])
for ppem in ppems:
    ft.FT_Set_Pixel_Sizes(face,0,ppem)
    for gid in range(first,min(ng,last+1),step):
        # FT_LOAD_NO_BITMAP(8)|FT_LOAD_NO_AUTOHINT(32768)
        if ft.FT_Load_Glyph(face,gid,8|32768): print(ppem,gid,"ERR"); continue
        o=face.contents.glyph.contents.outline
        pts=[(o.points[i].x,o.points[i].y) for i in range(o.n_points)]
        if len(sys.argv)>6 and sys.argv[6]=='advance': print(ppem,gid,face.contents.glyph.contents.advance.x)
        else: print(ppem,gid,*[v for p in pts for v in p])
