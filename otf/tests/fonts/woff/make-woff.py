#!/usr/bin/env python3
"""Make the WOFF test fonts: WOFF 1.0 with this script's own encoder (stdlib
zlib only), WOFF2 with woff2_compress.  Run from tests/fonts/woff:
    python3 make-woff.py
"""
import os, struct, subprocess, zlib

WOFF1 = ["base.ttf", "glyf_components.ttf", "vazirmatn_var_trimmed.ttf",
         "LiberationSans-Regular.ttf", "Cantarell-VF.otf"]
WOFF2 = WOFF1 + ["TTC.ttc"]
# base.woff also carries a metadata and a private data block
METADATA = (b'<?xml version="1.0" encoding="UTF-8"?>\n'
            b'<metadata version="1.0"><uniqueid id="zpb-ttf.test.base"/></metadata>\n')
PRIVATE = b"zpb-ttf private data"

def pad4(b):
    return b + b"\0" * (-len(b) % 4)

def sfnt2woff(sfnt, metadata=None, private=None):
    flavor, n = struct.unpack(">IH", sfnt[:6])
    tables = []
    for i in range(n):
        tag, csum, off, length = struct.unpack(">4sIII", sfnt[12 + 16 * i:28 + 16 * i])
        tables.append((tag, csum, sfnt[off:off + length]))
    tables.sort()
    offset = 44 + 20 * n
    entries, blobs = [], []
    for tag, csum, data in tables:
        comp = zlib.compress(data, 9)
        if len(comp) >= len(data):
            comp = data
        entries.append(struct.pack(">4sIIII", tag, offset, len(comp), len(data), csum))
        blobs.append(pad4(comp))
        offset += len(blobs[-1])
    meta_off = meta_len = meta_orig = priv_off = priv_len = 0
    tail = b""
    if metadata:
        comp = zlib.compress(metadata, 9)
        meta_off, meta_len, meta_orig = offset, len(comp), len(metadata)
        tail += comp
        offset += len(comp)
    if private:
        tail = pad4(tail)
        offset = meta_off + len(tail) if metadata else offset
        priv_off, priv_len = offset, len(private)
        tail += private
        offset += len(private)
    body = b"".join(entries) + b"".join(blobs) + tail
    total_sfnt = 12 + 16 * n + sum(len(pad4(d)) for _, _, d in tables)
    header = struct.pack(">IIIHHIHHIIIII", 0x774F4646, flavor, 44 + len(body), n, 0,
                         total_sfnt, 1, 0, meta_off, meta_len, meta_orig, priv_off, priv_len)
    return header + body

here = os.path.dirname(os.path.abspath(__file__))
fonts = os.path.dirname(here)
for name in WOFF1:
    with open(os.path.join(fonts, name), "rb") as f:
        sfnt = f.read()
    extra = (METADATA, PRIVATE) if name == "base.ttf" else (None, None)
    out = os.path.splitext(name)[0] + ".woff"
    with open(os.path.join(here, out), "wb") as f:
        f.write(sfnt2woff(sfnt, *extra))
for name in WOFF2:
    src = os.path.join(here, name)
    with open(os.path.join(fonts, name), "rb") as f, open(src, "wb") as g:
        g.write(f.read())
    subprocess.run(["woff2_compress", src], check=True, stdout=subprocess.DEVNULL)
    os.remove(src)
