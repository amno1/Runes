# Builds the Runes manual from runes.texi.
#   make          runes.info, for Emacs and info
#   make html     runes.html, one page
#   make pdf      runes.pdf
# All need Texinfo (makeinfo, texi2pdf); the PDF also needs TeX, such as
# TeX Live (pdftex and the texinfo.tex macros).

all: runes.info runes.pdf

runes.info: runes.texi
	makeinfo runes.texi

html: runes.html
runes.html: runes.texi
	makeinfo --html --no-split runes.texi

pdf: runes.pdf
runes.pdf: runes.texi
	texi2pdf --clean runes.texi

clean:
	rm -f runes.info runes.html runes.pdf

.PHONY: all html pdf clean
