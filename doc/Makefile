# Builds the Runes manual from runes.texi.
#   make          runes.info, for Emacs and info
#   make html     runes.html, one page
#   make pdf      runes.pdf (needs TeX)

all: runes.info

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
