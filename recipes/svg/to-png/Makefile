# ——— parameters ———————————————————————————————————————————————————————————————
width = 320
height = 320

# ——— implementation ———————————————————————————————————————————————————————————
DIRSVG = $(shell dir *.svg)

DIRPNG = $(DIRSVG:.svg=.png)

all: clean Makefile $(DIRPNG)

%.png: %.svg
	inkscape -z -e "$@" -w $(width) -h $(height) "$^"
	echo "$@" >> provide

clean:
	rm -f *.png provide

install-tools:
	sudo apt-get install inkscape

.PHONY: all install-tools clean
