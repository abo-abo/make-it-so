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

# Insert the install command here.
# e.g. sudo apt-get install ffmpeg
install-tools:
	sudo apt-get install inkscape

# Use this target when one file requires another.
# See "../../cue/split/Makefile" for an example.
require:
	@echo

.PHONY: all install-tools require clean
