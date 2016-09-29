# This is a template for the Makefile.
# Parameters should go in the upper half as:
#     width = 200
# and be referenced in the command as $(width)

# ______________________________________________________________________________

DIRDOT = $(shell dir *.dot)

DIRPNG = $(DIRDOT:.dot=.png)

all: clean Makefile $(DIRPNG)

%.png: %.dot
	dot -Tpng "$^" > "$@"
	echo $@ >> provide

clean:
	rm -f *.png provide

# Insert the install command here.
# e.g. sudo apt-get install ffmpeg
install-tools:
	sudo apt-get install graphviz

# Use this target when one file requires another.
# See "../../cue/split/Makefile" for an example.
require:
	@echo

.PHONY: all install-tools require clean
