# This is a template for the Makefile.
# Parameters should go in the upper half as:
#     width = 200
# and be referenced in the command as $(width)

# ______________________________________________________________________________

DIRPNG = $(shell dir *.png)

DIRGIF = $(DIRPNG:.png=.gif)

all: clean Makefile $(DIRGIF)

%.gif: %.png
	convert $^ -channel Alpha -threshold 80% $@
	echo $@ >> provide

clean:
	rm -f *.gif provide

# Insert the install command here.
# e.g. sudo apt-get install ffmpeg
install-tools:
	echo "No tools required"

# Use this target when one file requires another.
# See "../../cue/split/Makefile" for an example.
require:
	@echo

.PHONY: all install-tools require clean
