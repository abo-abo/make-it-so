# This is a template for the Makefile.
# Parameters should go in the upper half as:
#     width = 200
# and be referenced in the command as $(width)

# ______________________________________________________________________________

DIRMD = $(shell dir *.md)

DIRHTML = $(DIRMD:.md=.html)

all: clean Makefile $(DIRHTML)

%.html: %.md
	pandoc -o $@ $^
	echo $@ >> provide

clean:
	rm -f *.html provide

# Insert the install command here.
# e.g. sudo apt-get install ffmpeg
install-tools:
	sudo apt-get install pandoc

# Use this target when one file requires another.
# See "../../cue/split/Makefile" for an example.
require:
	@echo

.PHONY: all install-tools require clean
