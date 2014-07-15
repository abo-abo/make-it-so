# ______________________________________________________________________________

DIRPDF = $(shell dir *.pdf 2>/dev/null)

DIRTXT = $(DIRPDF:.pdf=.txt)

all: clean Makefile $(DIRTXT)

%.txt: %.pdf
	pdftotext $^
	echo $@ >> provide

clean:
	rm -f *.txt provide

# Insert the install command here.
# e.g. sudo apt-get install ffmpeg
install-tools:
	sudo apt-get install poppler-utils

# Use this target when one file requires another.
# See "../../cue/split/Makefile" for an example.
require:
	@echo

.PHONY: all install-tools require clean
