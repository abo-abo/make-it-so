DIRCHM = $(shell dir *.chm)

DIRPDF = $(DIRCHM:.chm=.pdf)

all: clean Makefile $(DIRPDF)

%.pdf: %.chm
	chm2pdf --continuous $^
	echo $@ >> provide

clean:
	rm -f *.pdf provide

# Insert the install command here.
# e.g. sudo apt-get install ffmpeg
install-tools:
	sudo apt-get install chm2pdf

# Use this target when one file requires another.
# See "../../cue/split/Makefile" for an example.
require:
	@echo

.PHONY: all install-tools require clean
