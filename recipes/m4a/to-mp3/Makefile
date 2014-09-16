# This is a template for the Makefile.
# Parameters should go in the upper half as:
#     width = 200
# and be referenced in the command as $(width)

# ______________________________________________________________________________

DIRM4A = $(shell dir *.m4a)

DIRMP3 = $(DIRM4A:.m4a=.mp3)

all: clean Makefile $(DIRMP3)

%.mp3: %.m4a
	ffmpeg -v 5 -y -i $^ -acodec libmp3lame -ac 2 -ab 192k $@
	echo $@ >> provide

clean:
	rm -f *.mp3 provide

install-tools:
	sudo apt-get install ffmpeg

# Use this target when one file requires another.
# See "../../cue/split/Makefile" for an example.
require:
	@echo

.PHONY: all install-tools require clean
