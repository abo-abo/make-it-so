# ______________________________________________________________________________

DIRMP4 = $(shell dir *.mp4)

DIRAAC = $(DIRMP4:.mp4=.aac)

all: Makefile $(DIRAAC)

%.aac: %.mp4
	avconv -i "$^" "$@"
	echo "$@" >> provide

clean:
	rm -f *.aac provide

# Insert the install command here.
# e.g. sudo apt-get install ffmpeg
install-tools:
	sudo apt-get install libav-tools

# Use this target when one file requires another.
# See "../../cue/split/Makefile" for an example.
require:
	@echo

.PHONY: all install-tools require clean
