# This is a template for the Makefile.
# Parameters should go in the upper half as:
#     width = 200
# and be referenced in the command as $(width)

delay = 60

# ______________________________________________________________________________

DIRGIF = $(shell dir *.gif)

all: clean Makefile anime.gif

anime.gif: $(DIRGIF)
	gifsicle --delay=$(delay) --colors=256 --loop $^ > $@
	echo $@ >> provide

clean:
	rm -f *.out.gif provide

# Insert the install command here.
# e.g. sudo apt-get install ffmpeg
install-tools:
	sudo apt-get install gifsicle

# Use this target when one file requires another.
# See "../../cue/split/Makefile" for an example.
require:
	@echo

.PHONY: all install-tools require clean
