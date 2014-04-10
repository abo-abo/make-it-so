# ——— parameters ———————————————————————————————————————————————————————————————

# ——— implementation ———————————————————————————————————————————————————————————
DIRMD = $(shell dir *.md)
DIRHTML = $(DIRMD:.md=.html)

all: clean Makefile $(DIRHTML)

%.html: %.md
	pandoc -o "$@" "$^"
	echo "$@" >> provide

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
