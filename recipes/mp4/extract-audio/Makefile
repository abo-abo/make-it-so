DIRMP4 = $(shell dir *.mp4)

DIRAAC = $(DIRMP4:.mp4=.aac)

all: Makefile $(DIRAAC)

%.aac: %.mp4
	avconv -i "$^" "$@"
	echo "$@" >> provide

clean:
	rm -f *.aac provide

install-tools:
	sudo apt-get install libav-tools

.PHONY: all install-tools clean
