DIRAPE = $(shell dir *.ape)
DIRFLAC = $(DIRAPE:.ape=.flac)

all: clean Makefile $(DIRFLAC)

%.flac: %.ape
	avconv -i '$^' '$@'
	echo '$@' >> provide

clean:
	rm -f *.flac provide

install-tools:
	sudo apt-get install avconv

.PHONY: all install-tools clean
