DIRMD = $(shell dir *.md)

DIRORG = $(DIRMD:.md=.org)

all: clean Makefile $(DIRORG)

%.org: %.md
	pandoc $^ -o $@
	echo $@ >> provide

clean:
	rm -f *.org provide

install-tools:
	sudo apt-get install pandoc

.PHONY: all install-tools clean
