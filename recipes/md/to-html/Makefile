DIRMD = $(shell dir *.md)
DIRHTML = $(DIRMD:.md=.html)

all: clean Makefile $(DIRHTML)

%.html: %.md
	pandoc -o "$@" "$^"
	echo "$@" >> provide

clean:
	rm -f *.html provide

install-tools:
	sudo apt-get install pandoc

.PHONY: all install-tools clean
