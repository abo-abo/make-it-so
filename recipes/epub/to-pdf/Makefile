DIREPUB = $(shell dir *.epub)

DIRPDF = $(DIREPUB:.epub=.pdf)

all: clean Makefile $(DIRPDF)

%.pdf: %.epub
	ebook-convert $^ $@
	echo $@ >> provide

clean:
	rm -f *.pdf provide

install-tools:
	sudo apt-get install calibre

.PHONY: all install-tools clean
