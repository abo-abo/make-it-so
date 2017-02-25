DIRPDF = $(shell dir *.pdf 2>/dev/null)

DIRTXT = $(DIRPDF:.pdf=.txt)

all: clean Makefile $(DIRTXT)

%.txt: %.pdf
	pdftotext $^
	echo $@ >> provide

clean:
	rm -f *.txt provide

install-tools:
	sudo apt-get install poppler-utils

.PHONY: all install-tools clean
