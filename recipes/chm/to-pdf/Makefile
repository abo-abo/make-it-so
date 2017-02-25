DIRCHM = $(shell dir *.chm)

DIRPDF = $(DIRCHM:.chm=.pdf)

all: clean Makefile $(DIRPDF)

%.pdf: %.chm
	chm2pdf --continuous $^
	echo $@ >> provide

clean:
	rm -f *.pdf provide

install-tools:
	sudo apt-get install chm2pdf

.PHONY: all install-tools clean
