DIRTIFF = $(shell dir *.tiff)

DIRJPG = $(DIRTIFF:.tiff=.jpg)

all: clean Makefile $(DIRJPG)

%.jpg: %.tiff
	convert $^ $@
	echo $@ >> provide

clean:
	rm -f *.jpg provide

install-tools:
	echo "No tools required"

.PHONY: all install-tools clean
