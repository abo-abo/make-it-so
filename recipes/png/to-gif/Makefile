DIRPNG = $(shell dir *.png)

DIRGIF = $(DIRPNG:.png=.gif)

all: clean Makefile $(DIRGIF)

%.gif: %.png
	convert $^ -channel Alpha -threshold 80% $@
	echo $@ >> provide

clean:
	rm -f *.gif provide

install-tools:
	echo "No tools required"

.PHONY: all install-tools clean
