DIRTXT = $(shell dir *.txt)

DIROUT.TXT = $(DIRTXT:.txt=.out.txt)

all: clean Makefile $(DIROUT.TXT)

%.out.txt: %.txt
	iconv -t UTF-8 $^ > $@
	echo $@ >> provide

clean:
	rm -f *.out.txt provide

install-tools:
	echo "No tools required"

.PHONY: all install-tools clean
