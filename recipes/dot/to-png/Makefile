DIRDOT = $(shell dir *.dot)

DIRPNG = $(DIRDOT:.dot=.png)

all: clean Makefile $(DIRPNG)

%.png: %.dot
	dot -Tpng "$^" > "$@"
	echo $@ >> provide

clean:
	rm -f *.png provide

install-tools:
	sudo apt-get install graphviz

.PHONY: all install-tools clean
