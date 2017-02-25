DIRIPYNB = $(shell dir *.ipynb)

DIRMD = $(DIRIPYNB:.ipynb=.md)

all: clean Makefile $(DIRMD)

%.md: %.ipynb
	jupyter nbconvert --to markdown $^
	echo $@ >> provide

clean:
	rm -f *.md provide

install-tools:
	sudo pip install jupyter

.PHONY: all install-tools clean
