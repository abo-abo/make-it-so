DIROGV = $(shell dir *.ogv)
DIRMP4 = $(DIROGV:.ogv=.mp4)

all: clean Makefile $(DIRMP4)

%.mp4: %.ogv
	avconv -i $^ $@
	echo $@ >> provide

clean:
	rm -f *.mp4 provide

install-tools:
	sudo apt-get install avconv

.PHONY: all install-tools clean
