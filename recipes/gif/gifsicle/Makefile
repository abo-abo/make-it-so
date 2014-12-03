# delay between frames in hundredths of a second
delay = 60

# ______________________________________________________________________________
DIRGIF = $(shell ls *.gif | grep -v anime.gif)

all: anime.gif

anime.gif: Makefile $(DIRGIF)
	rm -f anime.gif
	gifsicle --delay=$(delay) --colors=256 --loop $(DIRGIF) > $@
	echo $@ >> provide

clean:
	rm -f anime.gif provide

install-tools:
	sudo apt-get install gifsicle

require:
	@echo

.PHONY: all install-tools require clean
