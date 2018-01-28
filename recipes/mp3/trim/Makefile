# ——— parameters ———————————————————————————————————————————————————————————————
from = 00:01:15

# ——— implementation ———————————————————————————————————————————————————————————
DIRMP3 = $(shell dir *.mp3)

DIROUT.MP3 = $(DIRMP3:.mp3=.out.mp3)

all: clean Makefile $(DIROUT.MP3)

%.out.mp3: %.mp3
	avconv -i $< -ss $(from) -acodec copy "$@"
	echo $@ >> provide

clean:
	rm -f *.out.mp3 provide

install-tools:
	sudo apt-get install avconv

.PHONY: all install-tools clean
