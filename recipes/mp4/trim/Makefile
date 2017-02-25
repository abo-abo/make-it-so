# ——— parameters ———————————————————————————————————————————————————————————————
from = 00:00:00
to = 00:00:23

# ——— implementation ———————————————————————————————————————————————————————————
DIRMP4 = $(shell find . -maxdepth 1 -name '*.mp4' -type f ! -name '*.out.mp4' | tr '\n' ' ')
DIROUT.MP4 = $(DIRMP4:.mp4=.out.mp4)

all: Makefile $(DIROUT.MP4)

%.out.mp4: %.mp4
	avconv -i "$^" -vcodec copy -acodec copy -ss $(from) -t $(to) "$@"
	echo "$@" >> provide

clean:
	rm -f *.out.mp4 provide

install-tools:
	sudo apt-get install libav-tools

.PHONY: all install-tools clean
