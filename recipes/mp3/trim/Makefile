# ——— parameters ———————————————————————————————————————————————————————————————
from_to = -ss 00:00:10 -t 00:00:25
from_to = -ss 00:00:10
# ——— implementation ———————————————————————————————————————————————————————————
DIRMP3 = $(shell dir *.mp3)

DIROUT.MP3 = $(DIRMP3:.mp3=.out.mp3)

all: clean Makefile $(DIROUT.MP3)

%.out.mp3: %.mp3
	ffmpeg -i "$<" -acodec copy $(from_to) "$@"
	echo "$@" >> provide

clean:
	rm -f *.out.mp3 provide

install-tools:
	sudo apt-get install ffmpeg

.PHONY: all install-tools clean
